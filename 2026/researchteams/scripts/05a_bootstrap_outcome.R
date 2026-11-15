# Wild cluster bootstrap t-statistics for one outcome. Called by
# 05_romano_wolf.R, once per outcome, each time in a new R process.
#
# Usage   Rscript scripts/05a_bootstrap_outcome.R <outcome> <draws>
# Writes  results/bootstrap_t/<outcome>.csv, with one row per draw and one
#         column per tested indicator. The row with draw = 0 holds the observed
#         t-statistics.
#
# For each tested indicator (the writing flag `marker`, then `disclosed`):
#   1. Restricted model: Eq. (3) without the tested indicator, so that its
#      coefficient is zero under the null.
#   2. In each draw, every researcher's restricted residuals are multiplied by
#      +1 or -1. The pseudo-outcome is restricted fitted value + signed residual.
#   3. The full model is fitted to the pseudo-outcome, and the t-statistic of
#      the tested coefficient is stored.
#
# Step 3 does not call feols() for every draw. After the fixed effects are
# removed from the outcome and the regressors, the coefficients are an ordinary
# weighted least-squares solution, and the two-way clustered variance is a sum
# of three cross-products. The script computes both directly and first checks
# that this reproduces the feols() covariance matrix. The result is the same as
# refitting; it is faster, and it avoids crashes that repeated feols() calls
# caused in fixest 0.13.2 on Windows.

source("scripts/00_functions.R")

args  <- commandArgs(trailingOnly = TRUE)
y     <- args[1]
draws <- as.integer(args[2])

papers <- read_papers()
links  <- read_links()
rows   <- estimation_rows(papers, links, y)
w      <- rows$weight

# ---- Full model, keeping the variables with the fixed effects removed --------
full <- feols(as.formula(paste(y, "~", rhs_main, "| researcher + source_quarter + field")),
              data = rows, weights = ~weight, vcov = ~researcher + uid,
              demeaned = TRUE, notes = FALSE)
stopifnot(nobs(full) == nrow(rows))
X  <- full$X_demeaned                      # regressors, fixed effects removed
yd <- full$y_demeaned                      # outcome, fixed effects removed
bread <- solve(crossprod(X, w * X))        # (X'WX)^-1
influence <- X %*% bread

# fixest's finite-sample correction for two-way clustering.
V_fixest <- vcov(full, attr = TRUE)
N <- nrow(rows); G <- attr(V_fixest, "G"); K <- attr(V_fixest, "df.K")
correction <- (N - 1) / (N - K) * G / (G - 1)
researcher_id <- as.integer(factor(rows$researcher))
paper_id      <- as.integer(factor(rows$uid))

# Two-way clustered variance: researcher clusters + paper clusters - their
# intersection. Each researcher-paper pair is one row, so the intersection term
# is the row-level cross-product.
two_way_vcov <- function(residual) {
  A <- influence * (w * residual)
  R <- rowsum(A, researcher_id, reorder = FALSE)
  P <- rowsum(A, paper_id, reorder = FALSE)
  V <- correction * (crossprod(R) + crossprod(P) - crossprod(A))
  eig <- eigen(V, symmetric = TRUE)        # fixest's repair if V is not positive definite
  if (any(eig$values < 1e-12)) {
    V <- tcrossprod(eig$vectors %*% diag(pmax(eig$values, 1e-12), nrow(V)), eig$vectors)
  }
  V
}
stopifnot(max(abs(two_way_vcov(resid(full)) - V_fixest)) < 1e-10)

# ---- Rademacher signs: one per researcher and draw ---------------------------
# The matrix is generated for all researchers in the data and all 1,999 draws,
# whatever the outcome and the number of draws requested. A researcher therefore
# has the same sign in a given draw in all 20 tests.
researchers <- sort(unique(links$researcher), method = "radix")
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(2026110201L)
W <- matrix(sample(c(-1L, 1L), length(researchers) * 1999L, replace = TRUE),
            nrow = length(researchers), ncol = 1999L)
index <- match(rows$researcher, researchers)
fixed_effects <- rows[, c("researcher", "source_quarter", "field")]

# ---- Bootstrap ------------------------------------------------------------------
targets <- c("marker", "disclosed")
t_star  <- matrix(NA_real_, draws, 2, dimnames = list(NULL, targets))

for (target in targets) {
  # Restricted model, by least squares on the demeaned variables.
  Xr <- X[, setdiff(colnames(X), target), drop = FALSE]
  restricted_residual <- yd - Xr %*% solve(crossprod(Xr, w * Xr), crossprod(Xr, w * yd))
  restricted_fitted   <- rows[[y]] - restricted_residual
  k <- match(target, colnames(X))

  for (first in seq(1L, draws, by = 50L)) {          # 50 draws at a time
    batch <- first:min(first + 49L, draws)
    Y <- as.numeric(restricted_fitted) +
      as.numeric(restricted_residual) * W[index, batch, drop = FALSE]
    Yd <- fixest::demean(Y, f = fixed_effects, weights = w, iter = 10000,
                         tol = 1e-6, nthreads = 1, notes = FALSE)
    beta <- bread %*% crossprod(X, w * Yd)
    U <- Yd - X %*% beta
    for (b in seq_along(batch)) {
      t_star[batch[b], target] <- beta[k, b] / sqrt(two_way_vcov(U[, b])[k, k])
    }
  }
}
stopifnot(all(is.finite(t_star)))

observed <- coeftable(full)[targets, 3]
dir.create("results/bootstrap_t", recursive = TRUE, showWarnings = FALSE)
write_csv(data.frame(draw = 0:draws, rbind(observed, t_star), row.names = NULL),
          file.path("results/bootstrap_t", paste0(y, ".csv")))
