# Date-only observations: events are at integer days; completed-match histories
# change half-way to the next day. With (start, stop] survival intervals this
# excludes same-day results and includes them in every next-day risk set.
history_update <- function(completed, entry) as.numeric(completed - entry) + .5

# Aalen-Johansen incidence for many cluster multiplicity vectors at once.
# Each column of weights is one resample, in the order of groups.
cif_weighted <- function(d, groups, weights) {
  counts <- function(rows) tabulate(match(d$entry_cluster[rows], groups), length(groups))
  populated <- as.vector(crossprod(counts(rep(TRUE, nrow(d))), weights)) > 0
  survival <- rep(1, ncol(weights)); incidence <- rep(0, ncol(weights))
  for (tm in sort(unique(d$follow[d$status > 0]))) {
    n <- as.vector(crossprod(counts(d$follow >= tm), weights))
    n[n == 0] <- Inf
    d1 <- as.vector(crossprod(counts(d$follow == tm & d$status == 1), weights))
    d2 <- as.vector(crossprod(counts(d$follow == tm & d$status == 2), weights))
    incidence <- incidence + survival * d1 / n
    survival <- survival * (1 - (d1 + d2) / n)
  }
  incidence[!populated] <- NA_real_
  incidence
}

landmark_difference_interval <- function(d, exact = FALSE, draws = 1000000L, seed = 2412L) {
  groups <- sort(unique(d$entry_cluster)); G <- length(groups)
  a <- d[d$role == 'All-round role', ]; b <- d[d$role == 'Batting role', ]
  delta <- function(w) cif_weighted(a, groups, w) - cif_weighted(b, groups, w)
  estimate <- delta(matrix(1, G, 1))
  if (exact) {
    stopifnot(G <= 6L)
    allocations <- as.matrix(expand.grid(rep(list(0:G), G)))
    allocations <- allocations[rowSums(allocations) == G, , drop = FALSE]
    boot <- delta(t(allocations))
    probability <- apply(allocations, 1, dmultinom, prob = rep(1 / G, G))
    valid <- is.finite(boot); valid_mass <- sum(probability[valid])
    stopifnot(valid_mass > 0)
    ord <- order(boot[valid]); values <- boot[valid][ord]
    cumulative <- cumsum(probability[valid][ord]) / valid_mass
    limits <- vapply(c(.025, .975), function(p) values[which(cumulative >= p)[1]], numeric(1))
    method <- 'Exact multinomial entry-team bootstrap distribution'
  } else {
    # An independent seed makes this result insensitive to other bootstrap calls.
    set.seed(seed)
    boot <- numeric(draws)
    for (first in seq.int(1L, draws, by = 50000L)) {
      idx <- first:min(first + 49999L, draws)
      boot[idx] <- delta(rmultinom(length(idx), G, rep(1 / G, G)))
    }
    valid <- is.finite(boot); valid_mass <- mean(valid)
    stopifnot(any(valid))
    limits <- quantile(boot[valid], c(.025, .975), names = FALSE)
    method <- 'Monte Carlo multinomial entry-team bootstrap distribution'
  }
  data.frame(difference = estimate, lower = limits[1], upper = limits[2],
             clusters = G, method = method, evaluated_resamples = length(boot),
             valid_resamples = sum(valid), valid_probability = valid_mass,
             seed = if (exact) NA_integer_ else seed, margin = .05)
}
