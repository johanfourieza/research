# =============================================================================
# fig_citation_source.R
#
# Corrected "Where citations come from" figure + statistics, replacing the
# earlier within/cross-field decomposition that rested on OpenAlex's single
# top-concept (a measurement artefact; see verify_crossfield.R). Citers are
# now classified by DISCIPLINE using the re-queried primary_topic / level-0
# concept data cached in data/citing_field_data.rds.
#
# Outputs: figures/Fig6_CitationSource.{png,pdf}
# =============================================================================

suppressMessages({
  library(data.table); library(ggplot2); library(scales); library(patchwork)
})

SCRIPT_DIR <- tryCatch({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
}, error = function(e) getwd())
BASE_DIR   <- normalizePath(file.path(SCRIPT_DIR, ".."), winslash = "/")
DATA_DIR   <- file.path(BASE_DIR, "data")
FIGURE_DIR <- file.path(BASE_DIR, "figures")

LEAP_COLORS <- c(plum = "#5C2346", blue = "#3D8EB9", sage = "#6B8E5E",
                 gold = "#D4A03E", rose = "#A34466", teal = "#45808B",
                 earth = "#8B6B3D", mint = "#97C5B0")

theme_leap <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "sans") %+replace%
    theme(
      plot.title    = element_text(size = base_size + 1, face = "bold",
                                   margin = ggplot2::margin(b = 6), hjust = 0),
      plot.subtitle = element_text(size = base_size - 1, colour = "grey30",
                                   margin = ggplot2::margin(b = 8), hjust = 0),
      axis.title    = element_text(size = base_size - 1),
      panel.grid.minor = element_blank(),
      plot.margin   = ggplot2::margin(10, 14, 10, 10),
      legend.position = "none"
    )
}

# --- Data: link-level discipline + format -----------------------------------
cr <- readRDS(file.path(DATA_DIR, "network_citation_data.rds")); setDT(cr)
cr[, cs := gsub("https://openalex.org/", "", citing_oa_id)]
fd <- readRDS(file.path(DATA_DIR, "citing_field_data.rds")); setDT(fd)
lk <- merge(cr, fd, by.x = "cs", by.y = "citing_oa_id", all.x = TRUE)
N  <- nrow(lk)

# Discipline (mutually exclusive, by primary_topic), grouped for colour
lk[, disc := fifelse(pt_field == "Economics, Econometrics and Finance", "Economics",
              fifelse(pt_subfield == "History", "History",
              fifelse(pt_field == "Social Sciences", "Other social sciences",
              fifelse(pt_field == "Business, Management and Accounting", "Business & management",
              fifelse(pt_field == "Arts and Humanities", "Arts & humanities",
              fifelse(pt_field %in% c("Medicine","Biochemistry, Genetics and Molecular Biology",
                        "Agricultural and Biological Sciences","Health Professions","Nursing",
                        "Immunology and Microbiology","Neuroscience","Psychology",
                        "Pharmacology, Toxicology and Pharmaceutics","Dentistry","Veterinary"),
                        "Health & life sciences",
              fifelse(is.na(pt_field), "Unclassified", "Other sciences & engineering")))))))]

disc_lab <- c("Economics","History","Other social sciences","Business & management",
              "Arts & humanities","Health & life sciences","Other sciences & engineering")
dd <- lk[disc != "Unclassified", .(N = .N), by = disc]
dd[, pct := 100 * N / sum(N)]
dd[, group := fifelse(disc %in% c("Economics","History"), "Economic history & economics",
              fifelse(disc %in% c("Other social sciences","Business & management"),
                      "Adjacent social science", "Other fields"))]
dd[, disc := factor(disc, levels = rev(disc_lab))]

grp_cols <- c("Economic history & economics" = unname(LEAP_COLORS["plum"]),
              "Adjacent social science"        = unname(LEAP_COLORS["blue"]),
              "Other fields"                   = unname(LEAP_COLORS["gold"]))

pA <- ggplot(dd, aes(x = pct, y = disc, fill = group)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = sprintf("%.0f%%", pct)), hjust = -0.15, size = 3.4, colour = "grey20") +
  scale_fill_manual(values = grp_cols) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(x = "Share of all citations (%)", y = NULL,
       title = "a. By citing discipline") +
  theme_leap()

# Format (collapsed)
lk[, fmt := fifelse(type == "article", "Journal article",
             fifelse(type %in% c("book-chapter","book"), "Book or chapter",
             fifelse(type %in% c("preprint","report"), "Preprint / working paper",
             fifelse(type == "dissertation", "Dissertation", "Other"))))]
ff <- lk[!is.na(type), .(N = .N), by = fmt]
ff[, pct := 100 * N / sum(N)]
fmt_lev <- c("Journal article","Book or chapter","Preprint / working paper","Dissertation","Other")
ff[, fmt := factor(fmt, levels = rev(fmt_lev))]
ff[, grp := fifelse(fmt == "Journal article", "Journal article", "Other formats")]

pB <- ggplot(ff, aes(x = pct, y = fmt, fill = grp)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = sprintf("%.0f%%", pct)), hjust = -0.15, size = 3.4, colour = "grey20") +
  scale_fill_manual(values = c("Journal article" = unname(LEAP_COLORS["sage"]),
                               "Other formats"   = unname(LEAP_COLORS["earth"]))) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(x = "Share of all citations (%)", y = NULL,
       title = "b. By document type") +
  theme_leap()

fig <- pA / pB +
  plot_annotation(
    title = "Where citations to economic history papers come from",
    subtitle = sprintf("%s citation links from %s distinct citing works, classified by OpenAlex discipline",
                       format(N, big.mark = ","), format(uniqueN(lk$cs), big.mark = ",")),
    theme = theme(plot.title = element_text(face = "bold", size = 13, hjust = 0),
                  plot.subtitle = element_text(colour = "grey30", size = 10, hjust = 0)))

ggsave(file.path(FIGURE_DIR, "Fig6_CitationSource.png"), fig, width = 7.5, height = 6.2, dpi = 600)
ggsave(file.path(FIGURE_DIR, "Fig6_CitationSource.pdf"), fig, width = 7.5, height = 6.2)
cat("Saved Fig6_CitationSource.{png,pdf}\n\n")

cat("Discipline shares:\n"); print(dd[order(-pct), .(disc, pct = round(pct,1))])
cat("\nFormat shares:\n");   print(ff[order(-pct), .(fmt, pct = round(pct,1))])
