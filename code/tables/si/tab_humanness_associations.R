# tab_humanness_associations.R
# SI Table: associations of perceived humanness with attitude change,
# Studies 1-3, unconstrained AI vs all human classes. The rating was
# collected after the conversation, so every row except the unconditioned
# AI minus human difference conditions on a post-treatment measure and is an
# association, not a causal effect.
#   Panel 1 - attitude change per 10 rating points, within AI and within
#             human conversations (and pooled)
#   Panel 2 - AI minus human difference in attitude change: unconditioned,
#             adjusting for the rating, and within four bands of the rating
#             (AI-by-band likelihood-ratio test in the caption)
# Reads RDS written by code/analysis/si_17_humanlike.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_humanness_associations.R ====")

fmt_cell <- function(estimate, lower_ci, upper_ci) {
  sprintf("%+.2f [%+.2f, %+.2f]", estimate, lower_ci, upper_ci)
}
fmt_n <- function(n) ifelse(is.na(n), "", formatC(n, format = "d", big.mark = ","))

coefs <- load_rds("humanness_coefs")
bands <- load_rds("humanness_bands")
lrt   <- load_rds("humanness_band_interaction")
sep   <- load_rds("humanness_separation")
n_ai <- sep$n[sep$is_ai == 1L]
n_hu <- sep$n[sep$is_ai == 0L]

coef_of <- function(label) dplyr::filter(coefs, model == label)

SLOPE_GROUP <- "Attitude change per 10 points of perceived humanness (pp)"
DIFF_GROUP  <- "AI minus human difference in attitude change (pp)"

slope_rows <- tibble::tibble(
  group = SLOPE_GROUP,
  Row   = c("AI conversations", "Human conversations", "All conversations (pooled slope)"),
  label = c("perceived humanness per 10 pts, AI convs",
            "perceived humanness per 10 pts, human convs",
            "perceived humanness per 10 pts (pooled)"),
  n_ai  = c(n_ai, NA, n_ai),
  n_hu  = c(NA, n_hu, n_hu)
)
diff_rows <- tibble::tibble(
  group = DIFF_GROUP,
  Row   = c("All conversations, not conditioned on the rating",
            "All conversations, adjusting for the rating"),
  label = c("AI - human", "AI - human, adj. perceived humanness"),
  n_ai  = n_ai,
  n_hu  = n_hu
)

coef_rows <- dplyr::bind_rows(slope_rows, diff_rows) |>
  dplyr::mutate(cell = purrr::map_chr(label, function(l) {
    r <- coef_of(l)
    fmt_cell(r$estimate, r$lower_ci, r$upper_ci)
  }))

band_rows <- bands |>
  dplyr::transmute(
    group = DIFF_GROUP,
    Row   = sprintf("Rated %s", sub("-", "\u2013", as.character(band))),
    n_ai  = n_ai1,
    n_hu  = n_ai0,
    cell  = fmt_cell(estimate, lower_ci, upper_ci)
  )

tbl_df <- dplyr::bind_rows(dplyr::select(coef_rows, -label), band_rows) |>
  dplyr::transmute(
    group, Row,
    `n AI`              = fmt_n(n_ai),
    `n human`           = fmt_n(n_hu),
    `Estimate [95% CI]` = cell
  )

subtitle <- sprintf(
  paste0("The rating was collected after the conversation, so all rows except the first ",
         "AI minus human row condition on a post-treatment measure: they describe associations, ",
         "not causal effects, and may themselves reflect the conversation's effect. ",
         "LMMs of post-treatment attitude on pre-treatment attitude, issue, study, attempt number ",
         "and random intercepts for persuader and persuadee, plus: an AI indicator and the rating ",
         "(per 10 points) interacted with persuader type (first panel; pooled slope without the ",
         "interaction); an AI indicator alone or with the rating (first two rows of the second panel); ",
         "an AI indicator interacted with rating band (band rows). The AI-by-band interaction is not ",
         "significant (likelihood-ratio test, chi-squared(%d) = %.2f, p = %.2f). Studies 1-3; AI ",
         "excludes Constrained AI; Human pools all human persuader classes."),
  as.integer(lrt$df), lrt$chisq, lrt$p_value
)

tbl <- gt::gt(tbl_df, groupname_col = "group") |>
  gt::cols_label(Row = "") |>
  style_table(
    title    = gt::md("**Associations of perceived humanness with attitude change (conditioning on a post-treatment rating).**"),
    subtitle = subtitle
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_humanness_associations", TABLES_SI,
           label  = "tab:humanness_associations",
           layout = "auto",
           new    = TRUE)

message("  done.")
