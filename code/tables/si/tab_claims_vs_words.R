# tab_claims_vs_words.R
# SI Table: association of persuasive impact with the number of words and
# the number of claims in a conversation, entered separately and together,
# in fact-checked Studies 1-2 conversations. One row group per sample, one
# row per specification; cells are pp of attitude change per doubling.
# Reads RDS written by code/analysis/si_17_words_claims.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_claims_vs_words.R ====")

fmt_cell <- function(estimate, lower_ci, upper_ci) {
  ifelse(is.na(estimate), "",
         sprintf("%+.2f [%+.2f, %+.2f]", estimate, lower_ci, upper_ci))
}

SAMPLE_ORDER <- c("ai_vs_human", "ai_only", "human_only",
                  "all_incl_constrained", "constrained_only")
SAMPLE_LABELS <- c(
  ai_vs_human          = "AI and human persuaders",
  ai_only              = "AI persuaders only",
  human_only           = "Human persuaders only",
  all_incl_constrained = "AI and human persuaders, including Constrained AI",
  constrained_only     = "Constrained AI only"
)
SPEC_LABELS <- c(words = "Words only", claims = "Claims only", both = "Words and claims")

desc <- load_rds("claims_words_desc") |>
  dplyr::mutate(group = sprintf("%s (n = %s; r = %.2f)", SAMPLE_LABELS[sample],
                                format(n, big.mark = ","), r_log_words_log_claims))

wide <- load_rds("claims_words_fits") |>
  dplyr::filter(term %in% c("log_words", "log_claims")) |>
  dplyr::mutate(cell = fmt_cell(pp_per_doubling, pp_per_doubling_low, pp_per_doubling_high)) |>
  dplyr::select(sample, spec, term, cell) |>
  tidyr::pivot_wider(names_from = term, values_from = cell, values_fill = "") |>
  dplyr::left_join(dplyr::select(desc, sample, group), by = "sample") |>
  dplyr::arrange(match(sample, SAMPLE_ORDER), match(spec, names(SPEC_LABELS))) |>
  dplyr::transmute(
    group,
    Model = unname(SPEC_LABELS[spec]),
    `Log words` = log_words,
    `Log claims` = log_claims
  )

subtitle <- paste0(
  "Coefficients (pp of attitude change per doubling [95% CI]) from LMMs of post-treatment ",
  "attitude on pre-treatment attitude, issue, attempt number, log number of words written by ",
  "the persuader and/or log number of fact-checkable claims, a human-persuader indicator ",
  "(samples with both persuader types), and random intercepts for persuader and persuadee. ",
  "Fact-checked Studies 1-2 conversations with at least one claim. Unless stated, Constrained ",
  "AI is excluded. r is the correlation between log words and log claims in each sample. ",
  "Neither quantity was randomized, so the coefficients are associations."
)

tbl <- gt::gt(wide, groupname_col = "group") |>
  style_table(
    title    = gt::md("**Persuasive impact, number of words and number of claims.**"),
    subtitle = subtitle
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_claims_vs_words", TABLES_SI,
           label  = "tab:claims_vs_words",
           layout = "auto",
           new    = TRUE)

message("  done.")
