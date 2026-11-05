# tab_length_reply_time.R
# SI Table: associations of persuader message length and persuadee reply time
# and reply length with attitude change, holding the number of claims
# constant (fact-checked Studies 1-2 conversations, unconstrained AI and human
# persuaders). One row group per sample (AI and human, AI only, human only);
# columns are the claims-only model and the model adding the three length and
# timing terms. All measures are post-treatment, so these are associations.
# Reads RDS written by code/analysis/si_15_length_timing.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_length_reply_time.R ====")

fmt_cell <- function(estimate, lower_ci, upper_ci) {
  sprintf("%+.2f [%+.2f, %+.2f]", estimate, lower_ci, upper_ci)
}
fmt_p <- function(p) ifelse(p < 0.001, "p < .001", sprintf("p = %.3f", p))

TERM_LABELS <- c(
  log_claims      = "Fact-checkable claims",
  log_msg_len     = "Persuader message length",
  log_reply_s     = "Persuadee reply time",
  log_reply_words = "Persuadee reply length",
  ai_minus_human  = "AI minus human"
)
SAMPLE_LABELS <- c(ai_vs_human = "AI and human", ai_only = "AI only", human_only = "Human only")
SPEC_COLS <- c(claims = "Claims only", claims_timing = "Claims, length and timing")

coefs <- load_rds("length_reply_coefs")
lrt   <- load_rds("length_reply_lrt")

# is_human is human minus AI; report AI minus human.
coef_rows <- coefs |>
  dplyr::mutate(
    flip      = term == "is_human",
    term      = dplyr::if_else(flip, "ai_minus_human", term),
    cell      = dplyr::if_else(flip, fmt_cell(-estimate, -conf.high, -conf.low),
                               fmt_cell(estimate, conf.low, conf.high))
  ) |>
  dplyr::select(sample, n, spec, term, cell)

fit_rows <- lrt |>
  dplyr::transmute(
    sample,
    `r2m:claims`        = sprintf("%.3f", r2m_claims),
    `r2m:claims_timing` = sprintf("%.3f", r2m_claims_timing),
    `lrt:claims`        = "",
    `lrt:claims_timing` = sprintf("%.1f, %s", chisq, fmt_p(p_value))
  ) |>
  tidyr::pivot_longer(-sample, names_to = c("term", "spec"), names_sep = ":",
                      values_to = "cell")

n_by_sample <- dplyr::distinct(coefs, sample, n)

tbl_df <- dplyr::bind_rows(dplyr::select(coef_rows, -n), fit_rows) |>
  dplyr::left_join(n_by_sample, by = "sample") |>
  tidyr::pivot_wider(names_from = spec, values_from = cell, values_fill = "") |>
  dplyr::mutate(
    block        = sprintf("%s (n = %s)", SAMPLE_LABELS[sample],
                           formatC(n, format = "d", big.mark = ",")),
    sample_order = match(sample, names(SAMPLE_LABELS)),
    term_order   = match(term, c(names(TERM_LABELS), "r2m", "lrt")),
    Term         = dplyr::case_when(
      term == "r2m" ~ "Marginal R-squared",
      term == "lrt" ~ "Likelihood-ratio test, chi-squared(3)",
      TRUE          ~ unname(TERM_LABELS[term])
    )
  ) |>
  dplyr::arrange(sample_order, term_order) |>
  dplyr::select(block, Term, dplyr::all_of(names(SPEC_COLS))) |>
  dplyr::rename(!!!setNames(names(SPEC_COLS), SPEC_COLS))

subtitle <- paste0(
  "Attitude change in pp per doubling of each measure (AI minus human in pp). LMMs of ",
  "post-treatment attitude on pre-treatment attitude, issue, attempt number and log2 claims ",
  "(Claims only), plus log2 mean persuader message length, log2 median persuadee reply time ",
  "and log2 (median persuadee reply words + 1) (Claims, length and timing), with an AI indicator ",
  "in the pooled sample and random intercepts for persuader and persuadee; 95% Wald CIs. ",
  "Reply time runs from a persuader message to the persuadee's reply and includes reading and ",
  "writing; in AI conversations it also includes AI generation time. The likelihood-ratio test ",
  "compares the two models (ML fits). Fact-checked Studies 1-2 conversations with unconstrained ",
  "AI or human persuaders; Constrained AI is excluded because its imposed delay falls inside the ",
  "measured reply time. These measures were not randomly assigned and were recorded during the ",
  "conversation, so the estimates are associations, not causal effects."
)

tbl <- gt::gt(tbl_df, groupname_col = "block") |>
  gt::cols_label(Term = "") |>
  style_table(
    title    = gt::md("**Message length, reply time and attitude change, holding the number of claims constant.**"),
    subtitle = subtitle
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_length_reply_time", TABLES_SI,
           label  = "tab:length_reply_time",
           layout = "auto",
           new    = TRUE)

message("  done.")
