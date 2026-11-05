# tab_constrained_models.R
# SI Table: Constrained AI effects by model (Study 2), alongside each model's
# realized words per message and claims per conversation, and the pairwise
# contrasts between models. All three models shared the same delay sampler
# and length calibration, so the contrasts compare information content with
# delay and length held fixed.
# Reads RDS written by code/analysis/si_15_constrained_models.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_constrained_models.R ====")

fmt_cell <- function(estimate, lower_ci, upper_ci) {
  sprintf("%+.2f [%+.2f, %+.2f]", estimate, lower_ci, upper_ci)
}
fmt_p <- function(p) ifelse(p < 0.001, "<.001", sprintf("%.3f", p))

MODEL_LABELS <- c(
  constrained_gemini  = "Gemini 2.5 Pro",
  constrained_chatgpt = "ChatGPT-4o",
  constrained_claude  = "Claude Opus 4.1"
)
CONTRAST_LABELS <- c(
  claude_minus_gemini  = "Claude - Gemini",
  chatgpt_minus_gemini = "ChatGPT - Gemini",
  claude_minus_chatgpt = "Claude - ChatGPT"
)

EFFECT_GROUP   <- "Effect vs control (pp)"
CONTRAST_GROUP <- "Pairwise contrast (pp)"

desc <- load_rds("constrained_models_desc")
te   <- load_rds("constrained_models_te")
con  <- load_rds("constrained_models_contrasts")

effect_rows <- desc |>
  dplyr::inner_join(te, by = "group") |>
  dplyr::arrange(claims_per_conv) |>
  dplyr::transmute(
    block               = EFFECT_GROUP,
    Row                 = unname(MODEL_LABELS[group]),
    n                   = formatC(n, format = "d", big.mark = ","),
    `Words/msg`         = sprintf("%.1f", words_per_msg),
    `Claims/conv`       = sprintf("%.1f", claims_per_conv),
    `Estimate [95% CI]` = fmt_cell(estimate, lower_ci, upper_ci),
    p                   = fmt_p(p_value)
  )

contrast_rows <- con |>
  dplyr::mutate(contrast = as.character(contrast)) |>
  dplyr::arrange(match(contrast, names(CONTRAST_LABELS))) |>
  dplyr::transmute(
    block               = CONTRAST_GROUP,
    Row                 = unname(CONTRAST_LABELS[contrast]),
    n                   = "",
    `Words/msg`         = "",
    `Claims/conv`       = "",
    `Estimate [95% CI]` = fmt_cell(estimate, asymp.LCL, asymp.UCL),
    p                   = fmt_p(p.value)
  )

tbl_df <- dplyr::bind_rows(effect_rows, contrast_rows)

subtitle <- paste0(
  "All three Constrained AI models used the same response-delay sampler (mean 92 s) and ",
  "the same message-length calibration. ",
  "Estimates are from the preregistered pooled LMM on Studies 1-2 ",
  "(post ~ pre + issue + group + attempt + (1|persuader) + (1|persuadee)), with ",
  "Constrained AI and unconstrained AI each split by model; 95% Wald CIs, unadjusted p. ",
  "n is the number of persuadee conversations; words/msg is total persuader words over ",
  "total persuader messages; claims/conv is the mean number of ",
  "fact-checkable claims in fact-checked conversations."
)

tbl <- gt::gt(tbl_df, groupname_col = "block") |>
  gt::cols_label(Row = "") |>
  style_table(
    title    = gt::md("**Constrained AI effects by model (Study 2).**"),
    subtitle = subtitle
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_constrained_models", TABLES_SI,
           label  = "tab:constrained_models",
           layout = "auto",
           new    = TRUE)

message("  done.")
