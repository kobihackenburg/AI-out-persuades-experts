# tab_followup_per_study.R
# SI Table: per-study effects immediately and at follow-up (Studies 1-3), in
# each study's re-surveyed conversations: each persuader class vs control,
# and AI minus pooled human persuaders.
# Reads RDS written by code/analysis/si_13_followup.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_followup_per_study.R ====")

CLASS_LABELS <- c(PERSUADER_CLASS_LABELS, ai_constrained = "Constrained AI")

fmt_cell <- function(estimate, lower_ci, upper_ci) {
  ifelse(is.na(estimate), "",
         sprintf("%+.2f [%+.2f, %+.2f]", estimate, lower_ci, upper_ci))
}

ps <- load_rds("followup_per_study") |>
  dplyr::mutate(
    wave  = ifelse(grepl("immediate$", spec), "Immediate", "Follow-up"),
    group = sub(", (immediate|follow-up)$", "", spec),
    group = sub(" vs control$", "", group),
    order = match(group, c(PERSUADER_CLASS_ORDER, "ai_constrained", "AI - human")),
    Persuader = ifelse(group == "AI - human", "AI minus all human persuaders", CLASS_LABELS[group]),
    panel = sprintf("%s (n = %s)", sub("study_", "Study ", study), format(n, big.mark = ",")),
    cell  = fmt_cell(estimate, lower_ci, upper_ci)
  )

wide <- ps |>
  dplyr::select(study, panel, order, Persuader, wave, cell) |>
  tidyr::pivot_wider(names_from = wave, values_from = cell) |>
  dplyr::arrange(study, order) |>
  dplyr::select(panel, Persuader, Immediate, `Follow-up`)

tbl <- gt::gt(wide, groupname_col = "panel") |>
  style_table(
    title    = gt::md("**Per-study effects immediately and at follow-up (Studies 1--3).**"),
    subtitle = paste0(
      "Effects vs control on the 0--100 attitude scale (pp [95% CI]) from the preregistered model fitted ",
      "separately to each study's re-surveyed conversations, with the immediate or the follow-up attitude ",
      "as outcome. n is the number of re-surveyed conversations."
    )
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_followup_per_study", TABLES_SI,
           label  = "tab:followup_per_study",
           layout = "auto",
           new    = TRUE)

message("  done.")
