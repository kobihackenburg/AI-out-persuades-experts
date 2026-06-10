# tab_sample_sizes.R
# SI Table: per-(study x condition) sample sizes at three pipeline stages:
#   assigned -> successfully matched -> final (post-attitude non-missing).
# Study 4's "final" column counts persuadees with a non-missing
# donation_amount. Filenames / labels are semantic so re-ordering tables
# in SI.tex doesn't require renaming anything; the displayed "Table SX"
# number is auto-assigned by LaTeX at compile time.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_sample_sizes.R ====")

studies <- load_studies(c("study_1", "study_2", "study_3", "study_4"))

per_arm <- purrr::imap_dfr(studies, function(df, study_id) {
  outcome_col <- if (study_id == "study_4") "donation_amount" else "post_attitude"
  df |>
    dplyr::filter(!is.na(persuader_class)) |>
    dplyr::group_by(persuader_class, ai_constrained) |>
    dplyr::summarise(
      n_assigned    = dplyr::n(),
      n_matched     = sum(dplyr::coalesce(successfully_matched, FALSE)),
      n_cc          = sum(dplyr::coalesce(successfully_matched, FALSE) &
                          !dplyr::coalesce(attrited, FALSE) &
                          !is.na(.data[[outcome_col]])),
      .groups = "drop"
    ) |>
    dplyr::mutate(study = study_id) |>
    dplyr::relocate(study)
})

CONDITION_LABEL <- function(cls, constrained) {
  base <- unname(PERSUADER_CLASS_LABELS[as.character(cls)])
  base <- ifelse(is.na(base), as.character(cls), base)
  ifelse(cls == "ai" & isTRUE(constrained), "AI (Constrained)", base)
}

tbl_df <- per_arm |>
  dplyr::mutate(
    Condition = mapply(CONDITION_LABEL, persuader_class, ai_constrained),
    Study     = sub("study_", "Study ", study),
    `Assigned`        = format(n_assigned, big.mark = ","),
    `Matched`         = format(n_matched,  big.mark = ","),
    `Final`           = format(n_cc,       big.mark = ","),
    `Attrition %`     = sprintf("%.1f",
                                 100 * (n_matched - n_cc) /
                                 pmax(n_matched, 1L))
  ) |>
  dplyr::arrange(Study, match(persuader_class, PERSUADER_CLASS_ORDER),
                 dplyr::desc(ai_constrained)) |>
  dplyr::select(Study, Condition, Assigned, Matched, Final, `Attrition %`)

tbl <- gt::gt(tbl_df, groupname_col = "Study") |>
  style_table(
    title    = gt::md("**Per-condition sample sizes by study.**"),
    subtitle = "Assigned persuadees are those allocated to the condition by the platform's randomiser. Matched persuadees are those who were successfully paired with a partner in real time and exposed to treatment. Final persuadees are matched persuadees with a non-missing primary outcome (post-treatment attitude in Studies 1-3; donation amount in Study 4). Assigned exceeds Matched because persuadees who entered matchmaking but were not paired (e.g., no partner came online in their slot) had not yet seen any treatment and remained eligible to re-enter the study."
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_sample_sizes", TABLES_SI,
           label  = "tab:sample_sizes",
           layout = "auto")

tex_path <- file.path(TABLES_SI, "tab_sample_sizes.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
            "\\\\begin{tabular}{", txt)
txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt)
txt <- gsub("\\\\begin\\{table\\}\\[!t\\]", "\\\\begin{table}[H]", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
