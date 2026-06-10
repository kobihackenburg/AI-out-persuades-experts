# tab_fact_accuracy_by_condition.R
# SI Table: per-condition factual accuracy backing the accuracy-vs-impact
# figure. For each condition plotted in the S1-S3 facts panel (AI split by
# model x study, each human class, and the constrained-AI arms): the number of
# extracted claims, the proportion rated accurate (veracity > 50/100), the
# mean veracity, and the same arm's LMM persuasive-impact estimate vs control.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_fact_accuracy_by_condition.R ====")

fac <- load_rds("fact_accuracy_by_condition")

tbl_df <- fac |>
  dplyr::filter(!is.na(prop_accurate)) |>
  dplyr::arrange(dplyr::desc(prop_accurate)) |>
  dplyr::transmute(
    Condition       = condition,
    Type            = type,
    Study           = study,
    `Claims`        = format(n_claims, big.mark = ","),
    `% accurate`    = sprintf("%.1f", 100 * prop_accurate),
    `Mean veracity` = sprintf("%.1f", mean_veracity),
    `Estimate (pp)` = sprintf("%+.2f", estimate),
    `95% CI`        = sprintf("[%+.2f, %+.2f]", lower_ci, upper_ci)
  )

tbl <- gt::gt(tbl_df, groupname_col = "Type") |>
  style_table(
    title    = gt::md("**Per-condition factual accuracy vs. persuasive impact.**"),
    subtitle = gt::md("A claim is counted as accurate if its web-search veracity score exceeds 50 on the 0-100 scale; persuasive impact is the same arm's LMM estimate vs. control.")
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_fact_accuracy_by_condition", TABLES_SI,
           label  = "tab:fact_accuracy_by_condition",
           layout = "landscape")

tex_path <- file.path(TABLES_SI, "tab_fact_accuracy_by_condition.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\textbf\\{\\\\% accurate\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{\\\\% \\\\\\\\ accurate}}}", txt)
txt <- gsub("\\\\textbf\\{Mean veracity\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Mean \\\\\\\\ veracity}}}", txt)
txt <- gsub("\\\\textbf\\{Estimate \\(pp\\)\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Estimate \\\\\\\\ (pp)}}}", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
