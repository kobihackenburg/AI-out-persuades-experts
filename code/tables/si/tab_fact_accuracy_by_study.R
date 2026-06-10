# tab_fact_accuracy_by_study.R
# SI Table: per-study factual-accuracy summary. For each study, the number of
# fact-checked conversations, the total number of extracted claims, the mean
# claims per conversation, the proportion of claims rated accurate
# (veracity > 50/100), and the mean veracity score.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_fact_accuracy_by_study.R ====")

fas <- load_rds("fact_accuracy_by_study")

tbl_df <- fas |>
  dplyr::arrange(study) |>
  dplyr::transmute(
    Study                = study_label,
    `Conversations`      = format(n_conv, big.mark = ","),
    `Claims checked`     = format(n_claims, big.mark = ","),
    `Claims / conv`      = sprintf("%.1f", mean_claims_per_conv),
    `% accurate`         = sprintf("%.1f", 100 * prop_accurate),
    `Mean veracity`      = sprintf("%.1f", mean_veracity)
  )

tbl <- gt::gt(tbl_df) |>
  style_table(
    title    = gt::md("**Factual accuracy of persuader claims, by study.**"),
    subtitle = gt::md("A claim is counted as accurate if its web-search veracity score exceeds 50 on the 0-100 scale. Veracity is scored with the human-validated fact-checking pipeline of Hackenburg et al. (2025).")
  )

save_table(tbl, "tab_fact_accuracy_by_study", TABLES_SI,
           label  = "tab:fact_accuracy_by_study",
           layout = "auto")

tex_path <- file.path(TABLES_SI, "tab_fact_accuracy_by_study.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
            "\\\\begin{tabular}{", txt)
txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt)
txt <- gsub("\\\\textbf\\{Claims checked\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Claims \\\\\\\\ checked}}}", txt)
txt <- gsub("\\\\textbf\\{Claims / conv\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Claims / \\\\\\\\ conv}}}", txt)
txt <- gsub("\\\\textbf\\{\\\\% accurate\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{\\\\% \\\\\\\\ accurate}}}", txt)
txt <- gsub("\\\\textbf\\{Mean veracity\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Mean \\\\\\\\ veracity}}}", txt)
txt <- gsub("\\\\begin\\{table\\}\\[!t\\]", "\\\\begin{table}[H]", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
