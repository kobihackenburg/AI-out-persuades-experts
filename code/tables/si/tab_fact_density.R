# tab_fact_density.R
# SI Table: per-condition mean number of fact-checkable claims per
# conversation, alongside the LMM persuasive-impact estimate for the
# same arm. Underpins Fig. 3b.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_fact_density.R ====")

fc <- load_rds("canvasser_facts_vs_te")

tbl_df <- fc |>
  dplyr::arrange(dplyr::desc(mean_fact_claims)) |>
  dplyr::transmute(
    Condition        = condition,
    Type             = type,
    Study            = study,
    `n conv`         = format(n_conv, big.mark = ","),
    `Facts / conv`   = sprintf("%.1f", mean_fact_claims),
    `Estimate (pp)`  = sprintf("%+.2f", estimate),
    `95% CI`         = sprintf("[%+.2f, %+.2f]", lower_ci, upper_ci)
  )

tbl <- gt::gt(tbl_df, groupname_col = "Type") |>
  style_table(
    title    = gt::md("**Per-condition mean fact-checkable claims per conversation vs. persuasive impact.**"),
    subtitle = "Means over fact-checked conversations; persuasive impact is the same arm's LMM estimate vs. control (R^2 = 0.81 overall)."
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_fact_density", TABLES_SI,
           label  = "tab:fact_density",
           layout = "auto")

tex_path <- file.path(TABLES_SI, "tab_fact_density.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
            "\\\\begin{tabular}{", txt)
txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt)
txt <- gsub("\\\\textbf\\{n conv\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{n \\\\\\\\ conv}}}", txt)
txt <- gsub("\\\\textbf\\{Facts / conv\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Facts / \\\\\\\\ conv}}}", txt)
txt <- gsub("\\\\textbf\\{Estimate \\(pp\\)\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Estimate \\\\\\\\ (pp)}}}", txt)
txt <- gsub("\\\\begin\\{table\\}\\[!t\\]", "\\\\begin{table}[H]", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
