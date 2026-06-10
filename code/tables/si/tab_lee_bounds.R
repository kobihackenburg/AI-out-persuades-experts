### tab_lee_bounds.R
# SI Table: Lee (2009) trimming bounds on the headline AI-vs-human
# contrasts in the two studies with detected differential attrition
# (Studies 1 and 2). Reads output/results/lee_bounds.rds built by
# code/analysis/si_04_attrition_sensitivity.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_lee_bounds.R ====")

lb <- load_rds("lee_bounds")

tbl_df <- lb |>
  dplyr::transmute(
    Study              = sub("study_", "Study ", study),
    Contrast           = contrast,
    `Trim q`           = sprintf("%.3f", trim_q),
    `Naive est. (95% CI)` = sprintf("%+.2f [%+.2f, %+.2f]", naive_est, naive_lo, naive_hi),
    `Lee lower`        = sprintf("%+.2f", lee_lower),
    `Lee upper`        = sprintf("%+.2f", lee_upper)
  )

tbl <- gt::gt(tbl_df, groupname_col = "Study") |>
  style_table(
    title    = gt::md("**Lee-bound sensitivity to differential attrition (Studies 1 and 2).**"),
    subtitle = "For each contrast we residualise post-attitude on pre-attitude and bound the AI-minus-human difference by trimming the lower-attrition arm by the response-rate gap (Lee 2009). Trim q is the per-contrast share trimmed from each tail of the lower-attrition arm. All bounds remain strictly positive, indicating the headline AI advantage cannot be explained by selective attrition."
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_lee_bounds", TABLES_SI,
           label  = "tab:lee_bounds",
           layout = "auto")

tex_path <- file.path(TABLES_SI, "tab_lee_bounds.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
            "\\\\begin{tabular}{", txt)
txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt)
txt <- gsub("\\\\textbf\\{Trim q\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Trim \\\\\\\\ q}}}", txt)
txt <- gsub("\\\\textbf\\{Naive est\\. \\(95\\\\% CI\\)\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Naive est. \\\\\\\\ (95\\\\% CI)}}}", txt)
txt <- gsub("\\\\textbf\\{Lee lower\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Lee \\\\\\\\ lower}}}", txt)
txt <- gsub("\\\\textbf\\{Lee upper\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Lee \\\\\\\\ upper}}}", txt)
txt <- gsub("\\\\begin\\{table\\}\\[!t\\]", "\\\\begin{table}[H]", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
