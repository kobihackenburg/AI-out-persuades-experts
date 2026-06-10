# tab_attrition_chisq.R
# SI Table: per-study differential-attrition chi-square test of attrition
# status against assigned condition (Constrained AI treated as a distinct
# arm in Study 2). Reads output/results/attrition.rds (built by
# code/analysis/si_03_attrition.R) and pairs with tab_attrition.tex, which
# reports the per-arm rates the test is computed on.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_attrition_chisq.R ====")

a <- load_rds("attrition")

tbl_df <- a$differential_tests |>
  dplyr::transmute(
    Study        = sub("study_", "Study ", study),
    `k arms`     = n_arms,
    `chi-square` = sprintf("%.2f", chisq),
    df           = df,
    p            = ifelse(p.value < 0.001, "<.001", sprintf("%.3f", p.value)),
    `n matched`  = format(n, big.mark = ",")
  )

tbl <- gt::gt(tbl_df) |>
  style_table(
    title    = gt::md("**Differential-attrition tests, by study.**"),
    subtitle = "Per-study chi-square test of attrition status against assigned condition on the matched-only sample. Cells are the per-arm rates reported in the preceding attrition table; Constrained AI is treated as a distinct arm in Study 2."
  )

save_table(tbl, "tab_attrition_chisq", TABLES_SI,
           label  = "tab:attrition_chisq",
           layout = "auto")

tex_path <- file.path(TABLES_SI, "tab_attrition_chisq.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
            "\\\\begin{tabular}{", txt)
txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
