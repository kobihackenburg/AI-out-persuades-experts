# tab_attrition.R
# SI Table: per-(study x condition) post-treatment attrition rates on
# the matched-only denominator. Per-study chi-square differential-
# attrition tests are reported in the SI prose above the table (not in
# the caption) to keep the caption short. Reads
# output/results/attrition.rds (built by code/analysis/si_03_attrition.R).

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_attrition.R ====")

a <- load_rds("attrition")

# Display label keyed on `condition` (so AI in Study 2 splits into "AI" vs
# "AI (Constrained)"), with a stable display order that puts the
# Constrained AI immediately after Info-prompted AI within its study.
CONDITION_LABELS <- c(
  PERSUADER_CLASS_LABELS,
  ai_info        = "AI",
  ai_constrained = "AI (Constrained)"
)
CONDITION_ORDER <- c(
  setdiff(PERSUADER_CLASS_ORDER, "ai"),
  "ai_info", "ai_constrained"
)

per_cond_df <- a$per_condition |>
  dplyr::filter(!is.na(persuader_class), !is.na(condition)) |>
  dplyr::mutate(cond_order = match(condition, CONDITION_ORDER)) |>
  dplyr::arrange(study, cond_order) |>
  dplyr::transmute(
    Study       = sub("study_", "Study ", study),
    Condition   = ifelse(is.na(CONDITION_LABELS[condition]),
                         condition,
                         unname(CONDITION_LABELS[condition])),
    `n matched`  = format(n,          big.mark = ","),
    `n attrited` = format(n_attrited, big.mark = ","),
    `% attrited` = sprintf("%.1f", 100 * pct_attrited)
  )

tbl <- gt::gt(per_cond_df, groupname_col = "Study") |>
  style_table(
    title    = gt::md("**Post-treatment attrition by study x condition.**"),
    subtitle = "Matched-only denominator; in Study 2 the AI arm is split into Info-prompted and Constrained. Per-study differential-attrition tests on these cells are reported in the chi-square table that follows."
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_attrition", TABLES_SI,
           label  = "tab:attrition",
           layout = "auto")

tex_path <- file.path(TABLES_SI, "tab_attrition.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
            "\\\\begin{tabular}{", txt)
txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt)
txt <- gsub("\\\\begin\\{table\\}\\[!t\\]", "\\\\begin{table}[H]", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
