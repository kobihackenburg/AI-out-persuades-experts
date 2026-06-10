# tab_robustness_checks.R
# SI Table: side-by-side comparison of the main-text body model with
# two alternative specifications (+ study FE, AI as singleton RE).

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_robustness_checks.R ====")

fmt_cell <- function(estimate, lower_ci, upper_ci) {
  ifelse(is.na(estimate), "",
         sprintf("%+.2f [%+.2f, %+.2f]", estimate, lower_ci, upper_ci))
}

load_spec <- function(name, col_label) {
  load_rds(name) |>
    dplyr::transmute(group, !!col_label := fmt_cell(estimate, lower_ci, upper_ci))
}

main_spec    <- load_spec("robustness_main_spec",         "Main spec")
study_fe     <- load_spec("robustness_with_study_fe",     "+ study FE")
ai_singleton <- load_spec("robustness_ai_as_singleton",   "AI as singleton RE")

wide <- main_spec |>
  dplyr::full_join(study_fe,     by = "group") |>
  dplyr::full_join(ai_singleton, by = "group") |>
  dplyr::mutate(group_order = match(group, PERSUADER_CLASS_ORDER)) |>
  dplyr::arrange(group_order) |>
  dplyr::transmute(
    Class = unname(PERSUADER_CLASS_LABELS[group]),
    `Main spec`,
    `+ study FE`,
    `AI as singleton RE`
  )
wide$Class[is.na(wide$Class)] <- main_spec$group[is.na(wide$Class)]

tbl <- gt::gt(wide) |>
  style_table(
    title    = gt::md("**Robustness of per-class persuasive impact to alternative model specifications.**"),
    subtitle = "Cells are estimate [95% CI] in percentage points; specification details discussed in the SI text above."
  )

save_table(tbl, "tab_robustness_checks", TABLES_SI,
           label  = "tab:robustness_checks",
           layout = "shrink")

tex_path <- file.path(TABLES_SI, "tab_robustness_checks.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{table\\}\\[!t\\]", "\\\\begin{table}[H]", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
