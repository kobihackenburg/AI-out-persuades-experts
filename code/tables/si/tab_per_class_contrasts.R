# tab_per_class_contrasts.R
# SI Table: per-class persuasive contrasts vs control from the pooled
# S1-S3 LMM.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_per_class_contrasts.R ====")

m <- load_rds("pooled_s1_s3_lmm")
em <- tryCatch(emmeans::emmeans(m, ~ group), error = function(e) NULL)

contrasts_df <- if (!is.null(em)) {
  rows <- emmeans::contrast(em, method = "trt.vs.ctrl", ref = "control") |>
    summary(infer = TRUE, adjust = "none") |>
    tibble::as_tibble()
  ci_lower <- if ("lower.CL" %in% names(rows)) rows$lower.CL else rows$asymp.LCL
  ci_upper <- if ("upper.CL" %in% names(rows)) rows$upper.CL else rows$asymp.UCL
  rows |>
    dplyr::transmute(
      Class    = sub(" - control$", "", as.character(contrast)),
      Estimate = sprintf("%+.2f", estimate),
      `95% CI` = sprintf("[%+.2f, %+.2f]", ci_lower, ci_upper),
      p        = ifelse(p.value < 0.001, "<.001", sprintf("%.3f", p.value))
    ) |>
    dplyr::mutate(Class = factor(Class,
                                 levels = intersect(PERSUADER_CLASS_ORDER, Class))) |>
    dplyr::arrange(Class) |>
    dplyr::mutate(Class = PERSUADER_CLASS_LABELS[as.character(Class)])
} else {
  tibble::tibble(Class = character(), Estimate = character(),
                 `95% CI` = character(), p = character())
}

tbl <- gt::gt(contrasts_df) |>
  style_table(
    title    = gt::md("**Per-class persuasive impact vs. control (pooled S1-S3).**"),
    subtitle = "Marginal contrasts from the pooled LMM; complete-case sample."
  )

save_table(tbl, "tab_per_class_contrasts", TABLES_SI,
           label  = "tab:per_class_contrasts",
           layout = "auto")

tex_path <- file.path(TABLES_SI, "tab_per_class_contrasts.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
            "\\\\begin{tabular}{", txt)
txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
