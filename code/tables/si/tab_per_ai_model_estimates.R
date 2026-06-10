# tab_per_ai_model_estimates.R
# SI Table: per-AI-model treatment-effect estimates that underlie the
# black stencils on Figure 1. One row per (model x study) Info-prompted
# arm + one row per Constrained AI model (Study 2).

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_per_ai_model_estimates.R ====")

ai <- load_rds("summary_ai_overlays")

parsed <- ai |>
  dplyr::mutate(
    is_constrained = grepl("\\(Constrained\\)$", group),
    study          = dplyr::case_when(
      grepl("\\(Info\\) S1$", group) ~ "Study 1",
      grepl("\\(Info\\) S2$", group) ~ "Study 2",
      grepl("\\(Info\\) S3$", group) ~ "Study 3",
      is_constrained                  ~ "Study 2",
      TRUE                            ~ NA_character_
    ),
    variant        = ifelse(is_constrained, "Constrained", "Info-prompted"),
    model          = group |>
                       sub(" \\(Info\\) S[123]$", "", x = _) |>
                       sub(" \\(Constrained\\)$",  "", x = _)
  )

tbl_df <- parsed |>
  dplyr::arrange(variant, model, study) |>
  dplyr::transmute(
    Variant   = variant,
    Model     = model,
    Study     = study,
    Estimate  = sprintf("%+.2f", estimate),
    `95% CI`  = sprintf("[%+.2f, %+.2f]", lower_ci, upper_ci)
  )

tbl <- gt::gt(tbl_df, groupname_col = "Variant") |>
  style_table(
    title    = gt::md("**Per-AI-model treatment-effect estimates.**"),
    subtitle = "Each row reports estimate [95% CI] in percentage points for one model in one study; underlies the black stencils on Fig. 1."
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_per_ai_model_estimates", TABLES_SI,
           label  = "tab:per_ai_model_estimates",
           layout = "auto")

tex_path <- file.path(TABLES_SI, "tab_per_ai_model_estimates.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\begin\\{tabular\\*\\}\\{\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}",
            "\\\\begin{tabular}{", txt)
txt <- gsub("\\\\end\\{tabular\\*\\}", "\\\\end{tabular}", txt)
txt <- gsub("\\\\begin\\{table\\}\\[!t\\]", "\\\\begin{table}[H]", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
