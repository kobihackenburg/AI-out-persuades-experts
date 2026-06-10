# tab_per_study_prereg.R
# SI Table: per-class persuasive estimates from the THREE preregistered
# per-study LMMs (Studies 1, 2, 3) side-by-side with the pooled main-
# text estimate.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_per_study_prereg.R ====")

extract_contrasts <- function(model, source_label) {
  em <- tryCatch(emmeans::emmeans(model, ~ group), error = function(e) NULL)
  if (is.null(em)) {
    return(tibble::tibble(group = character(), source = character(),
                          estimate = double(), se = double()))
  }
  emmeans::contrast(em, method = "trt.vs.ctrl", ref = "control") |>
    summary() |>
    tibble::as_tibble() |>
    dplyr::transmute(
      group   = sub(" - control$", "", as.character(contrast)),
      source  = source_label,
      estimate,
      se      = SE
    )
}

sources <- list(
  pooled  = function() load_rds("pooled_s1_s3_lmm"),
  study_1 = function() load_rds("study1_prereg_lmm"),
  study_2 = function() load_rds("study2_prereg_lmm"),
  study_3 = function() load_rds("study3_prereg_lmm")
)

long <- purrr::imap_dfr(sources, function(loader, label) {
  fit <- tryCatch(loader(), error = function(e) NULL)
  if (is.null(fit)) return(tibble::tibble())
  extract_contrasts(fit, label)
})

wide <- long |>
  dplyr::mutate(cell = sprintf("%+.2f (%.2f)", estimate, se)) |>
  dplyr::select(group, source, cell) |>
  tidyr::pivot_wider(names_from = source, values_from = cell)

for (col in c("study_1", "study_2", "study_3", "pooled")) {
  if (!col %in% names(wide)) wide[[col]] <- NA_character_
}

wide <- wide |>
  dplyr::mutate(group_order = match(group, PERSUADER_CLASS_ORDER)) |>
  dplyr::arrange(group_order) |>
  dplyr::transmute(
    Class                       = PERSUADER_CLASS_LABELS[as.character(group)],
    `Study 1 (prereg)`          = study_1,
    `Study 2 (prereg)`          = study_2,
    `Study 3 (prereg)`          = study_3,
    `Pooled S1-S3 (main text)`  = pooled
  )

tbl <- gt::gt(wide) |>
  style_table(
    title    = gt::md("**Per-study preregistered estimates vs. pooled main-text estimate.**"),
    subtitle = "Cells are estimate (SE) vs. control, percentage points; audit for Fig 1."
  )

save_table(tbl, "tab_per_study_prereg", TABLES_SI,
           label  = "tab:per_study_prereg",
           layout = "landscape")

tex_path <- file.path(TABLES_SI, "tab_per_study_prereg.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\textbf\\{Study 1 \\(prereg\\)\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Study 1 \\\\\\\\ (prereg)}}}", txt)
txt <- gsub("\\\\textbf\\{Study 2 \\(prereg\\)\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Study 2 \\\\\\\\ (prereg)}}}", txt)
txt <- gsub("\\\\textbf\\{Study 3 \\(prereg\\)\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Study 3 \\\\\\\\ (prereg)}}}", txt)
txt <- gsub("\\\\textbf\\{Pooled S1-S3 \\(main text\\)\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{Pooled S1-S3 \\\\\\\\ (main text)}}}", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
