# tab_moderators.R
# SI Table: per-(moderator, level) AI-minus-humans estimate, joint Wald
# chi-square interaction p-value, and Benjamini-Hochberg q-value across
# the 14-moderator family. Reads output/results/moderator_effects.rds,
# which already carries `kind` (continuous / categorical), `level` (raw
# level for categorical; "Low (Q1 = x)" / "High (Q3 = x)" for continuous),
# and `q_bh`.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_moderators.R ====")

# Backs the main-text mechanism-section claim, so reads the Studies 1-2
# refit. Canvasser-inclusion analogue lives in
# Section~\ref{si:mechanism-with-canvasser}.
mef <- load_rds("moderator_effects_s1_s2")

MOD_LABELS <- c(
  # pre-treatment
  pre_attitude          = "Pre-treatment attitude (0-100)",
  pre_importance        = "Pre-treatment issue importance (0-100)",
  pre_issue_knowledge   = "Pre-treatment issue knowledge (0-100)",
  # demographics
  age                   = "Age (years)",
  gender                = "Gender",
  education             = "Education",
  income                = "Income bracket",
  ethnicity             = "Ethnicity",
  # political identity / knowledge
  party                 = "Party affiliation",
  ideology_coded        = "Ideology (0 = Left, 4 = Right)",
  political_knowledge_n = "Political knowledge (0-4 correct)",
  # psychological scales
  dogmatism             = "Dogmatism scale",
  empathic_trust        = "Empathic-trust scale",
  ai_trust              = "AI-trust scale"
)

tbl_df <- mef |>
  dplyr::mutate(
    mod_lbl   = unname(ifelse(is.na(MOD_LABELS[moderator]),
                              moderator, MOD_LABELS[moderator])),
    mod_ord   = match(moderator, names(MOD_LABELS)),
    # Within a moderator: continuous gets Low (Q1) before High (Q3);
    # categorical sorts alphabetically by level.
    level_ord = dplyr::case_when(
      kind == "continuous" & startsWith(level, "Low")  ~ 1L,
      kind == "continuous" & startsWith(level, "High") ~ 2L,
      TRUE                                              ~ NA_integer_
    )
  ) |>
  dplyr::arrange(mod_ord, level_ord, level) |>
  dplyr::transmute(
    Moderator         = mod_lbl,
    Level             = level,
    Estimate          = sprintf("%+.2f", estimate),
    `95% CI`          = sprintf("[%+.2f, %+.2f]", lower_ci, upper_ci),
    `p (within)`      = ifelse(p_value < 0.001, "<.001",
                                sprintf("%.3f", p_value)),
    `p (interaction)` = ifelse(p_interaction < 0.001, "<.001",
                                sprintf("%.3f", p_interaction)),
    `q (BH)`          = ifelse(q_bh < 0.001, "<.001",
                                sprintf("%.3f", q_bh))
  )

tbl <- gt::gt(tbl_df, groupname_col = "Moderator") |>
  style_table(
    title    = gt::md("**AI advantage across 14 tested moderators.**"),
    subtitle = "Pooled S1-S2 LMM with a persuader_class x moderator interaction. Per-level rows give the AI-minus-humans contrast at each level (categorical) or at the 25th and 75th percentiles (continuous). q (BH) controls FDR across the 14-moderator family. Canvasser-inclusion analogue (the same table refit on the full Studies 1-3 frame) is in Section~\\ref{si:mechanism-with-canvasser}."
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_moderators", TABLES_SI,
           label  = "tab:moderators",
           layout = "longtable")

tex_path <- file.path(TABLES_SI, "tab_moderators.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\textbf\\{p \\(within\\)\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{p \\\\\\\\ (within)}}}", txt)
txt <- gsub("\\\\textbf\\{p \\(interaction\\)\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{p \\\\\\\\ (interaction)}}}", txt)
txt <- gsub("\\\\textbf\\{q \\(BH\\)\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{q \\\\\\\\ (BH)}}}", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
