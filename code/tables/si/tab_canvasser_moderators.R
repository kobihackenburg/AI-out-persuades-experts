# tab_canvasser_moderators.R
# SI Table: per-(moderator, level) AI-minus-humans estimate, joint Wald
# chi-square interaction p-value, and Benjamini-Hochberg q-value across
# the 14-moderator family, refit on the full Studies 1-3 frame.
# Canvasser-inclusion robustness analogue of tab_moderators.R; cited
# from SI Section~\ref{si:mechanism-with-canvasser}.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_canvasser_moderators.R ====")

mef <- load_rds("moderator_effects")

MOD_LABELS <- c(
  pre_attitude          = "Pre-treatment attitude (0-100)",
  pre_importance        = "Pre-treatment issue importance (0-100)",
  pre_issue_knowledge   = "Pre-treatment issue knowledge (0-100)",
  age                   = "Age (years)",
  gender                = "Gender",
  education             = "Education",
  income                = "Income bracket",
  ethnicity             = "Ethnicity",
  party                 = "Party affiliation",
  ideology_coded        = "Ideology (0 = Left, 4 = Right)",
  political_knowledge_n = "Political knowledge (0-4 correct)",
  dogmatism             = "Dogmatism scale",
  empathic_trust        = "Empathic-trust scale",
  ai_trust              = "AI-trust scale"
)

tbl_df <- mef |>
  dplyr::mutate(
    mod_lbl   = unname(ifelse(is.na(MOD_LABELS[moderator]),
                              moderator, MOD_LABELS[moderator])),
    mod_ord   = match(moderator, names(MOD_LABELS)),
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
    title    = gt::md("**AI advantage across 14 moderators -- canvasser-inclusion refit.**"),
    subtitle = "Pooled Studies 1-3 LMM with a persuader_class x moderator interaction (all 5 human classes including Professional Canvassers). Per-level rows give the AI-minus-humans contrast at each level (categorical) or at the 25th and 75th percentiles (continuous). q (BH) controls FDR across the 14-moderator family. Main-text analogue (Studies 1-2 only) is in Table~\\ref{tab:moderators}."
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_canvasser_moderators", TABLES_SI,
           label  = "tab:canvasser_moderators",
           layout = "longtable")

tex_path <- file.path(TABLES_SI, "tab_canvasser_moderators.tex")
txt <- readLines(tex_path, encoding = "UTF-8")
txt <- gsub("\\\\textbf\\{p \\(within\\)\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{p \\\\\\\\ (within)}}}", txt)
txt <- gsub("\\\\textbf\\{p \\(interaction\\)\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{p \\\\\\\\ (interaction)}}}", txt)
txt <- gsub("\\\\textbf\\{q \\(BH\\)\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{q \\\\\\\\ (BH)}}}", txt)
txt <- gsub("\\\\textbf\\{95\\\\% CI\\}",
            "\\\\textbf{\\\\raisebox{-0.5\\\\height}{\\\\shortstack{95\\\\% \\\\\\\\ CI}}}", txt)
writeLines(txt, tex_path, useBytes = FALSE)

message("  done.")
