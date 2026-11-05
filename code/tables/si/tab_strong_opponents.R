# tab_strong_opponents.R
# SI Table: AI's advantage over each human persuader class among Studies 1-3
# persuadees who strongly opposed the assigned stance (pre-treatment attitude
# 0-25 or 0-10) and among strong opponents with high issue knowledge
# (attitude 0-25, knowledge 70-100). One row group per sample.
# Reads RDS written by code/analysis/si_21_opponents.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_strong_opponents.R ====")

SAMPLE_ORDER <- c("pre_le_25", "pre_le_10", "pre_le_25_knowledge_ge_70")
SAMPLE_LABELS <- c(
  pre_le_25                 = "Attitude 0--25",
  pre_le_10                 = "Attitude 0--10",
  pre_le_25_knowledge_ge_70 = "Attitude 0--25 and issue knowledge 70--100"
)
HUMAN_ORDER <- c("random_lay_person", "elite_lay_person", "canvasser",
                 "elite_debater", "coached_elite_debater")
CONTRAST_ORDER <- c("ai_vs_control", paste0("ai_minus_", HUMAN_ORDER), "ai_minus_human")
CONTRAST_LABELS <- c(
  ai_vs_control = "AI vs control",
  setNames(paste("AI minus", PERSUADER_CLASS_LABELS[HUMAN_ORDER]), paste0("ai_minus_", HUMAN_ORDER)),
  ai_minus_human = "AI minus human persuaders (pooled)"
)

tbl_df <- load_rds("strong_opponents") |>
  dplyr::filter(contrast %in% CONTRAST_ORDER) |>
  dplyr::arrange(match(sample, SAMPLE_ORDER), match(contrast, CONTRAST_ORDER)) |>
  dplyr::transmute(
    group = sprintf("%s (%s conversations, %s persuadees)", SAMPLE_LABELS[sample],
                    format(n_conversations, big.mark = ",", trim = TRUE),
                    format(n_persuadees, big.mark = ",", trim = TRUE)),
    Comparison = unname(CONTRAST_LABELS[contrast]),
    Estimate   = sprintf("%+.2f", estimate),
    `95% CI`   = sprintf("[%+.2f, %+.2f]", lower_ci, upper_ci),
    p          = ifelse(p_value < 0.001, "<.001", sprintf("%.3f", p_value)),
    n          = ifelse(is.na(n_class), "", format(n_class, big.mark = ",", trim = TRUE))
  )

subtitle <- paste0(
  "Studies 1-3 complete cases by pre-treatment attitude toward the assigned stance (0-100) and ",
  "self-reported issue knowledge (0-100). Estimates (pp) from the preregistered LMM with a study ",
  "fixed effect (post-treatment attitude on pre-treatment attitude, issue, persuader class, ",
  "attempt number and study, with random intercepts for persuader and persuadee), fit separately ",
  "in each sample. n is the number of conversations with that human persuader class in the ",
  "sample. The pooled row refits the model with the five human persuader classes merged into one ",
  "level."
)

tbl <- gt::gt(tbl_df, groupname_col = "group") |>
  style_table(
    title    = gt::md("**AI's advantage among persuadees who strongly opposed the assigned stance.**"),
    subtitle = subtitle
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_strong_opponents", TABLES_SI,
           label  = "tab:strong_opponents",
           layout = "auto",
           new    = TRUE)

message("  done.")
