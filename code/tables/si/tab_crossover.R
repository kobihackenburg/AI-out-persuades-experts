# tab_crossover.R
# SI Table: converting opponents. Among Studies 1-3 persuadees who opposed the
# assigned stance beforehand (pre < 50), the share who supported it afterwards
# (post > 50) by persuader class, with linear-probability LMM effects vs
# control per class and for human persuaders pooled, plus AI minus pooled
# human persuaders.
# Reads RDS written by code/analysis/si_21_opponents.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_crossover.R ====")

fmt_cell <- function(estimate, lower_ci, upper_ci) {
  ifelse(is.na(estimate), "",
         sprintf("%+.2f [%+.2f, %+.2f]", estimate, lower_ci, upper_ci))
}

CLASS_ORDER  <- c("control", "random_lay_person", "elite_lay_person", "canvasser",
                  "elite_debater", "coached_elite_debater", "ai_constrained", "ai")
CLASS_LABELS <- c(PERSUADER_CLASS_LABELS, ai_constrained = "Constrained AI")

rates  <- load_rds("crossover_rates")
te_cls <- load_rds("crossover_te_by_class")
pooled <- load_rds("crossover_ai_vs_human")

by_class <- rates |>
  dplyr::left_join(te_cls, by = "group") |>
  dplyr::arrange(match(group, CLASS_ORDER)) |>
  dplyr::transmute(
    block    = "By persuader class",
    Persuader = unname(CLASS_LABELS[group]),
    n         = format(n, big.mark = ",", trim = TRUE),
    rate      = sprintf("%.1f", crossover_pct),
    effect    = ifelse(group == "control", "Reference", fmt_cell(estimate, lower_ci, upper_ci))
  )

human <- dplyr::filter(rates, !group %in% c("control", "ai", "ai_constrained"))
human_row <- dplyr::filter(pooled, group == "human")
gap_row   <- dplyr::filter(pooled, group == "ai_minus_human")
pooled_rows <- tibble::tibble(
  block     = "Human persuaders pooled",
  Persuader = c("Human persuaders", "AI minus human persuaders"),
  n         = c(format(sum(human$n), big.mark = ",", trim = TRUE), ""),
  rate      = c(sprintf("%.1f", sum(human$n * human$crossover_pct) / sum(human$n)), ""),
  effect    = c(fmt_cell(human_row$estimate, human_row$lower_ci, human_row$upper_ci),
                fmt_cell(gap_row$estimate, gap_row$lower_ci, gap_row$upper_ci))
)

tbl_df <- dplyr::bind_rows(by_class, pooled_rows)

subtitle <- paste0(
  "Studies 1-3 complete cases with pre-treatment attitude below 50 (opposed to the assigned ",
  "stance). Crossed over = post-treatment attitude above 50. Effects (pp [95% CI]) are from a ",
  "linear probability version of the preregistered LMM (outcome 100 if crossed over, else 0; ",
  "pre-treatment attitude, issue, persuader class and attempt number, with random intercepts ",
  "for persuader and persuadee); effects are relative to control. The pooled rows refit the ",
  "model with the five human persuader classes merged into one level."
)

tbl <- gt::gt(tbl_df, groupname_col = "block") |>
  gt::cols_label(
    n      = "Conversations",
    rate   = "Crossed over (%)",
    effect = "Effect on crossover rate (pp)"
  ) |>
  style_table(
    title    = gt::md("**Converting opponents: crossover to the assigned stance by persuader class.**"),
    subtitle = subtitle
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_crossover", TABLES_SI,
           label  = "tab:crossover",
           layout = "auto",
           new    = TRUE)

message("  done.")
