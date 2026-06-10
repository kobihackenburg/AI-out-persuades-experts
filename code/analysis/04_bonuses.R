# 04_bonuses.R
# =============================================================================
# Per-conversation performance-bonus summaries for every persuader class that
# received the bonus (Selected Laypeople, Elite Debaters, Coached Elite
# Debaters). Reproduces, from the data, the preregistered Study 1 / 2
# bonus formula:
#
#   Individual Bonus = £15 × Relative Effectiveness × N_conversations
#   Relative Effectiveness = Persuader's TE / Mean AI TE
#
# Implementation choices:
#   * Persuader's TE = pooled class TE + pooled per-persuader BLUP, taken
#     from `robustness_main_spec` + `per_persuader_re` (both from
#     `pooled_s1_s3_lmm`). Matches the `panelb.empirical.*` per-(persuader
#     × class) estimates already emitted by 99_extract_numbers.R, so the
#     bonus calc and the "no individual human beat AI" tail use the same
#     per-persuader effect.
#   * Mean AI TE = study-specific AI vs control TE from
#     `summary_class_effects` (the same m_panel_a model used for Fig 1).
#     Per-study, because the actual incentive each class faced was relative
#     to the AI in its own study: Selected Laypeople + Elite Debaters saw
#     Study 1 AI; Coached Elite Debaters saw Study 2 AI. m_panel_a is the
#     same data + covariates as pooled_s1_s3_lmm, just with AI split by
#     study instead of by model.
#   * N_conversations per persuader is counted on the same complete-case
#     filter used by 01_main_attitudes.R and tab1_human_conditions.R, so
#     N's line up with Table 1 cells and Figure 1 denominators.
#
# Outputs:
#   bonuses_per_persuader.rds   one row per (persuader_id × class) with raw
#                               components + computed per-conversation rate
#                               + total bonus. Useful for audit / SI follow-
#                               ups if anyone wants per-persuader detail.
#   bonuses.rds                 one row per class with the summary scalars
#                               99_extract_numbers.R promotes into
#                               output/results/_numbers.json:
#                                 n_persuaders, mean_n_conversations,
#                                 mean_per_conv_gbp,
#                                 mean_total_bonus_gbp,
#                                 median_total_bonus_gbp,
#                                 mean_relative_effectiveness
# =============================================================================

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))

message("\n==== 04_bonuses.R ====")

BONUS_PER_CONV_GBP <- 15

# Classes that received the per-conversation bonus, paired with the AI
# benchmark each class actually competed against. The `ai_label` column is
# the m_panel_a (`summary_class_effects`) group label for that study's
# Info-prompted AI arm.
BONUSED_CLASSES <- tibble::tribble(
  ~raw_class,              ~study,    ~ai_label,
  "elite_lay_person",      "study_1", "AI (Info-prompted) S1",
  "elite_debater",         "study_1", "AI (Info-prompted) S1",
  "coached_elite_debater", "study_2", "AI (Info-prompted) S2"
)

# -----------------------------------------------------------------------------
# 1. Pull the three model-derived pieces from existing RDS outputs.
# -----------------------------------------------------------------------------
class_te <- load_rds("robustness_main_spec") |>
  dplyr::filter(group %in% BONUSED_CLASSES$raw_class) |>
  dplyr::transmute(raw_class = group, class_te = estimate)

per_persuader_re <- load_rds("per_persuader_re") |>
  dplyr::filter(group %in% BONUSED_CLASSES$raw_class,
                !grepl("^solo_|^ai_", persuader)) |>
  dplyr::transmute(persuader_id = as.character(persuader),
                   raw_class    = group,
                   blup         = re_intercept)

ai_by_study_te <- load_rds("summary_class_effects") |>
  dplyr::filter(group %in% BONUSED_CLASSES$ai_label) |>
  dplyr::transmute(ai_label = group, ai_mean_te = estimate)

# -----------------------------------------------------------------------------
# 2. Per-persuader conversation counts (matches Table 1 cell counts).
# -----------------------------------------------------------------------------
counts_for_class <- function(study_id, class) {
  load_study(study_id) |>
    complete_case_filter(
      require_attitudes = TRUE,
      label             = sprintf("04_bonuses/%s/%s", study_id, class)
    ) |>
    dplyr::filter(persuader_class == class) |>
    dplyr::count(persuader_id, name = "n_conversations") |>
    dplyr::mutate(persuader_id = as.character(persuader_id),
                  raw_class    = class)
}

n_per_persuader <- purrr::pmap_dfr(
  BONUSED_CLASSES,
  function(raw_class, study, ai_label) counts_for_class(study, raw_class)
)

# -----------------------------------------------------------------------------
# 3. Join everything and compute the per-persuader bonus.
# -----------------------------------------------------------------------------
bonuses_per_persuader <- per_persuader_re |>
  dplyr::inner_join(class_te,        by = "raw_class") |>
  dplyr::inner_join(BONUSED_CLASSES, by = "raw_class") |>
  dplyr::inner_join(ai_by_study_te,  by = "ai_label") |>
  dplyr::inner_join(n_per_persuader, by = c("persuader_id", "raw_class")) |>
  dplyr::mutate(
    persuader_te           = class_te + blup,
    relative_effectiveness = persuader_te / ai_mean_te,
    per_conv_bonus_gbp     = BONUS_PER_CONV_GBP * relative_effectiveness,
    total_bonus_gbp        = per_conv_bonus_gbp * n_conversations
  ) |>
  dplyr::select(raw_class, study, persuader_id, n_conversations,
                class_te, blup, persuader_te, ai_mean_te,
                relative_effectiveness, per_conv_bonus_gbp, total_bonus_gbp)

save_rds(bonuses_per_persuader, "bonuses_per_persuader")

# -----------------------------------------------------------------------------
# 4. Per-class summary (the scalars 99_extract_numbers.R promotes).
# -----------------------------------------------------------------------------
bonuses <- bonuses_per_persuader |>
  dplyr::group_by(raw_class) |>
  dplyr::summarise(
    n_persuaders                = dplyr::n_distinct(persuader_id),
    mean_n_conversations        = mean(n_conversations),
    mean_per_conv_gbp           = mean(per_conv_bonus_gbp),
    mean_total_bonus_gbp        = mean(total_bonus_gbp),
    median_total_bonus_gbp      = stats::median(total_bonus_gbp),
    mean_relative_effectiveness = mean(relative_effectiveness),
    .groups                     = "drop"
  )

save_rds(bonuses, "bonuses")

# -----------------------------------------------------------------------------
# 5. Audit log (so re-runs show the headline numbers in stdout).
# -----------------------------------------------------------------------------
message("\n  per-class bonus summary:")
message(sprintf("    %-22s  %4s  %5s  %7s  %7s  %5s",
                "class", "N", "n_cnv", "per_cnv", "total", "rel.E"))
for (i in seq_len(nrow(bonuses))) {
  r <- bonuses[i, ]
  display <- unname(PERSUADER_CLASS_LABELS[r$raw_class])
  message(sprintf("    %-22s  %4d  %5.1f  £%6.2f  £%6.0f  %4.2f",
                  display, r$n_persuaders, r$mean_n_conversations,
                  r$mean_per_conv_gbp, r$mean_total_bonus_gbp,
                  r$mean_relative_effectiveness))
}

message("\n  done.")
