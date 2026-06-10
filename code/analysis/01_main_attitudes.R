# 01_main_attitudes.R
# =============================================================================
# Main-text attitude-change analyses (Studies 1-3 pooled) + every RDS that
# backs Figure 1 (summary_figure.{pdf,png}), the SI per-study audit
# table, and the SI robustness-check table.
#
# Fits LMMs and writes tidy RDS outputs to `output/results/`. Figure and
# table scripts re-load these without re-fitting, so figure tweaks never
# re-run the analysis.
#
# Outputs (under output/results/, consumer after `->`):
#   pooled_s1_s3_lmm.rds              -> tab_pooled_lmm_coefs, tab_per_class_contrasts,
#                                        tab_per_study_prereg, tab_moderators
#   pooled_s1_s2_lmm.rds              -> mechanism-section body-text contrasts
#                                        + per-persuader BLUPs for Fig 2b
#   per_persuader_re.rds              -> SI canvasser-inclusion table (the S1-S3
#                                        BLUP table; the main-text Fig 2b uses
#                                        limits_per_persuader_re.rds instead)
#   limits_per_persuader_re.rds         -> Fig 2 panel B per-persuader BLUPs from
#                                        the S1+S2-only pooled LMM
#   study{1,2,3}_prereg_lmm.rds       -> tab_per_study_prereg
#   summary_class_effects.rds         -> Figure 1 summary forest (fig1_summary.R)
#                                        + per-study AI benchmark (04_bonuses.R):
#                                        all 5 human classes + 3 AI-by-study +
#                                        Constrained AI
#   summary_ai_overlays.rds           -> Fig 1 (summary) AI per-model stencils,
#                                        tab_per_ai_model_estimates
#   canvasser_class_distributions.rds -> SI canvasser-inclusion distributions
#                                        (5 classes, S1-S3 fit). Main-text Fig 2b
#                                        uses limits_class_distributions.rds
#                                        instead (4 classes, S1+S2 fit).
#   canvasser_ai_reference.rds     -> SI canvasser-inclusion AI reference line
#                                        (avg over S1+S2+S3 AI arms)
#   limits_class_distributions.rds -> Fig 2 panel B normal curves
#                                        (4 non-canvasser classes; mu, tau
#                                        from S1+S2 fits)
#   limits_ai_reference.rds     -> Fig 2 panel B red dashed AI line
#                                        (avg over S1+S2 AI arms only)
#   mechanism_te_inputs.rds        -> Per-(model x study) TE for Fig 3b
#                                        from m_ai_sep refit on S1+S2 only
#   canvasser_te_inputs.rds           -> SI canvasser-inclusion analogue: same
#                                        table from S1-S3 m_ai_sep fit (used by
#                                        fig_facts_with_canvasser.R)
#   robustness_main_spec.rds          -> tab_robustness_checks (column 1)
#   robustness_with_study_fe.rds      -> tab_robustness_checks (column 2)
#   robustness_ai_as_singleton.rds    -> tab_robustness_checks (column 3)
#
# =============================================================================
# WHAT THIS SCRIPT FITS
# =============================================================================
# Preregistered per-study model (one per study, fit on S1, S2, S3
# separately; reported in tab_per_study_prereg):
#
#   post_attitude ~ pre_attitude + issue + group + persuadee_attempt_number
#                 + (1 | persuader) + (1 | persuadee)
#
# Pooled S1-S3 model (used for the manuscript body-text contrasts and
# Figure 1): same RHS, fit jointly across the three studies.
#
# AI persuader RE follows Study 1's prereg note: AI rows are grouped by
# AI model (`persuader = ai_<model>`). Control rows have no persuader,
# so each is given a unique singleton ID. Sample is complete-case
# (`code/analysis/_setup.R::complete_case_filter`: pre + post attitude
# non-missing).
#
# Robustness checks (SI table tabS4): refit the pooled model with
# (a) `+ study` added as a fixed effect, and (b) AI rows given singleton
# persuader IDs (no AI-model grouping). Headline conclusions are
# unchanged in both cases (audit log at the end of this script).
# =============================================================================

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))
source(here::here("code/analysis/_lmm_helpers.R"))

message("\n==== 01_main_attitudes.R ====")

# =============================================================================
# 1. Helpers
# =============================================================================
# Shared LMM helpers (`fit_pooled_lmm`, `tidy_te_vs_control`,
# `clean_contrast_name`) live in code/analysis/_lmm_helpers.R. Only the
# Fig-1-specific helper is defined here.

# fit_class_persuader_sd(): per-class LMM that returns the between-
# persuader SD for one human class. Used by Panel D (and the SI
# canvasser-inclusion analogue) to set the width of each normal-density
# curve. `data` defaults to the global `pooled_df` (S1-S3) but can be
# passed in (e.g. S1+S2 only for the main-text mechanism figure).
# Returns NA (with a logged warning) if the class has too few rows /
# persuaders to fit reliably.
fit_class_persuader_sd <- function(raw_class, data = pooled_df) {
  class_df <- data |>
    dplyr::filter(group == raw_class) |>
    droplevels()
  if (nrow(class_df) < 30L || dplyr::n_distinct(class_df$persuader) < 5L) {
    message(sprintf("    [class SD] skip %s: only %d rows / %d persuaders",
                    raw_class, nrow(class_df),
                    dplyr::n_distinct(class_df$persuader)))
    return(NA_real_)
  }
  m <- tryCatch(
    lme4::lmer(
      post ~ pre + issue + attempt + (1 | persuader) + (1 | persuadee),
      data    = class_df,
      REML    = TRUE,
      control = lme4::lmerControl(optimizer = "bobyqa",
                                  optCtrl   = list(maxfun = 100000))
    ),
    error = function(e) {
      message(sprintf("    [class SD] %s fit failed: %s",
                      raw_class, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(m)) return(NA_real_)
  vc <- as.data.frame(lme4::VarCorr(m))
  sd_persuader <- vc$sdcor[vc$grp == "persuader"]
  if (length(sd_persuader) == 0L) NA_real_ else as.numeric(sd_persuader)
}

# =============================================================================
# 2. Data assembly
# =============================================================================
# prep(): extract the model-relevant columns from one study frame, apply
# the complete-case filter, and assign the persuader RE ID (real
# `persuader_id` for humans, `ai_<model>` for AI per Study 1's prereg,
# unique singleton for controls). `ai_model_full` is kept downstream for
# the Fig 1 per-AI-model overlay and the AI-as-singleton robustness fit.
prep <- function(df, study_label) {
  df |>
    complete_case_filter(label = paste("01_main", study_label)) |>
    dplyr::transmute(
      study           = study_label,
      persuadee       = persuadee_id,
      # Persuader RE grouping: real `persuader_id` for human
      # conversations (one group per human persuader), `ai_<model>` for
      # AI conversations (one group per AI model, per Study 1's
      # preregistered note), and a unique singleton for control rows
      # (which have no persuader).
      persuader       = dplyr::case_when(
        !is.na(persuader_id)                            ~ persuader_id,
        persuader_class == "ai" & !is.na(ai_model_full) ~ paste0("ai_", ai_model_full),
        TRUE                                            ~ paste0("solo_", persuadee_session)
      ),
      issue           = issue_id,
      group           = persuader_class,
      pre             = pre_attitude,
      post            = post_attitude,
      # Preregistered continuous covariate, recorded in the data as the
      # chronological rank of completed persuadee sessions per (study,
      # participantId). Residual NAs (attrited rows already dropped by
      # the complete-case filter) are coalesced to 1L for safety.
      attempt         = dplyr::coalesce(as.integer(persuadee_attempt_number), 1L),
      ai_constrained  = ai_constrained,
      ai_model_full   = ai_model_full,
      persuadee_session = persuadee_session
    )
}

studies <- load_studies(c("study_1", "study_2", "study_3"))

pooled_df <- dplyr::bind_rows(
  prep(studies$study_1, "study_1"),
  prep(studies$study_2, "study_2"),
  prep(studies$study_3, "study_3")
) |>
  dplyr::mutate(
    study = factor(study),
    group = forcats::fct_relevel(droplevels(group), "control")
  )

# Build fig1_df: pooled_df plus the figure-specific grouping columns
# that the Panel A models key off.
#   group_unified      collapses all info-prompted AI rows into one level
#                      ("AI (Info-prompted)"), and constrained AI into
#                      another ("AI (Constrained)").
#   group_by_study     further splits "AI (Info-prompted)" by study, so
#                      each shows as a separate jittered marker in Fig 1.
#   group_ai_separate  splits AI by model x study for the per-model
#                      stencil overlays in Fig 1.
fig1_df <- pooled_df |>
  dplyr::mutate(
    constrained_flag = dplyr::coalesce(ai_constrained, FALSE),
    group_unified = dplyr::case_when(
      group == "control"                ~ "Control",
      group == "ai" &  constrained_flag ~ "AI (Constrained)",
      group == "ai" & !constrained_flag ~ "AI (Info-prompted)",
      group == "random_lay_person"      ~ "Random Layperson",
      group == "elite_lay_person"       ~ "Elite Layperson",
      group == "canvasser"              ~ "Professional Canvasser",
      group == "elite_debater"          ~ "Elite Debater",
      group == "coached_elite_debater"  ~ "Coached Elite Debater",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::filter(!is.na(group_unified)) |>
  dplyr::mutate(
    group_by_study = dplyr::case_when(
      group_unified == "AI (Info-prompted)" & study == "study_1" ~ "AI (Info-prompted) S1",
      group_unified == "AI (Info-prompted)" & study == "study_2" ~ "AI (Info-prompted) S2",
      group_unified == "AI (Info-prompted)" & study == "study_3" ~ "AI (Info-prompted) S3",
      TRUE ~ group_unified
    ),
    ai_model_label = dplyr::case_when(
      ai_model_full == "claude-4.1-opus" ~ "Claude Opus 4.1",
      ai_model_full == "claude-4.6-opus" ~ "Claude Opus 4.6",
      ai_model_full == "gemini-2.5-pro"  ~ "Gemini 2.5 Pro",
      ai_model_full == "gpt-4o-latest"   ~ "ChatGPT-4o (latest)",
      ai_model_full == "gpt-5-4"         ~ "GPT-5.4",
      ai_model_full == "grok-4"          ~ "Grok 4.20",
      TRUE ~ NA_character_
    ),
    group_ai_separate = dplyr::case_when(
      group == "ai" &  constrained_flag ~ paste0(ai_model_label, " (Constrained)"),
      group == "ai" & !constrained_flag ~ paste0(ai_model_label, " (Info) ",
                                                 toupper(sub("study_", "S", study))),
      TRUE ~ group_unified
    )
  )

message(sprintf("  pooled_df: %d rows, %d persuaders, %d persuadees",
                nrow(pooled_df),
                dplyr::n_distinct(pooled_df$persuader),
                dplyr::n_distinct(pooled_df$persuadee)))
message(sprintf("  fig1_df:   %d rows, group_unified=%d levels, group_by_study=%d levels, group_ai_separate=%d levels",
                nrow(fig1_df),
                dplyr::n_distinct(fig1_df$group_unified),
                dplyr::n_distinct(fig1_df$group_by_study),
                dplyr::n_distinct(fig1_df$group_ai_separate)))

# Persist the pooled S1-S3 complete-case frame so SI scripts can re-use the
# exact same preprocessing (complete-case filter, persuader RE coding,
# `attempt` covariate) instead of duplicating `prep()`. Consumer:
# si_11_repeat_participation.R (first-session-only and carryover refits).
save_rds(pooled_df, "pooled_df")

# =============================================================================
# 3. Pooled LMMs (body text + Figure 1 + SI audit)
# =============================================================================

message("\n---- 3. fitting pooled LMMs ----")

# 3a. Body-text model: pooled S1-S3 with the raw `group` factor.
#     Consumers: tabS1 (per-class contrasts), tabS3 (pooled column),
#     99_extract_numbers (inline contrasts).
pooled_s1_s3_lmm <- fit_pooled_lmm(pooled_df,
                                   group_col = "group",
                                   label     = "pooled_s1_s3_lmm")
save_rds(pooled_s1_s3_lmm, "pooled_s1_s3_lmm")

# 3b. Panel A model: AI arm split by study so the three study estimates
#     show as separate jittered markers in Fig 1. Drives Panel A and
#     (via section 4) the Panel B class means and AI reference scalar.
m_panel_a <- fit_pooled_lmm(fig1_df,
                            group_col = "group_by_study",
                            ref       = "Control",
                            label     = "m_panel_a")

# 3c. AI-per-model overlay model: same as Panel A but with AI split by
#     (model x study) so we can plot the small black stencils on top of
#     the parent AI estimates.
m_ai_sep <- fit_pooled_lmm(fig1_df,
                           group_col = "group_ai_separate",
                           ref       = "Control",
                           label     = "m_ai_sep")

# 3d. Preregistered per-study LMMs (SI audit only). One LMM per study,
#     fit on that study's subset of pooled_df. Consumer: tabS3.
fit_per_study <- function(study_label) {
  df <- pooled_df |> dplyr::filter(study == study_label) |> droplevels()
  fit_pooled_lmm(df, group_col = "group",
                 label = sprintf("per_study[%s]", study_label))
}
save_rds(fit_per_study("study_1"), "study1_prereg_lmm")
save_rds(fit_per_study("study_2"), "study2_prereg_lmm")
save_rds(fit_per_study("study_3"), "study3_prereg_lmm")

# 3e. Per-persuader random effects, extracted from the body-text model.
#     Consumers: Fig 1 panel B (raw distribution shown alongside the
#     analytical densities), tabS2.
per_persuader_re <- tibble::as_tibble(lme4::ranef(pooled_s1_s3_lmm)$persuader,
                                      rownames = "persuader") |>
  dplyr::rename(re_intercept = `(Intercept)`) |>
  dplyr::left_join(
    pooled_df |> dplyr::distinct(persuader, group, study),
    by = "persuader"
  )
save_rds(per_persuader_re, "per_persuader_re")

# =============================================================================
# 4. Figure 1 inputs
# =============================================================================

message("\n---- 4. extracting Fig 1 inputs ----")

# 4a. Panel A point estimates (per-group point ranges). One row per
#     non-reference level of group_by_study, so the resulting table
#     covers every human class (5), the three AI-by-study estimates,
#     and the Constrained AI estimate. Backs the summary figure
#     (Figure 1, end of intro) via fig1_summary.R, and the per-study AI
#     benchmark in 04_bonuses.R; the Study-1 results are read off the
#     same figure via the circle (Study 1) markers.
summary_class_effects <- tidy_te_vs_control(m_panel_a, "group_by_study",
                                            ref = "Control")
save_rds(summary_class_effects, "summary_class_effects")

# 4b. Panel A AI-per-model stencils. Filter to the per-(model x study)
#     and per-(model, constrained) rows the figure actually overlays.
summary_ai_overlays <- tidy_te_vs_control(m_ai_sep, "group_ai_separate",
                                               ref = "Control") |>
  dplyr::filter(grepl("\\(Info\\) S[123]$|\\(Constrained\\)$", group)) |>
  dplyr::select(group, estimate, lower_ci, upper_ci)
save_rds(summary_ai_overlays, "summary_ai_overlays")

# 4b'. Full per-(group_ai_separate) TE table from m_ai_sep, including
#      humans / coached / canvasser. Consumer: Fig 2 panel C, which joins
#      this with per-condition fact density. Saved here (rather than
#      refit in 02_mechanism.R) so the two figures share one source of
#      truth for AI-per-model contrasts.
canvasser_te_inputs <- tidy_te_vs_control(m_ai_sep, "group_ai_separate",
                                        ref = "Control")
save_rds(canvasser_te_inputs, "canvasser_te_inputs")

# 4c. Panel B AI reference scalar (the red dashed vertical line). We
#     compute it as the mean of the three AI-by-study contrasts from
#     m_panel_a, which is algebraically equivalent to a single linear
#     contrast (AI_S1 + AI_S2 + AI_S3)/3 - Control on the same model.
#     Using m_panel_a (rather than a separate "unified" fit on
#     group_unified) guarantees Panel A and Panel B never disagree
#     about the AI effect, and saves one model fit.
ai_x_study_te <- summary_class_effects |>
  dplyr::filter(grepl("^AI \\(Info-prompted\\) S[123]$", group))
canvasser_ai_reference <- mean(ai_x_study_te$estimate)
save_rds(canvasser_ai_reference, "canvasser_ai_reference")

# 4d. Panel B per-class normal densities.
#     mean: the Panel A point estimate for that class (looked up from
#           summary_class_effects so the two panels never disagree).
#     sd:   the class-specific between-persuader SD from a per-class
#           REML fit (fit_class_persuader_sd), floored at the pooled
#           across-class persuader SD from pooled_s1_s3_lmm.
#
#     The floor handles boundary (singular) per-class fits with
#     tau_hat = 0 (which would otherwise plot as an infinitely sharp
#     spike). It is an empirical-Bayes-style fallback: when a single
#     class is uninformative about between-persuader variability, we
#     borrow strength from the across-class average. The pooled SD
#     sits well within the bootstrap 95% CI for any singular class
#     (e.g. [0, 9.6] pp for Elite Debater).
HUMAN_CLASSES <- c(
  random_lay_person     = "Random Layperson",
  elite_lay_person      = "Elite Layperson",
  canvasser             = "Professional Canvasser",
  elite_debater         = "Elite Debater",
  coached_elite_debater = "Coached Elite Debater"
)
PANEL_B_SD_FLOOR <- as.numeric(attr(lme4::VarCorr(pooled_s1_s3_lmm)$persuader,
                                    "stddev"))

message(sprintf("  per-class persuader SDs (raw -> displayed; floored at pooled tau_hat = %.2f pp):",
                PANEL_B_SD_FLOOR))

canvasser_class_distributions <- tibble::tibble(
  raw_class     = names(HUMAN_CLASSES),
  display_label = unname(HUMAN_CLASSES)
) |>
  dplyr::mutate(
    group_te = vapply(display_label, function(lbl) {
      v <- summary_class_effects |>
        dplyr::filter(group == lbl) |>
        dplyr::pull(estimate)
      if (length(v) == 0L) NA_real_ else v
    }, numeric(1L), USE.NAMES = FALSE),
    model_estimated_sd_raw = vapply(raw_class, fit_class_persuader_sd,
                                    numeric(1L), USE.NAMES = FALSE),
    model_estimated_sd     = pmax(model_estimated_sd_raw, PANEL_B_SD_FLOOR,
                                  na.rm = TRUE)
  )

for (i in seq_len(nrow(canvasser_class_distributions))) {
  r <- canvasser_class_distributions[i, ]
  fallback_tag <- if (is.na(r$model_estimated_sd_raw) ||
                      r$model_estimated_sd_raw < PANEL_B_SD_FLOOR) " <-- POOLED FALLBACK" else ""
  message(sprintf("    %-25s  TE = %5.2f pp   tau_hat = %4.2f -> %4.2f pp%s",
                  r$display_label, r$group_te,
                  r$model_estimated_sd_raw, r$model_estimated_sd, fallback_tag))
}
save_rds(canvasser_class_distributions, "canvasser_class_distributions")

# =============================================================================
# 4b. S1+S2-only refits for the main-text mechanism figure (Fig 2)
# =============================================================================
# The mechanism section ("Under what conditions do humans rival AI?") is
# narrated chronologically over Studies 1 and 2 only -- Study 3 (the
# Professional Canvasser study) is introduced one section later. Every
# Fig 2 input must therefore come from a pooled S1+S2 fit; the
# canvasser-inclusion robustness check (lives in the SI) reuses the
# S1-S3 outputs above.
#
# Outputs (under output/results/, consumer after `->`):
#   pooled_s1_s2_lmm.rds                    -> body-text mechanism contrasts
#                                              + per-persuader BLUPs (Fig 2b)
#   limits_per_persuader_re.rds               -> Fig 2 panel B persuader-level
#                                              shrinkage estimates (BLUPs)
#                                              for the "N estimates exceeded
#                                              AI" claim
#   limits_ai_reference.rds           -> mean of AI(S1) + AI(S2) per-
#                                              study contrasts (red dashed
#                                              line in Fig 2b)
#   limits_class_distributions.rds    -> Fig 2 panel B normal curves
#                                              (4 non-canvasser classes only;
#                                              mu, tau from S1+S2 fits)
#   mechanism_te_inputs.rds              -> per-(model x study) TE from
#                                              the S1+S2 m_ai_sep fit; used
#                                              by Fig 3b (no S3 AI points)

message("\n---- 4b. S1+S2-only refits (mechanism figure) ----")

pooled_df_s1_s2 <- pooled_df |>
  dplyr::filter(study %in% c("study_1", "study_2")) |>
  dplyr::mutate(study = droplevels(study),
                group = droplevels(group))

fig1_df_s1_s2 <- fig1_df |>
  dplyr::filter(study %in% c("study_1", "study_2")) |>
  dplyr::mutate(study = droplevels(study))

# 4b.i Pooled S1+S2 body-text model
pooled_s1_s2_lmm <- fit_pooled_lmm(pooled_df_s1_s2,
                                   group_col = "group",
                                   label     = "pooled_s1_s2_lmm")
save_rds(pooled_s1_s2_lmm, "pooled_s1_s2_lmm")

# 4b.ii Per-persuader BLUPs for Fig 2b's "318 estimates" claim.
limits_per_persuader_re <- tibble::as_tibble(
  lme4::ranef(pooled_s1_s2_lmm)$persuader,
  rownames = "persuader"
) |>
  dplyr::rename(re_intercept = `(Intercept)`) |>
  dplyr::left_join(
    pooled_df_s1_s2 |> dplyr::distinct(persuader, group, study),
    by = "persuader"
  )
save_rds(limits_per_persuader_re, "limits_per_persuader_re")

# 4b.iii Per-(group x study) TE for the S1+S2 universe (used to derive
#         the per-class TE means for Panel D's distributions).
m_panel_a_s1_s2 <- fit_pooled_lmm(fig1_df_s1_s2,
                                  group_col = "group_by_study",
                                  ref       = "Control",
                                  label     = "m_panel_a_s1_s2")
limits_panel_effects_s1_s2 <- tidy_te_vs_control(m_panel_a_s1_s2,
                                                 "group_by_study",
                                                 ref = "Control")

# 4b.iv AI reference for Panel D's red dashed line: mean of the two
#       AI(Info-prompted) per-study contrasts from m_panel_a_s1_s2.
ai_x_study_te_s1_s2 <- limits_panel_effects_s1_s2 |>
  dplyr::filter(grepl("^AI \\(Info-prompted\\) S[12]$", group))
limits_ai_reference <- mean(ai_x_study_te_s1_s2$estimate)
save_rds(limits_ai_reference, "limits_ai_reference")

# 4b.v  Per-class normal densities for Panel D (4 non-canvasser classes).
#       mu comes from the S1+S2 fit above; tau comes from the per-class
#       REML fit on pooled_df_s1_s2.
HUMAN_CLASSES_S1_S2 <- c(
  random_lay_person     = "Random Layperson",
  elite_lay_person      = "Elite Layperson",
  elite_debater         = "Elite Debater",
  coached_elite_debater = "Coached Elite Debater"
)
PANEL_D_SD_FLOOR <- as.numeric(
  attr(lme4::VarCorr(pooled_s1_s2_lmm)$persuader, "stddev")
)

message(sprintf("  per-class persuader SDs S1+S2 (raw -> displayed; floored at pooled tau_hat = %.2f pp):",
                PANEL_D_SD_FLOOR))

limits_class_distributions <- tibble::tibble(
  raw_class     = names(HUMAN_CLASSES_S1_S2),
  display_label = unname(HUMAN_CLASSES_S1_S2)
) |>
  dplyr::mutate(
    group_te = vapply(display_label, function(lbl) {
      v <- limits_panel_effects_s1_s2 |>
        dplyr::filter(group == lbl) |>
        dplyr::pull(estimate)
      if (length(v) == 0L) NA_real_ else v
    }, numeric(1L), USE.NAMES = FALSE),
    model_estimated_sd_raw = vapply(raw_class,
                                    function(rc) fit_class_persuader_sd(rc, data = pooled_df_s1_s2),
                                    numeric(1L), USE.NAMES = FALSE),
    model_estimated_sd     = pmax(model_estimated_sd_raw, PANEL_D_SD_FLOOR,
                                  na.rm = TRUE)
  )

for (i in seq_len(nrow(limits_class_distributions))) {
  r <- limits_class_distributions[i, ]
  fallback_tag <- if (is.na(r$model_estimated_sd_raw) ||
                      r$model_estimated_sd_raw < PANEL_D_SD_FLOOR) " <-- POOLED FALLBACK" else ""
  message(sprintf("    %-25s  TE = %5.2f pp   tau_hat = %4.2f -> %4.2f pp%s",
                  r$display_label, r$group_te,
                  r$model_estimated_sd_raw, r$model_estimated_sd, fallback_tag))
}
save_rds(limits_class_distributions, "limits_class_distributions")

# 4b.vi Per-(model x study) TE for the facts scatter (Panel C), drawn
#       from m_ai_sep refit on S1+S2 only. The S3 AI models (Claude 4.6,
#       GPT-5.4, Grok 4.20) drop out of the panel because they were only
#       run in Study 3.
m_ai_sep_s1_s2 <- fit_pooled_lmm(fig1_df_s1_s2,
                                 group_col = "group_ai_separate",
                                 ref       = "Control",
                                 label     = "m_ai_sep_s1_s2")
mechanism_te_inputs <- tidy_te_vs_control(m_ai_sep_s1_s2,
                                             "group_ai_separate",
                                             ref = "Control")
save_rds(mechanism_te_inputs, "mechanism_te_inputs")

# Audit summary of the S1+S2 mechanism inputs
message(sprintf("  Panel D AI reference (avg of AI S1 + AI S2 contrasts): %.2f pp",
                limits_ai_reference))
message(sprintf("  Per-persuader BLUPs (S1+S2): %d rows, %d unique humans (no canvasser)",
                nrow(limits_per_persuader_re |> dplyr::filter(!group %in% c("ai", "control"))),
                dplyr::n_distinct(
                  limits_per_persuader_re |>
                    dplyr::filter(!group %in% c("ai", "control")) |>
                    dplyr::pull(persuader)
                )))

# =============================================================================
# 5. Robustness checks (SI)
# =============================================================================
# Two alternative specifications of the pooled body-text model, refit on
# pooled_df. Each tidy contrast table feeds one column of tabS4.
#   * `robustness_with_study_fe`   -- adds `+ study` as a fixed effect
#   * `robustness_ai_as_singleton` -- gives each AI conversation its own
#                                     singleton persuader ID instead of
#                                     grouping by AI model

message("\n---- 5. robustness checks (SI) ----")

# 5a. Main spec contrasts (column 1 of tabS4). Re-extracted from the
#     already-fit pooled_s1_s3_lmm so all three columns use the same
#     emmeans pipeline.
robustness_main_spec <- tidy_te_vs_control(pooled_s1_s3_lmm, "group",
                                           ref = "control")
save_rds(robustness_main_spec, "robustness_main_spec")

# 5b. + study fixed effect.
m_with_study_fe <- fit_pooled_lmm(pooled_df,
                                  group_col = "group",
                                  extra_fe  = "study",
                                  label     = "robustness_with_study_fe")
robustness_with_study_fe <- tidy_te_vs_control(m_with_study_fe, "group",
                                               ref = "control")
save_rds(robustness_with_study_fe, "robustness_with_study_fe")

# 5c. AI rows as singleton persuader IDs (each AI conversation independent
#     instead of grouped by AI model). Humans + controls unchanged.
pooled_df_ai_singleton <- pooled_df |>
  dplyr::mutate(
    persuader = dplyr::if_else(
      group == "ai",
      paste0("solo_", persuadee_session),
      persuader
    )
  )
m_ai_as_singleton <- fit_pooled_lmm(pooled_df_ai_singleton,
                                    group_col = "group",
                                    label     = "robustness_ai_as_singleton")
robustness_ai_as_singleton <- tidy_te_vs_control(m_ai_as_singleton, "group",
                                                 ref = "control")
save_rds(robustness_ai_as_singleton, "robustness_ai_as_singleton")

# =============================================================================
# 6. Audit log
# =============================================================================
# One pass over the saved tibbles, printing a summary to stdout: the
# body-text class TEs, the Panel B class means and SDs, the AI reference
# scalar, and the worst-case shift in TE estimate / CI width under each
# robustness check.

message("\n---- 6. audit log ----")

display_class <- function(raw) {
  out <- unname(PERSUADER_CLASS_LABELS[raw])
  ifelse(is.na(out), raw, out)
}

message("  body-text class TEs (pooled_s1_s3_lmm vs control):")
for (i in seq_len(nrow(robustness_main_spec))) {
  r <- robustness_main_spec[i, ]
  message(sprintf("    %-25s  %+5.2f pp  [%+5.2f, %+5.2f]",
                  display_class(r$group),
                  r$estimate, r$lower_ci, r$upper_ci))
}

message(sprintf("  Panel B AI reference (avg of 3 AI x study contrasts): %.2f pp",
                canvasser_ai_reference))

# Robustness deltas: maximum |delta| in TE estimate and CI width across
# classes, vs the main spec.
spec_delta <- function(other, label) {
  joined <- dplyr::inner_join(
    robustness_main_spec |> dplyr::transmute(group, est_main = estimate,
                                             ciw_main = upper_ci - lower_ci),
    other |> dplyr::transmute(group, est = estimate,
                              ciw = upper_ci - lower_ci),
    by = "group"
  ) |>
    dplyr::mutate(d_est = est - est_main,
                  d_ciw = ciw - ciw_main)
  worst_est <- joined |> dplyr::slice_max(abs(d_est), n = 1L, with_ties = FALSE)
  worst_ciw <- joined |> dplyr::slice_max(abs(d_ciw), n = 1L, with_ties = FALSE)
  message(sprintf("    %-22s  max |delta_TE| = %+.2f pp (%s);  max |delta_CIwidth| = %+.2f pp (%s)",
                  label,
                  worst_est$d_est, display_class(worst_est$group),
                  worst_ciw$d_ciw, display_class(worst_ciw$group)))
}

message("  robustness deltas vs main spec:")
spec_delta(robustness_with_study_fe, "+ study FE")
spec_delta(robustness_ai_as_singleton, "AI as singleton RE")

message("\n  done.")
