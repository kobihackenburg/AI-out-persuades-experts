# si_11_repeat_participation.R
# =============================================================================
# Repeated-participation due-diligence analyses.
#
# Persuadees could complete multiple sessions (up to five in Studies 1-3,
# each on a new issue within that study) and the same Prolific IDs recur
# across studies. The platform's never-same-issue rule applied within a study
# only, so persuadees who took part in several of Studies 1-3 could discuss
# the same issue again. This script documents that multiplicity and shows the
# headline pooled result is not an artefact of it.
#
# Outputs (under output/results/, consumer after `->`):
#   repeat_descriptives.rds         -> tab_repeat_participation (sessions panel)
#   repeat_overlap.rds              -> tab_repeat_participation (overlap panel)
#                                      + 99_extract_numbers (cross-study counts)
#   repeat_issue_check.rds          -> 99_extract_numbers (within-study check)
#   repeat_issue_extent.rds         -> 99_extract_numbers (same-issue repeats
#                                      across S1-S3)
#   repeat_first_session.rds        -> tab_repeat_robustness (first-session column)
#   repeat_first_session_ai_minus.rds -> 99_extract_numbers (AI-minus-human, S1)
#   repeat_ai_minus_human.rds       -> tab_repeat_robustness (AI-minus-human table)
#   repeat_carryover.rds            -> tab_repeat_robustness (attempt-slope column)
#                                      + 99_extract_numbers (interaction LRT)
#   repeat_summary.rds              -> 99_extract_numbers (headline scalars)
#
# Scope:
#   * Descriptives + cross-study overlap span all five studies (S0 the
#     selection tournament, S1-S3 attitude studies, S4 donations).
#   * The within-study never-same-issue check covers the issue-varying
#     studies; the cross-study check and the AI-minus-human refits cover the
#     pooled S1-S3 attitude studies.
#   * First-session-only and attempt x treatment carryover refits use the
#     pooled S1-S3 attitude frame only (Study 4 is single-session by design,
#     so it cannot contribute to a repeated-measures robustness check).
# =============================================================================

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))
source(here::here("code/analysis/_lmm_helpers.R"))

message("\n==== si_11_repeat_participation.R ====")

ALL_STUDIES <- c("study_0", "study_1", "study_2", "study_3", "study_4")
ISSUE_STUDIES <- c("study_0", "study_1", "study_2", "study_3")  # S4 has one target
POOLED_STUDIES <- c("study_1", "study_2", "study_3")

lmm_control <- lme4::lmerControl(optimizer = "bobyqa",
                                 optCtrl   = list(maxfun = 100000))

# =============================================================================
# 1. Per-study analytic samples (complete-case persuadee sessions)
# =============================================================================
# One row per retained persuadee conversation. The outcome that defines a
# "complete case" differs by study: post-treatment attitude in S0-S3, a
# non-missing donation in S4. We keep only the columns needed for the
# session-count, overlap, and issue-uniqueness summaries.
studies <- load_studies(ALL_STUDIES)

analytic_sessions <- function(df, study_id) {
  if (study_id == "study_4") {
    cc <- df |>
      complete_case_filter(require_attitudes = FALSE,
                           label = paste("si_11", study_id)) |>
      dplyr::filter(!is.na(donation_amount))
  } else {
    cc <- df |>
      complete_case_filter(require_attitudes = TRUE,
                           label = paste("si_11", study_id))
  }
  cc |>
    dplyr::transmute(
      study      = study_id,
      persuadee  = persuadee_id,
      issue      = as.character(issue_id)
    )
}

sessions <- purrr::imap_dfr(studies, analytic_sessions)

# =============================================================================
# 2. Sessions-per-persuadee distribution (all five studies)
# =============================================================================
# For each study, count conversations per persuadee, then bucket persuadees by
# how many sessions they completed (1, 2, 3, 4, 5+).
session_counts <- sessions |>
  dplyr::count(study, persuadee, name = "n_sessions")

repeat_descriptives <- session_counts |>
  dplyr::mutate(
    bucket = dplyr::case_when(
      n_sessions >= 5L ~ "5+",
      TRUE             ~ as.character(n_sessions)
    )
  ) |>
  dplyr::count(study, bucket, name = "n_persuadees") |>
  tidyr::pivot_wider(names_from = bucket, values_from = n_persuadees,
                     values_fill = 0L) |>
  dplyr::left_join(
    session_counts |>
      dplyr::group_by(study) |>
      dplyr::summarise(
        n_persuadees_total = dplyr::n(),
        n_sessions_total   = sum(n_sessions),
        mean_sessions      = mean(n_sessions),
        max_sessions       = max(n_sessions),
        .groups = "drop"
      ),
    by = "study"
  ) |>
  dplyr::arrange(match(study, ALL_STUDIES))

# Guarantee every bucket column exists even if some study never hit it.
for (b in c("1", "2", "3", "4", "5+")) {
  if (!b %in% names(repeat_descriptives)) repeat_descriptives[[b]] <- 0L
}
repeat_descriptives <- repeat_descriptives |>
  dplyr::select(study, `1`, `2`, `3`, `4`, `5+`,
                n_persuadees_total, n_sessions_total, mean_sessions,
                max_sessions)

save_rds(repeat_descriptives, "repeat_descriptives")

# The intended per-study maximum was five sessions; a minority completed more.
repeat_session_caps <- session_counts |>
  dplyr::group_by(study) |>
  dplyr::summarise(
    median_sessions = stats::median(n_sessions),
    n_over_five     = sum(n_sessions > 5L),
    pct_over_five   = 100 * mean(n_sessions > 5L),
    .groups = "drop"
  )
save_rds(repeat_session_caps, "repeat_session_caps")

message("  sessions-per-persuadee distribution:")
print(repeat_descriptives)

# =============================================================================
# 3. Cross-study persuadee overlap (all five pools)
# =============================================================================
# Membership = the set of unique persuadee IDs in each study's analytic
# sample. We report (a) how many unique persuadees appear in exactly k of the
# five pools, and (b) a 5x5 matrix of shared-persuadee counts (diagonal =
# pool size, off-diagonal = persuadees common to both studies).
members <- lapply(ALL_STUDIES, function(s) {
  unique(sessions$persuadee[sessions$study == s])
})
names(members) <- ALL_STUDIES

# (a) membership-count summary
pool_membership <- sessions |>
  dplyr::distinct(study, persuadee) |>
  dplyr::count(persuadee, name = "n_pools")

membership_counts <- pool_membership |>
  dplyr::count(n_pools, name = "n_persuadees") |>
  dplyr::arrange(n_pools)

# (b) 5x5 overlap matrix
overlap_matrix <- outer(
  ALL_STUDIES, ALL_STUDIES,
  Vectorize(function(a, b) length(intersect(members[[a]], members[[b]])))
)
dimnames(overlap_matrix) <- list(ALL_STUDIES, ALL_STUDIES)
overlap_matrix_df <- tibble::as_tibble(overlap_matrix, rownames = "study")

repeat_overlap <- list(
  membership_counts = membership_counts,
  overlap_matrix    = overlap_matrix_df,
  n_unique_total    = dplyr::n_distinct(sessions$persuadee),
  n_in_multiple     = sum(pool_membership$n_pools > 1L),
  # persuadees shared among the three pooled attitude studies (S1-S3)
  n_cross_s1s2s3    = {
    s123 <- pool_membership |>
      dplyr::inner_join(
        sessions |>
          dplyr::filter(study %in% c("study_1", "study_2", "study_3")) |>
          dplyr::distinct(study, persuadee) |>
          dplyr::count(persuadee, name = "n_pools_123"),
        by = "persuadee"
      )
    sum(s123$n_pools_123 > 1L)
  }
)
save_rds(repeat_overlap, "repeat_overlap")

message("  cross-study overlap (membership counts):")
print(membership_counts)
message("  overlap matrix:")
print(overlap_matrix)

# =============================================================================
# 4. Verification: never randomized to the same issue twice within a study
# =============================================================================
# For each issue-varying study and persuadee, the maximum number of sessions
# on any single issue should be 1 (within a study, the design assigned
# returning persuadees a new, previously-unseen stance each time). We report,
# per study, the number of (persuadee, issue) pairs seen more than once
# (expected 0) and the max distinct issues a persuadee saw.
repeat_issue_check <- sessions |>
  dplyr::filter(study %in% ISSUE_STUDIES, !is.na(issue), issue != "NA") |>
  dplyr::group_by(study, persuadee, issue) |>
  dplyr::summarise(n_on_issue = dplyr::n(), .groups = "drop") |>
  dplyr::group_by(study) |>
  dplyr::summarise(
    n_persuadees             = dplyr::n_distinct(persuadee),
    n_duplicate_issue_pairs  = sum(n_on_issue > 1L),
    max_sessions_same_issue  = max(n_on_issue),
    max_distinct_issues      = max(tapply(n_on_issue, persuadee, length)),
    .groups = "drop"
  ) |>
  dplyr::arrange(match(study, ISSUE_STUDIES))

save_rds(repeat_issue_check, "repeat_issue_check")

message("  never-same-issue verification (within study):")
print(repeat_issue_check)

# =============================================================================
# 4b. Same-issue repeats across Studies 1-3
# =============================================================================
# The rule above did not carry across studies, so a persuadee could meet the
# same issue again in a later study. One row per retained S1-S3 conversation,
# with the persuadee's sessions ordered across studies (`attempt_global`;
# `attempt` restarts at 1 in each study) and each conversation's exposure
# number on its issue. `persuader` and `attempt` are coded as in
# 01_main_attitudes.R; `group3` pools the five human classes and keeps
# Constrained AI separate from (unconstrained) AI.
issue_df <- purrr::imap_dfr(studies[POOLED_STUDIES], ~ dplyr::mutate(.x, study = .y)) |>
  complete_case_filter(label = "si_11 cross-study issues") |>
  dplyr::transmute(
    study, session_completed_at,
    persuadee = persuadee_id,
    persuader = dplyr::case_when(
      !is.na(persuader_id)                            ~ persuader_id,
      persuader_class == "ai" & !is.na(ai_model_full) ~ paste0("ai_", ai_model_full),
      TRUE                                            ~ paste0("solo_", persuadee_session)
    ),
    issue = issue_id, pre = pre_attitude, post = post_attitude,
    attempt = dplyr::coalesce(as.integer(persuadee_attempt_number), 1L),
    group = dplyr::case_when(
      persuader_class == "ai" & dplyr::coalesce(ai_constrained, FALSE) ~ "ai_constrained",
      TRUE ~ as.character(persuader_class)
    ),
    group3 = dplyr::case_when(group %in% c("control", "ai", "ai_constrained") ~ group,
                              TRUE ~ "human")
  ) |>
  dplyr::arrange(persuadee, study, session_completed_at) |>
  dplyr::group_by(persuadee) |>
  dplyr::mutate(attempt_global = dplyr::row_number()) |>
  dplyr::group_by(persuadee, issue) |>
  dplyr::mutate(n_on_issue = dplyr::n(), exposure = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::mutate(repeated_issue = n_on_issue > 1L, repeat_exposure = exposure > 1L)

repeat_pairs <- issue_df |>
  dplyr::filter(repeated_issue) |>
  dplyr::group_by(persuadee, issue) |>
  dplyr::summarise(studies      = paste(sort(unique(study)), collapse = "+"),
                   within_study = dplyr::n_distinct(study) < dplyr::n(),
                   same_class   = dplyr::n_distinct(group3) == 1L,
                   .groups = "drop")

repeat_issue_extent <- list(
  n_conversations              = nrow(issue_df),
  n_conv_repeated_issue        = sum(issue_df$repeated_issue),
  n_conv_repeat_exposure       = sum(issue_df$repeat_exposure),
  n_persuadees                 = dplyr::n_distinct(issue_df$persuadee),
  n_persuadees_repeated_issue  = dplyr::n_distinct(issue_df$persuadee[issue_df$repeated_issue]),
  n_pairs                      = nrow(repeat_pairs),
  n_pairs_across_studies       = sum(!repeat_pairs$within_study),
  n_pairs_within_study         = sum(repeat_pairs$within_study),
  n_pairs_same_class           = sum(repeat_pairs$same_class),
  max_sessions_same_issue      = max(issue_df$n_on_issue),
  pairs_by_studies             = dplyr::count(repeat_pairs, studies, name = "n_pairs")
)
save_rds(repeat_issue_extent, "repeat_issue_extent")

message(sprintf(paste0(
  "  same-issue repeats across S1-S3: %d of %d conversations, %d of %d ",
  "persuadees, %d (persuadee, issue) pairs (%d across studies, %d within)"),
  repeat_issue_extent$n_conv_repeated_issue, repeat_issue_extent$n_conversations,
  repeat_issue_extent$n_persuadees_repeated_issue, repeat_issue_extent$n_persuadees,
  repeat_issue_extent$n_pairs, repeat_issue_extent$n_pairs_across_studies,
  repeat_issue_extent$n_pairs_within_study))
print(repeat_issue_extent$pairs_by_studies)

# =============================================================================
# 5. First-session-only robustness (pooled S1-S3)
# =============================================================================
# Refit the headline pooled model on each persuadee's first session within a
# study (attempt == 1), so within-study repeated measurement is removed. The
# constant `attempt` covariate is dropped; crossed random intercepts and all
# other terms match the headline specification.
pooled_df <- load_rds("pooled_df")

first_df <- pooled_df |>
  dplyr::filter(attempt == 1L) |>
  dplyr::mutate(group = droplevels(group), issue = droplevels(factor(issue)))

message(sprintf("  first-session frame: %d rows, %d persuaders, %d persuadees",
                nrow(first_df),
                dplyr::n_distinct(first_df$persuader),
                dplyr::n_distinct(first_df$persuadee)))

m_first <- lme4::lmer(
  post ~ pre + issue + group + (1 | persuader) + (1 | persuadee),
  data    = first_df,
  REML    = TRUE,
  control = lme4::lmerControl(optimizer = "bobyqa",
                              optCtrl = list(maxfun = 100000))
)
repeat_first_session <- tidy_te_vs_control(m_first, "group", ref = "control")
save_rds(repeat_first_session, "repeat_first_session")

# AI-minus-human contrasts in the first-session frame (positive = AI more
# persuasive), mirroring the headline AI-minus computation in 99_extract.
em_first <- emmeans::emmeans(m_first, ~ group, lmer.df = "asymptotic")
repeat_first_session_ai_minus <- emmeans::contrast(em_first,
                                                   method = "trt.vs.ctrl",
                                                   ref = "ai") |>
  summary(infer = TRUE, adjust = "none") |>
  tibble::as_tibble() |>
  dplyr::mutate(
    group    = sub(" - ai$", "", as.character(contrast)),
    group    = sub("^\\((.+)\\)$", "\\1", group),
    estimate = -estimate,
    lower_ci = -asymp.UCL,
    upper_ci = -asymp.LCL,
    p_value  = p.value
  ) |>
  dplyr::filter(group != "control") |>
  dplyr::select(group, estimate, lower_ci, upper_ci, p_value)
save_rds(repeat_first_session_ai_minus, "repeat_first_session_ai_minus")

message("  first-session per-class TEs vs control:")
print(repeat_first_session)

# =============================================================================
# 5b. AI minus human persuaders under cross-study definitions (pooled S1-S3)
# =============================================================================
# AI (unconstrained) minus all human persuaders pooled, from the
# preregistered model with `group3` as the condition factor, refit
#   * on all conversations (reference),
#   * on each persuadee's first conversation on each issue,
#   * excluding every conversation on a repeated issue,
#   * with `attempt` counted across studies,
#   * on each persuadee's first session across all studies (one row per
#     persuadee, so no persuadee intercept and no attempt term),
# plus the gap in first vs repeat conversations on an issue from a
# group3 x exposure interaction model.
ai_minus_human <- function(m, spec, n) {
  em <- emmeans::emmeans(m, ~ group3, lmer.df = "asymptotic")
  lv <- as.character(em@levels$group3)
  w  <- stats::setNames(numeric(length(lv)), lv)
  w["ai"] <- 1; w["human"] <- -1
  emmeans::contrast(em, list(w)) |>
    summary(infer = TRUE) |>
    tibble::as_tibble() |>
    dplyr::transmute(spec = spec, estimate, lower_ci = asymp.LCL,
                     upper_ci = asymp.UCL, p_value = p.value, n = n)
}

first_session_global_df <- issue_df |>
  dplyr::filter(attempt_global == 1L) |>
  dplyr::mutate(group3 = forcats::fct_relevel(factor(group3), "control"))
m_first_global <- lme4::lmer(post ~ pre + issue + group3 + (1 | persuader),
                             data = first_session_global_df, REML = TRUE,
                             control = lmm_control)

exposure_df <- issue_df |>
  dplyr::mutate(group3 = forcats::fct_relevel(factor(group3), "control"),
                exposure_type = factor(ifelse(repeat_exposure, "repeat", "first")))
m_exposure <- lme4::lmer(
  post ~ pre + issue + attempt + group3 * exposure_type + (1 | persuader) + (1 | persuadee),
  data = exposure_df, REML = TRUE, control = lmm_control
)
em_exposure <- emmeans::emmeans(m_exposure, ~ group3 | exposure_type, lmer.df = "asymptotic")
w_exposure <- stats::setNames(numeric(nlevels(exposure_df$group3)), levels(exposure_df$group3))
w_exposure["ai"] <- 1; w_exposure["human"] <- -1
gap_by_exposure <- emmeans::contrast(em_exposure, list(ai_minus_human = w_exposure),
                                     by = "exposure_type")
tidy_gap <- function(x) {
  summary(x, infer = TRUE) |>
    tibble::as_tibble() |>
    dplyr::mutate(lower_ci = asymp.LCL, upper_ci = asymp.UCL,
                  p_value = p.value, n = NA_integer_)
}
gap_rows <- dplyr::bind_rows(
  tidy_gap(gap_by_exposure) |>
    dplyr::mutate(spec = paste0("exposure_", exposure_type),
                  n    = as.integer(table(exposure_df$exposure_type)[as.character(exposure_type)])),
  tidy_gap(emmeans::contrast(gap_by_exposure, method = "revpairwise", by = NULL)) |>
    dplyr::mutate(spec = "exposure_repeat_minus_first")
) |>
  dplyr::select(spec, estimate, lower_ci, upper_ci, p_value, n)

repeat_ai_minus_human <- dplyr::bind_rows(
  ai_minus_human(fit_pooled_lmm(issue_df, "group3", label = "all"),
                 "all", nrow(issue_df)),
  ai_minus_human(fit_pooled_lmm(dplyr::filter(issue_df, !repeat_exposure), "group3",
                                label = "first conversation on each issue"),
                 "first_on_issue", sum(!issue_df$repeat_exposure)),
  ai_minus_human(fit_pooled_lmm(dplyr::filter(issue_df, !repeated_issue), "group3",
                                label = "no repeated issues"),
                 "no_repeated_issues", sum(!issue_df$repeated_issue)),
  ai_minus_human(fit_pooled_lmm(dplyr::mutate(issue_df, attempt = attempt_global), "group3",
                                label = "attempt across studies"),
                 "attempt_global", nrow(issue_df)),
  ai_minus_human(m_first_global, "first_session_global", nrow(first_session_global_df)),
  gap_rows
)
save_rds(repeat_ai_minus_human, "repeat_ai_minus_human")

message("  AI minus human persuaders (pooled S1-S3):")
print(repeat_ai_minus_human)

# =============================================================================
# 6. Carryover / learning / fatigue (attempt x treatment interaction)
# =============================================================================
# Does the per-class persuasive effect change across a persuadee's repeated
# sessions? We compare the additive headline model to one with a
# group x attempt interaction (ML fits, LRT) and report, per class, the
# difference in the attempt slope vs control (pp per additional session).
m_add_ml <- lme4::lmer(
  post ~ pre + issue + group + attempt + (1 | persuader) + (1 | persuadee),
  data = pooled_df, REML = FALSE,
  control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
m_int_ml <- lme4::lmer(
  post ~ pre + issue + group * attempt + (1 | persuader) + (1 | persuadee),
  data = pooled_df, REML = FALSE,
  control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
lrt <- anova(m_add_ml, m_int_ml)
p_interaction <- lrt$`Pr(>Chisq)`[2]
lrt_chisq     <- lrt$Chisq[2]
lrt_df        <- lrt$Df[2]

# REML refit for slope-difference inference.
m_int_reml <- lme4::lmer(
  post ~ pre + issue + group * attempt + (1 | persuader) + (1 | persuadee),
  data = pooled_df, REML = TRUE,
  control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
slope_diffs <- emmeans::emtrends(m_int_reml, ~ group, var = "attempt",
                                 lmer.df = "asymptotic") |>
  emmeans::contrast(method = "trt.vs.ctrl", ref = "control") |>
  summary(infer = TRUE, adjust = "none") |>
  tibble::as_tibble() |>
  dplyr::transmute(
    group    = clean_contrast_name(contrast),
    estimate = estimate,
    lower_ci = asymp.LCL,
    upper_ci = asymp.UCL,
    p_value  = p.value
  )

repeat_carryover <- list(
  slopes        = slope_diffs,
  p_interaction = p_interaction,
  lrt_chisq     = lrt_chisq,
  lrt_df        = lrt_df
)
save_rds(repeat_carryover, "repeat_carryover")

message(sprintf("  carryover LRT: chisq(%d) = %.2f, p = %.3f",
                lrt_df, lrt_chisq, p_interaction))
message("  per-class attempt-slope difference vs control (pp/session):")
print(slope_diffs)

# =============================================================================
# 7. Headline scalars for the SI prose
# =============================================================================
# Multi-session share is computed over the pooled S1-S3 attitude persuadees:
# of all unique persuadees in S1-S3, how many completed more than one
# conversation (within or across those studies).
s1s3 <- session_counts |>
  dplyr::filter(study %in% c("study_1", "study_2", "study_3")) |>
  dplyr::group_by(persuadee) |>
  dplyr::summarise(n_sessions = sum(n_sessions), .groups = "drop")

repeat_summary <- list(
  n_persuadees_s1s3   = nrow(s1s3),
  n_multi_session     = sum(s1s3$n_sessions > 1L),
  pct_multi_session   = 100 * mean(s1s3$n_sessions > 1L),
  max_sessions_s1s3   = max(s1s3$n_sessions),
  n_duplicate_issue_s1s3 = repeat_issue_extent$n_pairs,
  n_duplicate_issue_within_study_s1s3 = repeat_issue_check |>
    dplyr::filter(study %in% POOLED_STUDIES) |>
    dplyr::pull(n_duplicate_issue_pairs) |>
    sum()
)
save_rds(repeat_summary, "repeat_summary")

message(sprintf(paste0(
  "  S1-S3: %d unique persuadees, %d (%.1f%%) multi-session, %d repeated ",
  "(persuadee, issue) pairs (%d within a study)"),
  repeat_summary$n_persuadees_s1s3, repeat_summary$n_multi_session,
  repeat_summary$pct_multi_session, repeat_summary$n_duplicate_issue_s1s3,
  repeat_summary$n_duplicate_issue_within_study_s1s3))

message("\n  done.")
