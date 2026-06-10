# si_11_repeat_participation.R
# =============================================================================
# Repeated-participation due-diligence analyses.
#
# Persuadees could complete multiple sessions (up to five in Studies 1-3,
# each on a new issue) and the same Prolific IDs recur across studies. This
# script documents that multiplicity and shows the headline pooled result is
# not an artefact of it.
#
# Outputs (under output/results/, consumer after `->`):
#   repeat_descriptives.rds         -> tab_repeat_participation (sessions panel)
#   repeat_overlap.rds              -> tab_repeat_participation (overlap panel)
#                                      + 99_extract_numbers (cross-study counts)
#   repeat_issue_check.rds          -> 99_extract_numbers (never-same-issue check)
#   repeat_first_session.rds        -> tab_repeat_robustness (first-session column)
#   repeat_first_session_ai_minus.rds -> 99_extract_numbers (AI-minus-human, S1)
#   repeat_carryover.rds            -> tab_repeat_robustness (attempt-slope column)
#                                      + 99_extract_numbers (interaction LRT)
#   repeat_summary.rds              -> 99_extract_numbers (headline scalars)
#
# Scope:
#   * Descriptives + cross-study overlap span all five studies (S0 the
#     selection tournament, S1-S3 attitude studies, S4 donations).
#   * The never-same-issue verification covers the issue-varying studies.
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
# 4. Verification: never randomized to the same issue twice
# =============================================================================
# For each issue-varying study and persuadee, the maximum number of sessions
# on any single issue should be 1 (the design assigned returning persuadees a
# new, previously-unseen stance each time). We report, per study, the number
# of (persuadee, issue) pairs seen more than once (expected 0) and the max
# distinct issues a persuadee saw.
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

message("  never-same-issue verification:")
print(repeat_issue_check)

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
  n_duplicate_issue_s1s3 = repeat_issue_check |>
    dplyr::filter(study %in% c("study_1", "study_2", "study_3")) |>
    dplyr::pull(n_duplicate_issue_pairs) |>
    sum()
)
save_rds(repeat_summary, "repeat_summary")

message(sprintf(
  "  S1-S3: %d unique persuadees, %d (%.1f%%) multi-session, %d duplicate-issue sessions",
  repeat_summary$n_persuadees_s1s3, repeat_summary$n_multi_session,
  repeat_summary$pct_multi_session, repeat_summary$n_duplicate_issue_s1s3))

message("\n  done.")
