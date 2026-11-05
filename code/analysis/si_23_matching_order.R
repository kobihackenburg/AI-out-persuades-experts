# si_23_matching_order.R
# =============================================================================
# Matchmaking selection, the wait for a human partner, and human persuader
# order effects (Studies 1-3).
#
# Condition was randomized at session start, before the pre-survey. Persuadees
# assigned to a human condition then queued for a partner (4-minute timeout);
# unmatched persuadees were returned to Prolific and could re-attempt.
#
# Outputs (under output/results/, consumer after `->`):
#   matching_rates               -> tab_matching_rates
#   matching_unmatched_reappear  -> SI prose (share of unmatched persuadees
#                                   later in the analysed sample)
#   matching_balance             -> tab_matching_balance (SMDs)
#   matching_balance_tests       -> tab_matching_balance (joint LRTs)
#   matching_selection_specs     -> tab_matching_robustness
#   matching_start_gap           -> tab_matching_start_gap
#   matching_wait_models         -> tab_matching_start_gap (caption), SI prose
#   matching_wait_specs          -> tab_matching_robustness
#   persuader_load               -> tab_persuader_order
#   persuader_order              -> tab_persuader_order
#
# Inputs: data/study_{1,2,3}.jsonl via the loader, plus per-session
# timestamps from output/results/_py/si_23_timing.csv
# (code/analysis/py/si_23_timing.py).
#
# A. Selection at matchmaking
#    1. Match rate by study x condition.
#    2. Matched vs unmatched sessions in human conditions on pre-treatment
#       covariates (standardized mean differences; joint logit LRT).
#    3. Balance of the analysed sample: AI vs human conditions on the same
#       covariates.
#    4. AI - human contrast (pooled LMM) adjusting for all pre-treatment
#       covariates.
#    5. Same contrast restricted to clock hours in which human persuaders were
#       holding conversations, with and without a study-date-hour fixed effect
#       (matching depended on who was online, so the human sample is selected
#       on time of participation).
# B. Waiting for a human partner
#    start_gap = conversation start - session start (pre-survey + queue).
#    1. Distribution by study x condition.
#    2. Within human conditions: does a longer start gap predict less
#       persuasion or less enjoyment?
#    3. AI - human restricted to human conversations whose start gap was no
#       longer than the median AI start gap in the same study.
# C. Persuader order
#    Order of each conversation within a persuader's own sequence (overall and
#    within a sitting; a new sitting starts after a > 30-minute break).
#    Within-persuader models (persuader fixed effects) of persuasion on order.
# =============================================================================

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))
source(here::here("code/analysis/_lmm_helpers.R"))

message("\n==== si_23_matching_order.R ====")

lmm_control <- lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

COVARIATES <- c("pre_attitude", "age", "female", "degree", "ideology_coded", "political_knowledge_n",
                "dogmatism", "ai_trust", "pre_issue_knowledge", "pre_importance")

ai_minus_human <- function(m, label) {
  em <- emmeans::emmeans(m, ~ group3, lmer.df = "asymptotic")
  lv <- as.character(em@levels$group3)
  v <- setNames(numeric(length(lv)), lv); v["ai"] <- 1; v["human"] <- -1
  emmeans::contrast(em, list(v)) |> summary(infer = TRUE) |> tibble::as_tibble() |>
    dplyr::transmute(spec = label, estimate, lower_ci = asymp.LCL, upper_ci = asymp.UCL, p_value = p.value)
}

smd_table <- function(data, by, label) {
  purrr::map_dfr(COVARIATES, function(v) {
    x <- data[[v]]; g <- data[[by]]
    a <- x[g & !is.na(x)]; b <- x[!g & !is.na(x)]
    tibble::tibble(comparison = label, covariate = v, mean_1 = mean(a), mean_0 = mean(b),
                   smd = (mean(a) - mean(b)) / sqrt((stats::var(a) + stats::var(b)) / 2),
                   n_1 = length(a), n_0 = length(b))
  })
}

# Missing covariates are median-imputed so both models are fit on the same rows.
joint_logit <- function(data, by, label, extra = "study") {
  dd <- data |> dplyr::mutate(dplyr::across(dplyr::all_of(COVARIATES),
                                            ~ dplyr::coalesce(., stats::median(., na.rm = TRUE))))
  f1 <- stats::as.formula(paste(by, "~", paste(c(COVARIATES, extra), collapse = " + ")))
  f0 <- stats::as.formula(paste(by, "~", paste(extra, collapse = " + ")))
  m1 <- stats::glm(f1, data = dd, family = stats::binomial()); m0 <- stats::glm(f0, data = dd, family = stats::binomial())
  lr <- stats::anova(m0, m1, test = "LRT")
  tibble::tibble(test = label, chisq = lr$Deviance[2], df = lr$Df[2], p_value = lr$`Pr(>Chi)`[2], n = nrow(dd))
}

# =============================================================================
# Data
# =============================================================================
timing <- readr::read_csv(file.path(RESULTS_PY, "si_23_timing.csv"), show_col_types = FALSE) |>
  dplyr::rename(session_start_ts = session_started_at)

studies <- load_studies(c("study_1", "study_2", "study_3"))
raw <- purrr::imap_dfr(studies, ~ dplyr::mutate(.x, study = .y)) |>
  dplyr::left_join(timing, by = "row_id") |>
  dplyr::mutate(
    matched = dplyr::coalesce(as.logical(successfully_matched), FALSE),
    group = dplyr::case_when(
      persuader_class == "ai" & dplyr::coalesce(ai_constrained, FALSE) ~ "ai_constrained",
      TRUE ~ as.character(persuader_class)
    ),
    group3 = dplyr::case_when(group %in% c("control", "ai", "ai_constrained") ~ group, TRUE ~ "human"),
    human_arm = group3 == "human",
    female = dplyr::if_else(is.na(gender), NA_real_, as.numeric(grepl("^female|^woman", tolower(gender)))),
    degree = dplyr::if_else(is.na(education), NA_real_,
                            as.numeric(grepl("bachelor|degree|master|doctor|phd|postgrad|undergrad", tolower(education)))),
    age = as.numeric(age)
  )
message(sprintf("  S1-3 sessions: %d; matched %d", nrow(raw), sum(raw$matched)))

cc <- raw |>
  complete_case_filter(label = "si_23") |>
  dplyr::mutate(
    persuadee = persuadee_id,
    persuader = dplyr::case_when(
      !is.na(persuader_id)                            ~ persuader_id,
      persuader_class == "ai" & !is.na(ai_model_full) ~ paste0("ai_", ai_model_full),
      TRUE                                            ~ paste0("solo_", persuadee_session)
    ),
    issue = issue_id, pre = pre_attitude, post = post_attitude,
    attempt = dplyr::coalesce(as.integer(persuadee_attempt_number), 1L)
  )

# =============================================================================
# A. Selection at matchmaking
# =============================================================================
matching_rates <- raw |>
  dplyr::group_by(study, group) |>
  dplyr::summarise(assigned = dplyr::n(), match_rate = mean(matched), matched = sum(matched), .groups = "drop")
save_rds(matching_rates, "matching_rates")

human_sessions <- dplyr::filter(raw, human_arm, !is.na(pre_attitude))
matching_unmatched_reappear <- human_sessions |> dplyr::filter(!matched) |>
  dplyr::summarise(unmatched_sessions = dplyr::n(),
                   unmatched_persuadees = dplyr::n_distinct(persuadee_id),
                   later_in_analysed_sample = mean(unique(persuadee_id) %in% cc$persuadee_id))
save_rds(matching_unmatched_reappear, "matching_unmatched_reappear")

matching_balance <- dplyr::bind_rows(
  smd_table(human_sessions, "matched", "human conditions: matched vs unmatched sessions"),
  smd_table(dplyr::filter(cc, group3 %in% c("ai", "human")) |> dplyr::mutate(is_ai = group3 == "ai"),
            "is_ai", "analysed sample: AI vs human conditions")
)
matching_balance_tests <- dplyr::bind_rows(
  joint_logit(dplyr::mutate(human_sessions, y = as.numeric(matched)), "y",
              "human conditions: covariates predict matching"),
  joint_logit(dplyr::filter(cc, group3 %in% c("ai", "human")) |> dplyr::mutate(y = as.numeric(group3 == "ai")),
              "y", "analysed sample: covariates predict AI vs human condition")
)
save_rds(matching_balance, "matching_balance")
save_rds(matching_balance_tests, "matching_balance_tests")

cc_cov <- cc |>
  dplyr::mutate(dplyr::across(dplyr::all_of(setdiff(COVARIATES, "pre_attitude")),
                              ~ dplyr::coalesce(., stats::median(., na.rm = TRUE))))
human_slots <- cc |> dplyr::filter(human_arm, !is.na(conv_started_at)) |>
  dplyr::distinct(slot = paste(study, format(conv_started_at, "%Y-%m-%d %H"))) |> dplyr::pull(slot)
# Control rows have no conversation timestamp; they are dated by session start.
cc_slot <- cc |>
  dplyr::mutate(slot = paste(study, format(dplyr::coalesce(conv_started_at, session_start_ts), "%Y-%m-%d %H"))) |>
  dplyr::filter(slot %in% human_slots)

matching_selection_specs <- dplyr::bind_rows(
  ai_minus_human(fit_pooled_lmm(cc, "group3", label = "main"), "preregistered spec") |>
    dplyr::mutate(n = nrow(cc)),
  ai_minus_human(fit_pooled_lmm(cc_cov, "group3", extra_fe = setdiff(COVARIATES, "pre_attitude"), label = "covariates"),
                 "+ all pre-treatment covariates") |> dplyr::mutate(n = nrow(cc_cov)),
  ai_minus_human(fit_pooled_lmm(cc_slot, "group3", label = "slots"),
                 "hours with human persuaders online") |> dplyr::mutate(n = nrow(cc_slot)),
  ai_minus_human(fit_pooled_lmm(cc_slot, "group3", extra_fe = "slot", label = "slots + FE"),
                 "hours with human persuaders online + study-date-hour FE") |> dplyr::mutate(n = nrow(cc_slot))
)
save_rds(matching_selection_specs, "matching_selection_specs")

print(matching_rates, n = Inf); print(matching_unmatched_reappear); print(matching_balance, n = Inf)
print(matching_balance_tests); print(matching_selection_specs)

# =============================================================================
# B. Waiting for a human partner
# =============================================================================
matching_start_gap <- cc |> dplyr::filter(!is.na(start_gap_min)) |>
  dplyr::group_by(study, group3) |>
  dplyr::summarise(n = dplyr::n(), median_min = stats::median(start_gap_min),
                   p25 = stats::quantile(start_gap_min, .25), p75 = stats::quantile(start_gap_min, .75),
                   .groups = "drop")
save_rds(matching_start_gap, "matching_start_gap")

# Start gap winsorized at its 99th percentile.
hum <- cc |> dplyr::filter(human_arm, !is.na(start_gap_min)) |>
  dplyr::mutate(gap = pmin(start_gap_min, stats::quantile(start_gap_min, .99)),
                group = factor(group))
matching_wait_models <- purrr::map_dfr(c(attitude = "post", enjoyment = "rating_enjoyment"), function(o) {
  dd <- hum; dd$y <- dd[[o]]
  dd <- dplyr::filter(dd, !is.na(y))
  m <- lme4::lmer(y ~ pre + issue + attempt + group + gap + (1 | persuader) + (1 | persuadee),
                  data = dd, REML = TRUE, control = lmm_control)
  b <- lme4::fixef(m)[["gap"]]; se <- sqrt(as.matrix(stats::vcov(m))["gap", "gap"])
  tibble::tibble(outcome = o, term = "per minute of start gap", estimate = b,
                 lower_ci = b - 1.96 * se, upper_ci = b + 1.96 * se, p_value = 2 * stats::pnorm(-abs(b / se)),
                 n = nrow(dd))
}, .id = NULL)
save_rds(matching_wait_models, "matching_wait_models")

ai_median_gap <- cc |> dplyr::filter(group3 == "ai", !is.na(start_gap_min)) |>
  dplyr::group_by(study) |> dplyr::summarise(ai_median = stats::median(start_gap_min))
cc_short <- cc |> dplyr::left_join(ai_median_gap, by = "study") |>
  dplyr::filter(!human_arm | (!is.na(start_gap_min) & start_gap_min <= ai_median))
matching_wait_specs <- ai_minus_human(fit_pooled_lmm(cc_short, "group3", label = "short wait"),
                                      "human conversations with start gap <= AI median") |>
  dplyr::mutate(n = nrow(cc_short), n_human = sum(cc_short$human_arm))
save_rds(matching_wait_specs, "matching_wait_specs")

print(matching_start_gap, n = Inf); print(matching_wait_models); print(matching_wait_specs)

# =============================================================================
# C. Persuader order
# =============================================================================
# Persuaders in order of first appearance, not by id, so ntile()'s tie-breaking
# does not depend on the (pseudonymised, in the public release) id labels.
ord <- cc |>
  dplyr::filter(human_arm, !is.na(persuader_id), !is.na(conv_started_at)) |>
  dplyr::arrange(factor(persuader_id, levels = unique(persuader_id)), conv_started_at) |>
  dplyr::group_by(persuader_id) |>
  dplyr::mutate(order_overall = dplyr::row_number(),
                n_convs = dplyr::n(),
                prev_end = dplyr::lag(as.POSIXct(conv_ended_at)),
                new_sitting = is.na(prev_end) |
                  as.numeric(difftime(conv_started_at, prev_end, units = "mins")) > 30,
                sitting = cumsum(new_sitting)) |>
  dplyr::group_by(persuader_id, sitting) |>
  dplyr::mutate(order_in_sitting = dplyr::row_number(), sitting_size = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::mutate(order_quartile = dplyr::ntile(order_overall / n_convs, 4),
                group = factor(group))

# Sitting sizes are summarised over conversations, not sittings.
persuader_load <- ord |>
  dplyr::group_by(group) |>
  dplyr::summarise(persuaders = dplyr::n_distinct(persuader_id), conversations = dplyr::n(),
                   convs_per_persuader_median = stats::median(table(persuader_id)),
                   convs_per_persuader_max = max(table(persuader_id)),
                   sittings_per_persuader_median = stats::median(tapply(sitting, persuader_id, max)),
                   convs_per_sitting_median = stats::median(sitting_size),
                   convs_per_sitting_p90 = stats::quantile(sitting_size, .9),
                   .groups = "drop")
save_rds(persuader_load, "persuader_load")

fit_order <- function(data, term_formula) {
  f <- stats::as.formula(paste("post ~ pre + issue + attempt +", term_formula, "+ factor(persuader_id) + (1 | persuadee)"))
  lme4::lmer(f, data = data, REML = TRUE, control = lmm_control)
}
coef_row <- function(m, term, label, n) {
  b <- lme4::fixef(m)[[term]]; se <- sqrt(as.matrix(stats::vcov(m))[term, term])
  tibble::tibble(spec = label, estimate = b, lower_ci = b - 1.96 * se, upper_ci = b + 1.96 * se,
                 p_value = 2 * stats::pnorm(-abs(b / se)), n = n)
}
ord <- ord |> dplyr::mutate(order10 = order_overall / 10, in_sitting_late = as.numeric(order_in_sitting >= 3),
                            last_quartile = as.numeric(order_quartile == 4))
persuader_order <- dplyr::bind_rows(
  coef_row(fit_order(ord, "order10"), "order10", "all human: per 10 conversations in persuader's sequence", nrow(ord)) |>
    dplyr::mutate(group = "all_human", term = "order10"),
  coef_row(fit_order(ord, "last_quartile"), "last_quartile",
           "all human: last vs first three quarters of persuader's sequence", nrow(ord)) |>
    dplyr::mutate(group = "all_human", term = "last_quartile"),
  coef_row(fit_order(ord, "in_sitting_late"), "in_sitting_late",
           "all human: 3rd+ conversation in a sitting vs 1st-2nd", nrow(ord)) |>
    dplyr::mutate(group = "all_human", term = "in_sitting_late"),
  purrr::map_dfr(levels(ord$group), function(g) {
    dg <- dplyr::filter(ord, group == g)
    dplyr::bind_rows(
      coef_row(fit_order(dg, "order10"), "order10", paste(g, ": per 10 conversations"), nrow(dg)) |>
        dplyr::mutate(term = "order10"),
      coef_row(fit_order(dg, "in_sitting_late"), "in_sitting_late", paste(g, ": 3rd+ in sitting"), nrow(dg)) |>
        dplyr::mutate(term = "in_sitting_late")
    ) |> dplyr::mutate(group = g)
  })
) |> dplyr::relocate(group, term, .after = spec)
save_rds(persuader_order, "persuader_order")

print(as.data.frame(persuader_load)); print(persuader_order, n = Inf)

message("\n  done.")
