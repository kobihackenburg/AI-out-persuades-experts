# si_13_followup.R
# =============================================================================
# Follow-up survey (durability of persuasive effects).
#
# All Studies 1-4 persuadees were re-contacted in September 2026
# (data/final_followup_data/). Studies 1-3 persuadees re-answered the three
# attitude items for each issue they had discussed; Study 4 persuadees chose
# how much of a new GBP 1 bonus to donate to Save the Children. Each outcome is
# analysed with its study's preregistered model, the follow-up outcome
# replacing the immediate one. The follow-up was not preregistered.
#
# Outputs (under output/results/, consumer after `->`):
#   followup_sample.rds         -> tab_followup_sample + 99_extract_numbers
#   followup_s4_sample.rds      -> tab_followup_sample + 99_extract_numbers
#   followup_by_class.rds       -> tab_followup_effects
#   followup_ai_vs_human.rds    -> tab_followup_effects + 99_extract_numbers
#   followup_per_study.rds      -> tab_followup_per_study
#   followup_retention.rds      -> tab_followup_retention + 99_extract_numbers
#   followup_response.rds       -> tab_followup_selection
#   followup_selection_gap.rds  -> tab_followup_selection
#   followup_robustness.rds     -> tab_followup_robustness
#   followup_s4_results.rds     -> tab_followup_study4 + 99_extract_numbers
#   followup_s4_robustness.rds  -> tab_followup_robustness
#   followup_claims.rds         -> tab_followup_claims
#
# Scope:
#   * Studies 1-3: complete-case conversations whose persuadee had only one
#     conversation on that issue (counted across studies). The follow-up asked
#     about a repeated issue once per persuadee, so that answer cannot be
#     attributed to one persuader; those blocks are not analysed.
#   * Study 4: the preregistered analysis sample of 03_donation.R (complete-case
#     persuadees with age and ideology observed).
#   * The retention-share bootstrap refits two LMMs per draw (B = 2,000), so
#     this script takes several minutes on all available cores.
# =============================================================================

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))
source(here::here("code/analysis/_lmm_helpers.R"))

message("\n==== si_13_followup.R ====")

FOLLOWUP_DATA <- file.path(DATA, "final_followup_data")
N_BOOT <- 2000L

lmm_control <- lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))

# =============================================================================
# 1. Follow-up outcomes, one row per analysed block
# =============================================================================
# Timestamps are JavaScript Date strings ("Mon Sep 21 2026 11:12:45 GMT+0000 ...").
parse_ts <- function(x) as.POSIXct(sub(" GMT.*", "", x), format = "%a %b %d %Y %H:%M:%S", tz = "UTC")

blocks <- readr::read_csv(file.path(FOLLOWUP_DATA, "followup_blocks.csv"), show_col_types = FALSE)
items  <- readr::read_csv(file.path(FOLLOWUP_DATA, "followup_items.csv"),  show_col_types = FALSE)

item_wide <- items |>
  dplyr::select(block_id, item_id, value_raw) |>
  tidyr::pivot_wider(names_from = item_id, values_from = value_raw)

# Blocks key back to the main data by "<study>:<session id>" (= row_id). The
# public release ships the (pseudonymised) row_id itself in place of those two
# columns (code/redact_data.R).
# Follow-up attitude is the mean of the three items, item 2 reverse-coded.
if (!"row_id" %in% names(blocks)) blocks$row_id <- paste0(blocks$studies, ":", blocks$session_ids)
fu_blocks <- blocks |>
  dplyr::filter(exposure_pattern == "single", !skipped) |>
  dplyr::transmute(block_id, row_id,
                   gap_days = as.numeric(difftime(parse_ts(dplyr::coalesce(completed_at, served_at)),
                                                  parse_ts(last_exposure_at), units = "days"))) |>
  dplyr::inner_join(item_wide, by = "block_id") |>
  dplyr::transmute(row_id, gap_days,
                   fu = (post_attitude_1 + (100 - post_attitude_2) + post_attitude_3) / 3,
                   fu_donation = donation_amount)

# =============================================================================
# 2. Helpers
# =============================================================================
# AI minus one other level of `factor_name` (positive = AI more persuasive).
contrast_ai_minus <- function(m, factor_name, minus, label) {
  em <- emmeans::emmeans(m, stats::as.formula(paste("~", factor_name)), lmer.df = "asymptotic")
  lv <- as.character(em@levels[[factor_name]])
  v <- setNames(numeric(length(lv)), lv); v["ai"] <- 1; v[minus] <- -1
  emmeans::contrast(em, list(v)) |>
    summary(infer = TRUE) |> tibble::as_tibble() |>
    dplyr::transmute(spec = label, estimate, lower_ci = asymp.LCL, upper_ci = asymp.UCL, p_value = p.value)
}

# Preregistered pooled LMM with `outcome` in place of the immediate attitude.
fit_outcome <- function(data, outcome, group_col, extra_fe = character(0)) {
  data$post <- data[[outcome]]
  fit_pooled_lmm(data, group_col, extra_fe = extra_fe, label = paste(outcome, group_col))
}

# OLS with HC2 standard errors and t(n - k) intervals (the estimatr::lm_robust
# default). Outcomes are in pp, so coefficients are differences in return rate.
lm_hc2 <- function(formula, data) {
  fit <- stats::lm(formula, data = data)
  X <- stats::model.matrix(fit)
  e <- stats::residuals(fit)
  h <- stats::hatvalues(fit)
  bread <- solve(crossprod(X))
  V <- bread %*% crossprod(X * (e^2 / (1 - h)), X) %*% bread
  b <- stats::coef(fit)
  se <- sqrt(diag(V))
  df <- fit$df.residual
  t_crit <- stats::qt(0.975, df)
  list(
    coef = b, vcov = V,
    table = tibble::tibble(term = names(b), estimate = unname(b),
                           lower_ci = unname(b - t_crit * se), upper_ci = unname(b + t_crit * se),
                           p_value = unname(2 * stats::pt(-abs(b / se), df)))
  )
}

# Lee (2009) bounds on the follow-up difference between `arms` (first arm
# minus second): residualise the outcome on `covariates` among respondents,
# then trim the arm with the higher response rate by the rate gap.
lee_bounds <- function(data, arm_col, arms, outcome, covariates) {
  ah <- data[data[[arm_col]] %in% arms, ]
  ah$arm <- as.character(ah[[arm_col]])
  rates <- tapply(ah$in_fu, ah$arm, mean)[arms]
  resp <- dplyr::filter(ah, in_fu)
  resp$y <- stats::residuals(stats::lm(stats::reformulate(covariates, outcome), data = resp))
  hi <- names(which.max(rates)); lo <- setdiff(arms, hi)
  q <- (rates[[hi]] - rates[[lo]]) / rates[[hi]]
  y_hi <- sort(resp$y[resp$arm == hi]); n_hi <- length(y_hi); k <- floor(q * n_hi)
  m_lo <- mean(resp$y[resp$arm == lo])
  sgn <- if (hi == arms[1]) 1 else -1
  bounds <- sort(sgn * c(mean(y_hi[seq_len(n_hi - k)]) - m_lo, mean(y_hi[(k + 1):n_hi]) - m_lo))
  tibble::tibble(spec = "Lee bounds", estimate = sgn * (mean(y_hi) - m_lo),
                 lower_ci = bounds[1], upper_ci = bounds[2], p_value = NA_real_,
                 n = nrow(resp), rate_first = rates[[arms[1]]], rate_second = rates[[arms[2]]],
                 trim_share = q, trimmed_arm = hi)
}

# =============================================================================
# 3. Studies 1-3 analysis frame
# =============================================================================
studies <- load_studies(c("study_1", "study_2", "study_3"))
all_rows <- purrr::imap_dfr(studies, ~ dplyr::mutate(.x, study = .y)) |>
  complete_case_filter(label = "si_13 S1-3") |>
  dplyr::add_count(persuadee_id, issue_id, name = "n_same_issue") |>
  dplyr::add_count(persuadee_id, name = "n_convs") |>
  dplyr::left_join(fu_blocks, by = "row_id") |>
  dplyr::transmute(
    study, row_id,
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
    group3 = dplyr::case_when(group %in% c("control", "ai", "ai_constrained") ~ group, TRUE ~ "human"),
    age = dplyr::coalesce(as.numeric(age), stats::median(as.numeric(age), na.rm = TRUE)),
    gender    = forcats::fct_na_value_to_level(factor(gender),         "missing"),
    education = forcats::fct_na_value_to_level(factor(education),      "missing"),
    ideology  = forcats::fct_na_value_to_level(factor(ideology_coded), "missing"),
    eligible = n_same_issue == 1, single_conv = n_convs == 1,
    log_n_facts = dplyr::if_else(dplyr::coalesce(fact_checked, FALSE) & dplyr::coalesce(n_fact_claims, 0) > 0,
                                 log(n_fact_claims), NA_real_),
    gap_days, fu,
    in_fu = n_same_issue == 1 & !is.na(fu)
  )

elig <- dplyr::filter(all_rows, eligible)
d    <- dplyr::filter(elig, in_fu)
message(sprintf("  S1-3: %d complete-case rows; %d eligible (single conversation on the issue); %d re-surveyed (%.1f%%)",
                nrow(all_rows), nrow(elig), nrow(d), 100 * nrow(d) / nrow(elig)))

summarise_sample <- function(x) {
  x |>
    dplyr::summarise(eligible_rows = dplyr::n(), fu_rows = sum(in_fu), fu_rate = mean(in_fu),
                     persuadees = dplyr::n_distinct(persuadee),
                     fu_persuadees = dplyr::n_distinct(persuadee[in_fu]),
                     median_gap_days = stats::median(gap_days[in_fu]),
                     min_gap_days = min(gap_days[in_fu]), max_gap_days = max(gap_days[in_fu]),
                     .groups = "drop")
}
# The Methods quote the Studies 1-2 median interval; the SI table omits that row.
followup_sample <- dplyr::bind_rows(
  elig |> dplyr::group_by(study) |> summarise_sample(),
  elig |> dplyr::filter(study %in% c("study_1", "study_2")) |> summarise_sample() |>
    dplyr::mutate(study = "pooled S1-2", .before = 1),
  elig |> summarise_sample() |> dplyr::mutate(study = "pooled S1-3", .before = 1)
)
save_rds(followup_sample, "followup_sample")
print(followup_sample)

# =============================================================================
# 4. Effects immediately and at follow-up (same respondents)
# =============================================================================
followup_by_class <- dplyr::bind_rows(
  tidy_te_vs_control(fit_outcome(d, "post", "group"), "group") |> dplyr::mutate(wave = "immediate"),
  tidy_te_vs_control(fit_outcome(d, "fu",   "group"), "group") |> dplyr::mutate(wave = "follow-up")
)
save_rds(followup_by_class, "followup_by_class")

m_post <- fit_outcome(d, "post", "group3")
m_fu   <- fit_outcome(d, "fu",   "group3")
te_post <- tidy_te_vs_control(m_post, "group3")
te_fu   <- tidy_te_vs_control(m_fu,   "group3")
followup_ai_vs_human <- dplyr::bind_rows(
  contrast_ai_minus(m_post, "group3", "human", "AI - human, immediate"),
  contrast_ai_minus(m_fu,   "group3", "human", "AI - human, follow-up"),
  te_post |> dplyr::transmute(spec = paste(group, "vs control, immediate"), estimate, lower_ci, upper_ci, p_value),
  te_fu   |> dplyr::transmute(spec = paste(group, "vs control, follow-up"), estimate, lower_ci, upper_ci, p_value)
) |> dplyr::mutate(n = nrow(d))
save_rds(followup_ai_vs_human, "followup_ai_vs_human")
print(followup_ai_vs_human)

followup_per_study <- purrr::map_dfr(c("study_1", "study_2", "study_3"), function(s) {
  ds <- dplyr::filter(d, study == s)
  dplyr::bind_rows(
    tidy_te_vs_control(fit_outcome(ds, "post", "group"), "group") |>
      dplyr::transmute(spec = paste(group, "vs control, immediate"), estimate, lower_ci, upper_ci, p_value),
    tidy_te_vs_control(fit_outcome(ds, "fu", "group"), "group") |>
      dplyr::transmute(spec = paste(group, "vs control, follow-up"), estimate, lower_ci, upper_ci, p_value),
    contrast_ai_minus(fit_outcome(ds, "post", "group3"), "group3", "human", "AI - human, immediate"),
    contrast_ai_minus(fit_outcome(ds, "fu",   "group3"), "group3", "human", "AI - human, follow-up")
  ) |> dplyr::mutate(study = s, n = nrow(ds), .before = 1)
})
save_rds(followup_per_study, "followup_per_study")

# =============================================================================
# 5. Share of each effect retained
# =============================================================================
# Retention = effect vs control at follow-up / effect vs control immediately,
# from the preregistered LMMs above. CIs from a persuadee-clustered bootstrap;
# resampled persuadees get fresh ids so duplicates enter the random effects as
# distinct persuadees. Persuadees are listed in order of first appearance, not
# by id, so the draws do not depend on the (pseudonymised, in the public
# release) id labels.
retention_shares <- function(x) {
  b_post <- lme4::fixef(suppressMessages(fit_pooled_lmm(x, "group3", label = "boot post")))
  x$post <- x$fu
  b_fu <- lme4::fixef(suppressMessages(fit_pooled_lmm(x, "group3", label = "boot fu")))
  ai <- b_fu[["group3ai"]] / b_post[["group3ai"]]
  hu <- b_fu[["group3human"]] / b_post[["group3human"]]
  c(retained_ai = ai, retained_human = hu, retained_diff = ai - hu)
}
boot_summary <- function(point, boot) {
  tibble::tibble(
    quantity = names(point), estimate = unname(point),
    lower_ci = apply(boot, 2, stats::quantile, 0.025),
    upper_ci = apply(boot, 2, stats::quantile, 0.975),
    p_two_sided = apply(boot, 2, function(b) min(1, 2 * min(mean(b <= 0), mean(b >= 0)))),
    n_boot = nrow(boot)
  )
}

message(sprintf("  retention bootstrap (LMM, B = %d)...", N_BOOT))
set.seed(20261030)
rows_by_id <- split(seq_len(nrow(d)), factor(d$persuadee, levels = unique(d$persuadee)))
draws <- replicate(N_BOOT, sample(names(rows_by_id), length(rows_by_id), replace = TRUE),
                   simplify = FALSE)
boot_lmm <- do.call(rbind, parallel::mclapply(draws, function(pick) {
  idx <- rows_by_id[pick]
  b <- d[unlist(idx, use.names = FALSE), ]
  b$persuadee <- rep(seq_along(idx), lengths(idx))
  retention_shares(b)
}, mc.cores = max(1L, parallel::detectCores() - 2L, na.rm = TRUE), mc.set.seed = FALSE))
retention_lmm <- boot_summary(retention_shares(d), boot_lmm)

# Robustness: OLS with the preregistered fixed effects only (no random
# effects), AI and pooled humans vs control, Constrained AI rows dropped.
d_ols <- d |>
  dplyr::filter(group != "ai_constrained") |>
  dplyr::transmute(persuadee, fu, pre, post, issue = factor(issue), attempt,
                   group = factor(group3, levels = c("control", "ai", "human")))
ols_effects <- function(x) {
  b_post <- stats::coef(stats::lm(post ~ group + pre + issue + attempt, data = x))
  b_fu   <- stats::coef(stats::lm(fu   ~ group + pre + issue + attempt, data = x))
  ret_ai <- b_fu[["groupai"]]    / b_post[["groupai"]]
  ret_hu <- b_fu[["grouphuman"]] / b_post[["grouphuman"]]
  c(post_ai = b_post[["groupai"]], post_human = b_post[["grouphuman"]],
    fu_ai = b_fu[["groupai"]], fu_human = b_fu[["grouphuman"]],
    retained_ai = ret_ai, retained_human = ret_hu, retained_diff = ret_ai - ret_hu)
}
message(sprintf("  retention bootstrap (OLS, B = %d)...", N_BOOT))
set.seed(20261007)
ids <- unique(d_ols$persuadee)
rows_by_id_ols <- split(seq_len(nrow(d_ols)), d_ols$persuadee)
boot_ols <- t(replicate(N_BOOT, {
  pick <- sample(ids, length(ids), replace = TRUE)
  ols_effects(d_ols[unlist(rows_by_id_ols[pick], use.names = FALSE), ])
}))
retention_ols <- boot_summary(ols_effects(d_ols), boot_ols) |> dplyr::mutate(n = nrow(d_ols))

# Absolute decay: AI - human difference in (follow-up - immediate) attitude.
decay_ai_minus_human <- contrast_ai_minus(fit_outcome(dplyr::mutate(d, decay = fu - post), "decay", "group3"),
                                          "group3", "human", "AI - human, follow-up minus immediate")

followup_retention <- list(
  shares = dplyr::inner_join(
    te_post |> dplyr::select(group, immediate = estimate),
    te_fu   |> dplyr::select(group, follow_up = estimate), by = "group") |>
    dplyr::mutate(share_retained = follow_up / immediate),
  lmm   = retention_lmm,
  ols   = retention_ols,
  decay = decay_ai_minus_human,
  n     = nrow(d)
)
save_rds(followup_retention, "followup_retention")
print(followup_retention)

# =============================================================================
# 6. Selection into the follow-up (Studies 1-3)
# =============================================================================
# 6a. Return by assigned condition: each persuadee's first session, one row each.
first <- elig |> dplyr::filter(attempt == 1) |> dplyr::distinct(persuadee, .keep_all = TRUE) |>
  dplyr::mutate(resp_pp = as.numeric(in_fu) * 100,
                group  = forcats::fct_relevel(factor(group),  "control"),
                group3 = forcats::fct_relevel(factor(group3), "control"))
s123_rates <- first |>
  dplyr::filter(!is.na(group)) |>
  dplyr::group_by(group) |>
  dplyr::summarise(n = dplyr::n(), fu_rate = mean(in_fu), .groups = "drop")
resp_class <- lm_hc2(resp_pp ~ group + study, first)
idx <- grep("^group", names(resp_class$coef))
wald <- as.numeric(t(resp_class$coef[idx]) %*% solve(resp_class$vcov[idx, idx]) %*% resp_class$coef[idx])
resp_three <- lm_hc2(resp_pp ~ group3 + study, first)
ai_h <- first |> dplyr::filter(group3 %in% c("ai", "human")) |>
  dplyr::mutate(is_ai = as.numeric(group3 == "ai"))
resp_ai_vs_human <- lm_hc2(resp_pp ~ is_ai + study, ai_h)
s123_tests <- dplyr::bind_rows(
  tibble::tibble(spec = "joint Wald: persuader class predicts response", estimate = wald,
                 lower_ci = NA_real_, upper_ci = NA_real_,
                 p_value = stats::pchisq(wald, length(idx), lower.tail = FALSE), df = length(idx)),
  resp_class$table |> dplyr::filter(grepl("^group", term)) |>
    dplyr::transmute(spec = paste("response pp vs control, by class:", sub("^group", "", term)),
                     estimate, lower_ci, upper_ci, p_value),
  resp_three$table |> dplyr::filter(grepl("^group3", term)) |>
    dplyr::transmute(spec = paste("response pp vs control:", term), estimate, lower_ci, upper_ci, p_value),
  resp_ai_vs_human$table |> dplyr::filter(term == "is_ai") |>
    dplyr::transmute(spec = "response pp: AI - human", estimate, lower_ci, upper_ci, p_value)
)

# 6b. Immediate AI - human gap among later returners vs non-returners.
elig_b <- elig |> dplyr::mutate(group3 = forcats::fct_relevel(factor(group3), "control"),
                                resp = factor(ifelse(in_fu, "responder", "nonresponder")))
m_sel <- lme4::lmer(post ~ pre + issue + attempt + group3 * resp + (1 | persuader) + (1 | persuadee),
                    data = elig_b, REML = TRUE, control = lmm_control)
em_sel <- emmeans::emmeans(m_sel, ~ group3 | resp, lmer.df = "asymptotic")
lv <- as.character(em_sel@levels$group3)
v <- setNames(numeric(length(lv)), lv); v["ai"] <- 1; v["human"] <- -1
gap_by_resp <- emmeans::contrast(em_sel, list(ai_minus_human = v), by = "resp")
s123_gap <- dplyr::bind_rows(
  summary(gap_by_resp, infer = TRUE) |> tibble::as_tibble() |>
    dplyr::transmute(spec = paste("immediate AI - human,", resp), estimate,
                     lower_ci = asymp.LCL, upper_ci = asymp.UCL, p_value = p.value),
  emmeans::contrast(gap_by_resp, method = "revpairwise", by = NULL) |>
    summary(infer = TRUE) |> tibble::as_tibble() |>
    dplyr::transmute(spec = "difference (responders - non-responders)", estimate,
                     lower_ci = asymp.LCL, upper_ci = asymp.UCL, p_value = p.value)
)

# 6c. Inverse-probability-of-response weights (trimmed at the 99th percentile
# among respondents).
ps <- stats::glm(in_fu ~ group + study + issue + pre + post + attempt + age + gender + education + ideology,
                 data = elig, family = stats::binomial(), na.action = stats::na.exclude)
elig$p_resp <- stats::fitted(ps)
w <- mean(elig$in_fu) / dplyr::coalesce(elig$p_resp, mean(elig$in_fu))
w <- pmin(w, stats::quantile(w[elig$in_fu], 0.99))
d_w <- elig |> dplyr::mutate(w = w) |> dplyr::filter(in_fu) |>
  dplyr::mutate(group3 = forcats::fct_relevel(factor(group3), "control"))
m_ipw <- lme4::lmer(fu ~ pre + issue + group3 + attempt + (1 | persuader) + (1 | persuadee),
                    data = d_w, weights = w, REML = TRUE, control = lmm_control)

# 6d. Lee bounds on the follow-up AI - human gap.
lee <- lee_bounds(elig, "group3", c("ai", "human"), "fu", c("pre", "issue"))

# 6e. Subsamples with one row per persuadee: no persuadee intercept, no attempt term.
fit_one_row_each <- function(data) {
  data <- dplyr::mutate(data, group3 = forcats::fct_relevel(factor(group3), "control"))
  lme4::lmer(fu ~ pre + issue + group3 + (1 | persuader), data = data, REML = TRUE, control = lmm_control)
}
s123_robust <- dplyr::bind_rows(
  contrast_ai_minus(m_fu, "group3", "human", "main (preregistered spec)") |> dplyr::mutate(n = nrow(d)),
  contrast_ai_minus(m_ipw, "group3", "human", "inverse-probability-of-response weighted") |>
    dplyr::mutate(n = nrow(d)),
  contrast_ai_minus(fit_outcome(d, "fu", "group3", extra_fe = "study"), "group3", "human", "+ study fixed effect") |>
    dplyr::mutate(n = nrow(d)),
  contrast_ai_minus(fit_one_row_each(dplyr::filter(d, attempt == 1)), "group3", "human", "first session only") |>
    dplyr::mutate(n = sum(d$attempt == 1)),
  contrast_ai_minus(fit_one_row_each(dplyr::filter(d, single_conv)), "group3", "human",
                    "persuadees with one conversation in total") |> dplyr::mutate(n = sum(d$single_conv)),
  lee |> dplyr::select(spec, estimate, lower_ci, upper_ci, p_value) |> dplyr::mutate(n = nrow(d))
)
print(s123_robust)

# =============================================================================
# 7. Claim count and attitudes, immediately and at follow-up
# =============================================================================
# si_07 specification on the re-surveyed persuader conversations. Claim count
# is not randomised, so this compares the claim-count gradient at the two
# waves in the same conversations rather than estimating a causal effect.
d_facts <- d |> dplyr::filter(group != "control", !is.na(log_n_facts)) |>
  dplyr::mutate(is_human = as.integer(group3 == "human"))
followup_claims <- purrr::map_dfr(c(immediate = "post", `follow-up` = "fu"), function(o) {
  dd <- d_facts; dd$y <- dd[[o]]
  m <- lme4::lmer(y ~ pre + issue + attempt + log_n_facts + is_human + (1 | persuader) + (1 | persuadee),
                  data = dd, REML = TRUE, control = lmm_control)
  fe <- lme4::fixef(m); se <- sqrt(diag(as.matrix(stats::vcov(m))))
  tibble::tibble(term = c("per doubling of claims", "human - AI at equal claims"),
                 estimate = c(log(2) * fe[["log_n_facts"]], fe[["is_human"]]),
                 se = c(log(2) * se[["log_n_facts"]], se[["is_human"]])) |>
    dplyr::mutate(lower_ci = estimate - 1.96 * se, upper_ci = estimate + 1.96 * se,
                  p_value = 2 * stats::pnorm(-abs(estimate / se)), n = nrow(dd))
}, .id = "wave")
save_rds(followup_claims, "followup_claims")
print(followup_claims)

# =============================================================================
# 8. Study 4: follow-up donation
# =============================================================================
s4 <- load_study("study_4") |>
  complete_case_filter(require_attitudes = FALSE, label = "si_13 S4") |>
  dplyr::left_join(fu_blocks, by = "row_id") |>
  dplyr::transmute(
    persuadee = persuadee_id,
    persuader = dplyr::case_when(
      persuader_class == "canvasser" & !is.na(persuader_id) ~ as.character(persuader_id),
      persuader_class == "ai"      ~ paste0("ai_",      persuadee_session),
      persuader_class == "control" ~ paste0("control_", persuadee_session)
    ),
    group = forcats::fct_relevel(droplevels(persuader_class), "control"),
    donation = donation_amount, fu_donation, gap_days,
    pre_attitude, pre_donation_will = pre_donation_willingness, age, ideology = ordered(ideology_coded)
  ) |>
  dplyr::mutate(in_fu = !is.na(fu_donation))
s4_all <- dplyr::filter(s4, !is.na(donation), !is.na(pre_attitude), !is.na(pre_donation_will))
s4     <- dplyr::filter(s4_all, !is.na(age), !is.na(ideology))
s4_fu  <- dplyr::filter(s4, in_fu)
message(sprintf("  S4: %d rows with prereg covariates (%d without); %d re-surveyed (%.1f%%)",
                nrow(s4), nrow(s4_all), nrow(s4_fu), 100 * mean(s4$in_fu)))

followup_s4_sample <- s4_all |>
  dplyr::summarise(rows = dplyr::n(), fu_rows = sum(in_fu), fu_rate = mean(in_fu),
                   rows_prereg_covariates = sum(!is.na(age) & !is.na(ideology)),
                   fu_rows_prereg_covariates = sum(in_fu & !is.na(age) & !is.na(ideology)),
                   median_gap_days = stats::median(gap_days[in_fu], na.rm = TRUE),
                   min_gap_days = min(gap_days[in_fu], na.rm = TRUE),
                   max_gap_days = max(gap_days[in_fu], na.rm = TRUE),
                   q25_gap_days = stats::quantile(gap_days[in_fu], 0.25, na.rm = TRUE),
                   q75_gap_days = stats::quantile(gap_days[in_fu], 0.75, na.rm = TRUE))
save_rds(followup_s4_sample, "followup_s4_sample")

fit_s4 <- function(data, outcome, weights = NULL) {
  data$y <- data[[outcome]]
  data$w <- if (is.null(weights)) 1 else weights
  lme4::lmer(y ~ pre_attitude + pre_donation_will + age + ideology + group + (1 | persuader),
             data = data, weights = w, REML = TRUE, control = lmm_control)
}

followup_s4_results <- purrr::map_dfr(c(immediate = "donation", `follow-up` = "fu_donation"), function(o) {
  m <- fit_s4(s4_fu, o)
  dplyr::bind_rows(
    tidy_te_vs_control(m, "group") |>
      dplyr::transmute(spec = paste(group, "vs control"), estimate, lower_ci, upper_ci, p_value),
    contrast_ai_minus(m, "group", "canvasser", "AI - canvasser")
  )
}, .id = "wave") |> dplyr::mutate(n = nrow(s4_fu))
save_rds(followup_s4_results, "followup_s4_results")
print(followup_s4_results)

# Selection checks, as for Studies 1-3.
ps4 <- stats::glm(in_fu ~ group + pre_attitude + pre_donation_will + donation + age + ideology,
                  data = s4, family = stats::binomial())
w4 <- mean(s4$in_fu) / stats::fitted(ps4)
w4 <- pmin(w4, stats::quantile(w4[s4$in_fu], 0.99))[s4$in_fu]

s4_lee <- lee_bounds(s4, "group", c("ai", "canvasser"), "fu_donation", c("pre_attitude", "pre_donation_will"))

followup_s4_robustness <- dplyr::bind_rows(
  contrast_ai_minus(fit_s4(s4_fu, "fu_donation"), "group", "canvasser", "main (preregistered spec)") |>
    dplyr::mutate(n = nrow(s4_fu)),
  contrast_ai_minus(fit_s4(s4_fu, "fu_donation", weights = w4), "group", "canvasser",
                    "inverse-probability-of-response weighted") |> dplyr::mutate(n = nrow(s4_fu)),
  s4_lee |> dplyr::select(spec, estimate, lower_ci, upper_ci, p_value, n, trim_share, trimmed_arm)
)
save_rds(followup_s4_robustness, "followup_s4_robustness")
print(followup_s4_robustness)

s4_rates <- s4 |> dplyr::group_by(group) |>
  dplyr::summarise(n = dplyr::n(), fu_rate = mean(in_fu), .groups = "drop")
s4_resp_test <- lm_hc2(resp_pp ~ group, dplyr::mutate(s4, resp_pp = as.numeric(in_fu) * 100))
s4_sel <- lme4::lmer(donation ~ pre_attitude + pre_donation_will + age + ideology +
                       group * factor(in_fu) + (1 | persuader),
                     data = s4, REML = TRUE, control = lmm_control)
em4 <- emmeans::emmeans(s4_sel, ~ group | in_fu, lmer.df = "asymptotic")
lv4 <- as.character(em4@levels$group)
v4 <- setNames(numeric(length(lv4)), lv4); v4["ai"] <- 1; v4["canvasser"] <- -1
s4_gap_by_resp <- emmeans::contrast(em4, list(ai_minus_canvasser = v4), by = "in_fu")
s4_gap <- dplyr::bind_rows(
  summary(s4_gap_by_resp, infer = TRUE) |> tibble::as_tibble() |>
    dplyr::transmute(spec = paste("immediate AI - canvasser,",
                                  ifelse(as.character(in_fu) == "TRUE", "responder", "nonresponder")),
                     estimate, lower_ci = asymp.LCL, upper_ci = asymp.UCL, p_value = p.value),
  emmeans::contrast(s4_gap_by_resp, method = "revpairwise", by = NULL) |>
    summary(infer = TRUE) |> tibble::as_tibble() |>
    dplyr::transmute(spec = "difference (responders - non-responders)", estimate,
                     lower_ci = asymp.LCL, upper_ci = asymp.UCL, p_value = p.value)
)
s4_tests <- s4_resp_test$table |>
  dplyr::filter(term != "(Intercept)") |>
  dplyr::transmute(spec = paste("response pp vs control, by class:", sub("^group", "", term)),
                   estimate, lower_ci, upper_ci, p_value)

# =============================================================================
# 9. Save selection outputs
# =============================================================================
followup_response <- list(s123_rates = s123_rates, s123_tests = s123_tests,
                          s4_rates = s4_rates, s4_tests = s4_tests)
save_rds(followup_response, "followup_response")

followup_selection_gap <- list(s123 = s123_gap, s4 = s4_gap)
save_rds(followup_selection_gap, "followup_selection_gap")

followup_robustness <- list(s123 = s123_robust, s123_lee = lee)
save_rds(followup_robustness, "followup_robustness")

print(followup_response); print(followup_selection_gap)

message("\n  done.")
