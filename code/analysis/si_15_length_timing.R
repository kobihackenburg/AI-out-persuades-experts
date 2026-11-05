# si_15_length_timing.R
# =============================================================================
# Message length and conversation timing vs attitude change, holding the
# number of claims constant (Studies 1-2, fact-checked conversations).
#
# Per-conversation length and timing come from
# code/analysis/py/si_15_conversation_timing.py (output/results/_py/).
# All predictors are log2, so coefficients are pp per doubling. Length, timing
# and claims are all recorded during the conversation and not randomly
# assigned, so these are associations, not causal effects.
#
# 1. Length and reply time (unconstrained AI and human persuaders):
#      post ~ pre + issue + attempt + log_claims [+ log_msg_len + log_reply_s
#             + log_reply_words] [+ is_human] + (1|persuader) + (1|persuadee)
#    fit on AI and human conversations together, AI only and human only.
#    Constrained AI is excluded because its imposed delay is inside the
#    measured reply time; AI reply time also includes generation time.
#    Added explanatory power of the three length/timing terms: ML fits, LRT,
#    marginal R^2.
#
# 2. Waiting, pause and exchange time (human and Constrained AI):
#      post ~ pre + issue + attempt + log_claims + log_msg_len + <timing>
#             + (1|persuader) + (1|persuadee)
#    Human conversations: wait (persuadee message to persuader's next
#    message, exact) and pause (persuader message to persuadee reply).
#    Constrained AI: the per-message drawn delay is not logged separately, so
#    only the exchange time (persuadee message to next persuadee message =
#    drawn delay + reading + reply) is available.
#
# Outputs (under output/results/, consumer after `->`):
#   length_reply_desc.rds    -> tab_length_reply_time (sample medians), SI prose
#   length_reply_coefs.rds   -> tab_length_reply_time
#   length_reply_lrt.rds     -> tab_length_reply_time (LRT, marginal R^2)
#   waiting_time_desc.rds    -> tab_waiting_time (sample medians), SI prose
#   waiting_time_coefs.rds   -> tab_waiting_time
# =============================================================================

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))
source(here::here("code/analysis/_lmm_helpers.R"))

message("\n==== si_15_length_timing.R ====")

timing <- readr::read_csv(file.path(RESULTS_PY, "si_15_conversation_timing.csv"),
                          show_col_types = FALSE)

studies <- load_studies(c("study_1", "study_2"))
cc <- purrr::imap_dfr(studies, ~ dplyr::mutate(.x, study = .y)) |>
  complete_case_filter(label = "si_15 length/timing") |>
  dplyr::filter(persuader_class != "control", fact_checked,
                !is.na(n_fact_claims), n_fact_claims > 0) |>
  dplyr::inner_join(dplyr::select(timing, -study), by = "persuadee_session")

fit <- function(dat, fm_rhs, reml = TRUE) {
  fm <- stats::as.formula(paste("post ~ pre + issue + attempt", fm_rhs,
                                "+ (1 | persuader) + (1 | persuadee)"))
  lme4::lmer(fm, data = dat, REML = reml, control = lme4::lmerControl(optimizer = "bobyqa"))
}

tidy_terms <- function(m, terms) {
  broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE) |>
    dplyr::filter(term %in% terms) |>
    dplyr::mutate(p.value = 2 * stats::pnorm(-abs(statistic)))
}

r2_marginal <- function(m) {
  v_fixed <- stats::var(as.vector(lme4::getME(m, "X") %*% lme4::fixef(m)))
  v_re <- sum(as.data.frame(lme4::VarCorr(m))$vcov)
  v_fixed / (v_fixed + v_re)
}

# =============================================================================
# 1. Message length and persuadee reply time
# =============================================================================
base <- cc |>
  dplyr::filter(!dplyr::coalesce(ai_constrained, FALSE)) |>
  dplyr::filter(median_reply_s > 0, mean_persuader_msg_words > 0) |>
  dplyr::transmute(
    persuadee = persuadee_id,
    persuader = dplyr::if_else(!is.na(persuader_id), persuader_id, paste0("ai_", ai_model_full)),
    issue = issue_id, pre = pre_attitude, post = post_attitude,
    attempt = dplyr::coalesce(as.integer(persuadee_attempt_number), 1L),
    is_human = as.integer(persuader_class != "ai"),
    claims = n_fact_claims, msg_len = mean_persuader_msg_words,
    reply_s = median_reply_s, reply_words = median_reply_words,
    log_claims = log2(claims), log_msg_len = log2(msg_len),
    log_reply_s = log2(reply_s), log_reply_words = log2(reply_words + 1)
  )

length_reply_desc <- base |>
  dplyr::group_by(is_human) |>
  dplyr::summarise(n = dplyr::n(), median_claims = median(claims),
                   median_msg_len = median(msg_len), median_reply_s = median(reply_s),
                   median_reply_words = median(reply_words),
                   r_msg_len_claims = cor(log_msg_len, log_claims),
                   r_reply_s_msg_len = cor(log_reply_s, log_msg_len), .groups = "drop")
save_rds(length_reply_desc, "length_reply_desc")
print(length_reply_desc, width = Inf)

samples <- list(ai_vs_human = base,
                ai_only = dplyr::filter(base, is_human == 0),
                human_only = dplyr::filter(base, is_human == 1))

specs <- list(
  claims = "+ log_claims",
  claims_timing = "+ log_claims + log_msg_len + log_reply_s + log_reply_words"
)

length_terms <- c("is_human", "log_claims", "log_msg_len", "log_reply_s", "log_reply_words")

spec_rhs <- function(dat, rhs) {
  if (length(unique(dat$is_human)) > 1) paste(rhs, "+ is_human") else rhs
}

length_reply_coefs <- purrr::imap_dfr(samples, function(dat, sname) {
  purrr::imap_dfr(specs, function(rhs, spec) {
    tidy_terms(fit(dat, spec_rhs(dat, rhs)), length_terms) |>
      dplyr::mutate(sample = sname, spec = spec, n = nrow(dat), .before = 1)
  })
})
save_rds(length_reply_coefs, "length_reply_coefs")
print(dplyr::select(length_reply_coefs, sample, spec, n, term, estimate, conf.low, conf.high, p.value),
      n = Inf)

length_reply_lrt <- purrr::imap_dfr(samples, function(dat, sname) {
  m0 <- fit(dat, spec_rhs(dat, specs$claims), reml = FALSE)
  m1 <- fit(dat, spec_rhs(dat, specs$claims_timing), reml = FALSE)
  a <- stats::anova(m0, m1)
  tibble::tibble(sample = sname, chisq = a$Chisq[2], df = a$Df[2], p_value = a$`Pr(>Chisq)`[2],
                 r2m_claims = r2_marginal(m0), r2m_claims_timing = r2_marginal(m1))
})
save_rds(length_reply_lrt, "length_reply_lrt")
print(length_reply_lrt, width = Inf)

# =============================================================================
# 2. Waiting, pause and exchange time (human and Constrained AI)
# =============================================================================
tt <- cc |>
  dplyr::transmute(
    persuadee = persuadee_id,
    persuader = dplyr::if_else(!is.na(persuader_id), persuader_id, paste0("ai_", ai_model_full)),
    group = dplyr::case_when(persuader_class != "ai" ~ "human",
                             dplyr::coalesce(ai_constrained, FALSE) ~ "constrained_ai",
                             TRUE ~ "ai"),
    issue = issue_id, pre = pre_attitude, post = post_attitude,
    attempt = dplyr::coalesce(as.integer(persuadee_attempt_number), 1L),
    claims = n_fact_claims, msg_len = mean_persuader_msg_words,
    wait_s = median_wait_s, pause_s = median_reply_s, cycle_s = median_cycle_s,
    log_claims = log2(claims), log_msg_len = log2(msg_len),
    log_wait = log2(wait_s), log_pause = log2(pause_s), log_cycle = log2(cycle_s)
  )

human <- dplyr::filter(tt, group == "human", wait_s > 0, pause_s > 0, cycle_s > 0)
cons  <- dplyr::filter(tt, group == "constrained_ai", cycle_s > 0)

waiting_time_desc <- dplyr::bind_rows(human, cons) |>
  dplyr::group_by(group) |>
  dplyr::summarise(n = dplyr::n(), median_wait_s = median(wait_s, na.rm = TRUE),
                   median_pause_s = median(pause_s), median_cycle_s = median(cycle_s),
                   iqr_cycle_s = IQR(cycle_s), median_claims = median(claims), .groups = "drop")
save_rds(waiting_time_desc, "waiting_time_desc")
print(waiting_time_desc, width = Inf)

timing_terms <- c("log_claims", "log_msg_len", "log_wait", "log_pause", "log_cycle")
fit_timing <- function(dat, rhs, label) {
  tidy_terms(fit(dat, paste("+ log_claims + log_msg_len", rhs)), timing_terms) |>
    dplyr::mutate(model = label, n = nrow(dat), .before = 1)
}

waiting_time_coefs <- dplyr::bind_rows(
  fit_timing(human, "+ log_wait + log_pause", "human"),
  fit_timing(cons,  "+ log_cycle",            "constrained_ai")
)
save_rds(waiting_time_coefs, "waiting_time_coefs")
print(dplyr::select(waiting_time_coefs, model, n, term, estimate, conf.low, conf.high, p.value),
      n = Inf)

message("\n  done.")
