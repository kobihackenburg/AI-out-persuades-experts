# si_17_words_claims.R
# =============================================================================
# Claims versus words. Is persuasive impact tracked by the number of words a
# persuader writes or by the number of claims it makes? Fact-checked
# Studies 1-2 conversations with at least one claim:
#
#   post ~ pre + issue + attempt + [log words] + [log claims] + is_human
#        + (1 | persuader) + (1 | persuadee)
#
# with log words alone, log claims alone, and both together. `is_human` is
# dropped in single-type samples. Samples:
#   ai_vs_human           unconstrained AI + all human classes (si_07 spec 2)
#   all_incl_constrained  the same plus Constrained AI
#   ai_only, human_only, constrained_only
# Neither quantity was randomized, so the coefficients are associations.
#
# Outputs (under output/results/, consumer after `->`):
#   claims_words_desc.rds   -> tab_claims_vs_words (n, r(log words, log claims))
#   claims_words_fits.rds   -> tab_claims_vs_words (coefficients, pp per doubling)
# =============================================================================

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))
source(here::here("code/analysis/_lmm_helpers.R"))

message("\n==== si_17_words_claims.R ====")

studies <- load_studies(c("study_1", "study_2"))
base <- purrr::imap_dfr(studies, ~ dplyr::mutate(.x, study = .y)) |>
  complete_case_filter(label = "si_17_words_claims") |>
  dplyr::filter(persuader_class != "control", fact_checked,
                !is.na(n_fact_claims), n_fact_claims > 0,
                !is.na(persuader_words), persuader_words > 0) |>
  dplyr::transmute(
    persuadee = persuadee_id,
    persuader = dplyr::if_else(!is.na(persuader_id), persuader_id, paste0("ai_", ai_model_full)),
    issue = issue_id, pre = pre_attitude, post = post_attitude,
    attempt = dplyr::coalesce(as.integer(persuadee_attempt_number), 1L),
    constrained = persuader_class == "ai" & dplyr::coalesce(ai_constrained, FALSE),
    is_human = as.integer(persuader_class != "ai"),
    words = persuader_words, claims = n_fact_claims,
    log_words = log(persuader_words), log_claims = log(n_fact_claims)
  )

samples <- list(
  ai_vs_human = dplyr::filter(base, !constrained),
  all_incl_constrained = base,
  ai_only = dplyr::filter(base, !constrained, is_human == 0),
  human_only = dplyr::filter(base, is_human == 1),
  constrained_only = dplyr::filter(base, constrained)
)

claims_words_desc <- purrr::imap_dfr(samples, ~ tibble::tibble(
  sample = .y, n = nrow(.x), median_words = median(.x$words), median_claims = median(.x$claims),
  r_log_words_log_claims = cor(.x$log_words, .x$log_claims)))
save_rds(claims_words_desc, "claims_words_desc")
print(claims_words_desc)

specs <- list(words = "+ log_words", claims = "+ log_claims", both = "+ log_words + log_claims")

fit_one <- function(dat, rhs, has_group) {
  grp <- if (has_group) "+ is_human" else ""
  fm <- stats::as.formula(paste("post ~ pre + issue + attempt", rhs, grp,
                                "+ (1 | persuader) + (1 | persuadee)"))
  m <- lme4::lmer(fm, data = dat, REML = TRUE, control = lme4::lmerControl(optimizer = "bobyqa"))
  broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE) |>
    dplyr::filter(term %in% c("is_human", "log_words", "log_claims"))
}

# Log-term coefficients are reported as pp per doubling (x log 2).
per_doubling <- function(est, term) ifelse(term == "is_human", est, est * log(2))

claims_words_fits <- purrr::imap_dfr(samples, function(dat, sname) {
  has_group <- length(unique(dat$is_human)) > 1
  purrr::imap_dfr(specs, ~ dplyr::mutate(fit_one(dat, .x, has_group), sample = sname, spec = .y, .before = 1))
}) |>
  dplyr::mutate(pp_per_doubling = per_doubling(estimate, term),
                pp_per_doubling_low = per_doubling(conf.low, term),
                pp_per_doubling_high = per_doubling(conf.high, term))
save_rds(claims_words_fits, "claims_words_fits")
print(claims_words_fits, n = Inf)

message("\n  done.")
