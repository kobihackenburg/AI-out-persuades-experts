# si_19_conviction_markers.R
# =============================================================================
# Does Schoenegger et al.'s (2026) "conviction" account explain AI's
# advantage over human persuaders? Markers are maximizers, boosters and
# hedges per 100 persuader words (output/results/_py/si_19_conviction_markers.csv,
# written by code/analysis/py/si_19_conviction_markers.py).
#
# Sample and model match si_07_ai_gap_net_of_facts.R spec (2): fact-checked
# Studies 1-2 conversations with at least one claim, unconstrained AI vs all
# human classes,
#
#   post ~ pre + issue + attempt + [adjustors] + is_human
#        + (1 | persuader) + (1 | persuadee)
#
# with four adjustor sets: none; the three markers; log claim count; both.
#
# Outputs (under output/results/, consumer after `->`):
#   conviction_marker_means.rds -> tab_conviction_markers
#   conviction_gap_models.rds   -> tab_conviction_markers
# =============================================================================

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))
source(here::here("code/analysis/_lmm_helpers.R"))

message("\n==== si_19_conviction_markers.R ====")

markers <- readr::read_csv(file.path(RESULTS_PY, "si_19_conviction_markers.csv"))

studies <- load_studies(c("study_1", "study_2"))
d <- purrr::imap_dfr(studies, ~ dplyr::mutate(.x, study = .y)) |>
  complete_case_filter(label = "si_19 conviction") |>
  dplyr::filter(persuader_class != "control", fact_checked, !is.na(n_fact_claims), n_fact_claims > 0,
                !(persuader_class == "ai" & dplyr::coalesce(ai_constrained, FALSE))) |>
  dplyr::inner_join(markers, by = "row_id") |>
  dplyr::transmute(
    persuadee = persuadee_id,
    persuader = dplyr::if_else(!is.na(persuader_id), persuader_id, paste0("ai_", ai_model_full)),
    issue = issue_id, pre = pre_attitude, post = post_attitude,
    attempt = dplyr::coalesce(as.integer(persuadee_attempt_number), 1L),
    is_human = as.integer(persuader_class != "ai"),
    log_n_facts = log(n_fact_claims),
    maximizers = maximizers_per100, boosters = boosters_per100, hedges = hedges_per100
  )
message(sprintf("  n = %d (AI %d, human %d)", nrow(d), sum(d$is_human == 0), sum(d$is_human == 1)))

conviction_marker_means <- d |>
  dplyr::group_by(persuader = ifelse(is_human == 1, "human", "ai")) |>
  dplyr::summarise(dplyr::across(c(maximizers, boosters, hedges), mean), n = dplyr::n(),
                   .groups = "drop")
save_rds(conviction_marker_means, "conviction_marker_means")
print(conviction_marker_means)

specs <- list(
  none       = "",
  conviction = "+ maximizers + boosters + hedges",
  facts      = "+ log_n_facts",
  both       = "+ log_n_facts + maximizers + boosters + hedges"
)
conviction_gap_models <- purrr::imap_dfr(specs, function(rhs, name) {
  fm <- stats::as.formula(paste("post ~ pre + issue + attempt", rhs,
                                "+ is_human + (1 | persuader) + (1 | persuadee)"))
  m <- lme4::lmer(fm, data = d, REML = TRUE,
                  control = lme4::lmerControl(optimizer = "bobyqa"))
  broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE) |>
    dplyr::filter(term %in% c("is_human", "log_n_facts", "maximizers", "boosters", "hedges")) |>
    dplyr::mutate(spec = name, .before = 1)
}) |>
  dplyr::mutate(n_obs = nrow(d))
save_rds(conviction_gap_models, "conviction_gap_models")
print(conviction_gap_models, n = Inf)

message("\n  done.")
