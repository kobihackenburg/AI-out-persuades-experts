# si_04_partner_ratings.R
# Per-item partner-rating contrasts (AI minus each human persuader class),
# pooled across Studies 1-3. Used by SI Table S17. The 7 items are the
# same as in the Study 4 partner-perception table (tabS22), which is
# what made this the natural S1-S3 analogue: tabS17 lets readers see
# whether the AI-vs-human gap on perceived arguments / learning /
# empathy / enjoyment that emerges in Study 4 is the same gap that
# was present in S1-S3.
#
# Output:
#   partner_ratings_s123.rds  long tibble with one row per (item, class)
#     contrast (AI - class), with estimate, SE, 95% CI, p_value.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))
source(here::here("code/analysis/_lmm_helpers.R"))

message("\n==== si_04_partner_ratings.R ====")

studies <- load_studies(c("study_1", "study_2", "study_3"))

ITEMS <- c("rating_enjoyment", "rating_learning", "rating_arguments",
           "rating_empathy",   "rating_deception", "rating_anthropo",
           "rating_bias")

HUMAN_CLASSES <- c("random_lay_person", "elite_lay_person", "canvasser",
                   "elite_debater", "coached_elite_debater")

prep <- function(df, study_label) {
  df |>
    complete_case_filter(label = paste("si_04", study_label),
                         require_attitudes = FALSE) |>
    dplyr::filter(!is.na(persuader_class)) |>
    dplyr::transmute(
      study     = study_label,
      persuadee = persuadee_id,
      persuader = dplyr::case_when(
        !is.na(persuader_id)                            ~ persuader_id,
        persuader_class == "ai" & !is.na(ai_model_full) ~ paste0("ai_", ai_model_full),
        TRUE                                            ~ paste0("solo_", persuadee_session)
      ),
      issue     = issue_id,
      group     = forcats::fct_relevel(droplevels(persuader_class), "control"),
      attempt   = dplyr::coalesce(as.integer(persuadee_attempt_number), 1L),
      dplyr::across(dplyr::all_of(ITEMS))
    )
}

pooled_ratings <- dplyr::bind_rows(
  prep(studies$study_1, "study_1"),
  prep(studies$study_2, "study_2"),
  prep(studies$study_3, "study_3")
) |>
  dplyr::mutate(study = factor(study))

results <- list()
for (it in ITEMS) {
  message(sprintf("  fitting %s ...", it))
  df <- pooled_ratings |>
    dplyr::filter(!is.na(.data[[it]])) |>
    droplevels()
  rhs <- paste0(it, " ~ issue + study + group + (1 | persuader) + (1 | persuadee)")
  fm  <- stats::as.formula(rhs)
  m   <- tryCatch(
    lme4::lmer(fm, data = df, REML = TRUE,
               control = lme4::lmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 100000))),
    error = function(e) { message(sprintf("    fit failed: %s", conditionMessage(e))); NULL }
  )
  if (is.null(m)) next

  # AI minus each human class -- use trt.vs.ctrl with `ref = "ai"`, then
  # flip sign so positive = AI rated higher than the class on this item.
  em <- emmeans::emmeans(m, ~ group, lmer.df = "asymptotic", nesting = NULL)
  contr <- emmeans::contrast(em, method = "trt.vs.ctrl", ref = "ai") |>
    summary(infer = TRUE, adjust = "none") |>
    tibble::as_tibble() |>
    dplyr::mutate(
      contrast = sub(" - ai$", "", as.character(contrast)),
      contrast = sub("^\\((.+)\\)$", "\\1", contrast),
      estimate = -estimate,
      lower_ci = -asymp.UCL,
      upper_ci = -asymp.LCL
    ) |>
    dplyr::filter(contrast %in% HUMAN_CLASSES) |>
    dplyr::transmute(item = it, class = contrast, estimate, lower_ci,
                     upper_ci, p_value = p.value)
  results[[it]] <- contr
}

partner_ratings_s123 <- dplyr::bind_rows(results)
save_rds(partner_ratings_s123, "partner_ratings_s123")
message("  done.")
