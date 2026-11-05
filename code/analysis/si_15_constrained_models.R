# si_15_constrained_models.R
# =============================================================================
# Constrained AI split by model (Study 2).
#
# The constraint changed message length and waiting time together. All three
# Constrained AI models shared the same delay sampler (mean 92 s) and the same
# calibrated length (~55 words/msg realized), but differed in claims per
# conversation. Do their effects differ, i.e. does information content matter
# with delay and length held fixed?
#
# Preregistered pooled LMM on Studies 1-2, with Constrained AI split by model
# and unconstrained AI split by model (pooled over S1 and S2):
#
#   post ~ pre + issue + group + attempt + (1 | persuader) + (1 | persuadee)
#
# Outputs (under output/results/, consumer after `->`):
#   constrained_models_te.rds         -> tab_constrained_models (effect vs control)
#   constrained_models_contrasts.rds  -> tab_constrained_models (pairwise contrasts)
#   constrained_models_desc.rds       -> tab_constrained_models (words/msg, claims/conv)
# =============================================================================

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))
source(here::here("code/analysis/_lmm_helpers.R"))

message("\n==== si_15_constrained_models.R ====")

studies <- load_studies(c("study_1", "study_2"))
d <- purrr::imap_dfr(studies, ~ dplyr::mutate(.x, study = .y)) |>
  complete_case_filter(label = "si_15 constrained models") |>
  dplyr::transmute(
    persuadee = persuadee_id,
    persuader = dplyr::case_when(
      !is.na(persuader_id)                            ~ persuader_id,
      persuader_class == "ai" & !is.na(ai_model_full) ~ paste0("ai_", ai_model_full),
      TRUE                                            ~ paste0("solo_", persuadee_session)
    ),
    issue = issue_id, pre = pre_attitude, post = post_attitude,
    attempt = dplyr::coalesce(as.integer(persuadee_attempt_number), 1L),
    group = dplyr::case_when(
      persuader_class == "ai" & dplyr::coalesce(ai_constrained, FALSE) ~ paste0("constrained_", ai_model),
      persuader_class == "ai" ~ paste0("ai_", ai_model),
      TRUE ~ persuader_class
    ),
    persuader_words, n_persuader_msgs, fact_checked, n_fact_claims
  ) |>
  dplyr::filter(!is.na(group))
print(dplyr::count(d, group))

# Realized message length and claims per conversation for each Constrained AI
# model: the premise that the models differed in claims but not in length.
constrained_models_desc <- d |>
  dplyr::filter(startsWith(group, "constrained_")) |>
  dplyr::group_by(group) |>
  dplyr::summarise(
    n = dplyr::n(),
    words_per_msg = sum(persuader_words, na.rm = TRUE) / sum(n_persuader_msgs, na.rm = TRUE),
    n_fact_checked = sum(dplyr::coalesce(fact_checked, FALSE)),
    claims_per_conv = mean(n_fact_claims[dplyr::coalesce(fact_checked, FALSE)], na.rm = TRUE),
    .groups = "drop"
  )
save_rds(constrained_models_desc, "constrained_models_desc")
print(constrained_models_desc)

m <- fit_pooled_lmm(d, "group", label = "si_15 constrained models")
constrained_models_te <- tidy_te_vs_control(m, "group")
save_rds(constrained_models_te, "constrained_models_te")
print(constrained_models_te)

em <- emmeans::emmeans(m, ~ group, lmer.df = "asymptotic")
lv <- as.character(em@levels$group)
vec <- function(plus, minus) { v <- setNames(numeric(length(lv)), lv); v[plus] <- 1; v[minus] <- -1; v }
constrained_models_contrasts <- emmeans::contrast(em, list(
  claude_minus_gemini  = vec("constrained_claude",  "constrained_gemini"),
  chatgpt_minus_gemini = vec("constrained_chatgpt", "constrained_gemini"),
  claude_minus_chatgpt = vec("constrained_claude",  "constrained_chatgpt")
)) |> summary(infer = TRUE) |> tibble::as_tibble()
save_rds(constrained_models_contrasts, "constrained_models_contrasts")
print(constrained_models_contrasts)

message("\n  done.")
