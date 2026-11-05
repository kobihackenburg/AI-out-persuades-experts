# si_21_opponents.R
# =============================================================================
# Persuading opponents of the assigned stance (pooled Studies 1-3).
#
#   * Converting opponents: among persuadees who opposed the stance beforehand
#     (pre_attitude < 50), the share who ended up supporting it
#     (post_attitude > 50). Effects come from a linear probability version of
#     the preregistered LMM (outcome = 100 x crossover), so they are
#     percentage-point differences in the crossover rate.
#   * Strong opponents: AI's advantage over each human persuader class among
#     persuadees who strongly opposed the stance (pre_attitude <= 25, <= 10),
#     and among those who also reported high issue knowledge
#     (pre_attitude <= 25 and pre_issue_knowledge >= 70). Preregistered LMM
#     with a study fixed effect, as in si_02_subgroups.R. Also reports mean
#     issue knowledge by prior attitude.
#
# Outputs (under output/results/, consumer after `->`):
#   crossover_rates.rds              -> tab_crossover + 99_extract_numbers
#   crossover_te_by_class.rds        -> tab_crossover
#   crossover_ai_vs_human.rds        -> tab_crossover + 99_extract_numbers
#   strong_opponents.rds             -> tab_strong_opponents
#   strong_opponents_knowledge.rds   -> tab_knowledge_by_prior
#
# Human persuaders are pooled into one "human" level (not the unweighted
# mean of classes used in si_02), and the Constrained AI arm of Study 2 is a
# separate level in every model.
# =============================================================================

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))
source(here::here("code/analysis/_lmm_helpers.R"))

message("\n==== si_21_opponents.R ====")

studies <- load_studies(c("study_1", "study_2", "study_3"))
d_all <- purrr::imap_dfr(studies, ~ dplyr::mutate(.x, study = .y)) |>
  complete_case_filter(label = "si_21") |>
  dplyr::transmute(
    persuadee = persuadee_id,
    persuader = dplyr::case_when(
      !is.na(persuader_id)                            ~ persuader_id,
      persuader_class == "ai" & !is.na(ai_model_full) ~ paste0("ai_", ai_model_full),
      TRUE                                            ~ paste0("solo_", persuadee_session)
    ),
    issue = issue_id, study, pre = pre_attitude, post = post_attitude,
    knowledge = pre_issue_knowledge,
    attempt = dplyr::coalesce(as.integer(persuadee_attempt_number), 1L),
    group = dplyr::case_when(
      persuader_class == "ai" & dplyr::coalesce(ai_constrained, FALSE) ~ "ai_constrained",
      TRUE ~ persuader_class
    )
  ) |>
  dplyr::filter(!is.na(group)) |>
  dplyr::mutate(group2 = dplyr::case_when(
    group %in% c("control", "ai", "ai_constrained") ~ group,
    TRUE ~ "human"
  ))

human_classes <- setdiff(unique(d_all$group), c("control", "ai", "ai_constrained"))
message("  human classes: ", paste(human_classes, collapse = ", "))

# AI minus each level in `others`, from an emmeans grid on `col`.
ai_vs <- function(model, col, others) {
  em <- emmeans::emmeans(model, stats::as.formula(paste("~", col)), lmer.df = "asymptotic")
  lv <- as.character(em@levels[[col]])
  cons <- lapply(others, function(o) {
    v <- setNames(numeric(length(lv)), lv); v["ai"] <- 1; v[o] <- -1; v
  })
  names(cons) <- paste0("ai_minus_", others)
  emmeans::contrast(em, cons) |>
    summary(infer = TRUE, adjust = "none") |>
    tibble::as_tibble() |>
    dplyr::transmute(contrast = as.character(contrast), estimate,
                     lower_ci = asymp.LCL, upper_ci = asymp.UCL, p_value = p.value)
}

# =============================================================================
# 1. Converting opponents (pre < 50 -> post > 50)
# =============================================================================
d_opp <- d_all |>
  dplyr::filter(pre < 50) |>
  dplyr::mutate(post = as.numeric(post > 50) * 100)

crossover_rates <- d_opp |>
  dplyr::group_by(group) |>
  dplyr::summarise(n = dplyr::n(), crossover_pct = mean(post), .groups = "drop")
save_rds(crossover_rates, "crossover_rates")
print(crossover_rates)

m_cls <- fit_pooled_lmm(d_opp, "group", label = "crossover by class")
crossover_te_by_class <- tidy_te_vs_control(m_cls, "group")
save_rds(crossover_te_by_class, "crossover_te_by_class")
print(crossover_te_by_class)

m_pooled <- fit_pooled_lmm(d_opp, "group2", label = "crossover ai vs human")
crossover_ai_vs_human <- dplyr::bind_rows(
  tidy_te_vs_control(m_pooled, "group2"),
  ai_vs(m_pooled, "group2", "human") |> dplyr::rename(group = contrast)
)
save_rds(crossover_ai_vs_human, "crossover_ai_vs_human")
print(crossover_ai_vs_human)

# =============================================================================
# 2. Strong opponents
# =============================================================================
run_cut <- function(label, keep) {
  d <- dplyr::filter(d_all, keep)
  n_tab <- dplyr::count(d, group, name = "n")
  m_c <- fit_pooled_lmm(d, "group",  extra_fe = "study", label = paste(label, "by class"))
  m_p <- fit_pooled_lmm(d, "group2", extra_fe = "study", label = paste(label, "pooled"))
  dplyr::bind_rows(
    tidy_te_vs_control(m_c, "group") |>
      dplyr::transmute(contrast = paste0(group, "_vs_control"), estimate, lower_ci, upper_ci, p_value),
    ai_vs(m_c, "group", human_classes),
    ai_vs(m_p, "group2", "human")
  ) |>
    dplyr::mutate(sample = label, n_conversations = nrow(d),
                  n_persuadees = dplyr::n_distinct(d$persuadee), .before = 1) |>
    dplyr::left_join(
      dplyr::transmute(n_tab, contrast = paste0("ai_minus_", group), n_class = n),
      by = "contrast"
    )
}

strong_knowledgeable <- d_all$pre <= 25 & !is.na(d_all$knowledge) & d_all$knowledge >= 70

strong_opponents <- dplyr::bind_rows(
  run_cut("pre_le_25", d_all$pre <= 25),
  run_cut("pre_le_10", d_all$pre <= 10),
  run_cut("pre_le_25_knowledge_ge_70", strong_knowledgeable)
)
save_rds(strong_opponents, "strong_opponents")
print(strong_opponents, n = Inf, width = Inf)

# Mean self-reported issue knowledge by strength of prior attitude.
knowledge_by_prior <- function(d) {
  dplyr::summarise(d, n = dplyr::n(), mean_knowledge = mean(knowledge, na.rm = TRUE))
}
strong_opponents_knowledge <- list(
  bands = d_all |>
    dplyr::mutate(prior_band = cut(pre, c(-Inf, 10, 25, 50, 75, Inf),
                                   labels = c("0-10", "11-25", "26-50", "51-75", "76-100"))) |>
    dplyr::group_by(prior_band) |>
    knowledge_by_prior(),
  strong   = knowledge_by_prior(dplyr::filter(d_all, pre <= 25)),
  rest     = knowledge_by_prior(dplyr::filter(d_all, pre > 25)),
  pre_q25  = unname(stats::quantile(d_all$pre, 0.25))
)
save_rds(strong_opponents_knowledge, "strong_opponents_knowledge")
print(strong_opponents_knowledge)

message("\n  done.")
