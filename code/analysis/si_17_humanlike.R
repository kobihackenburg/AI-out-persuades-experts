# si_17_humanlike.R
# =============================================================================
# Perceived humanness of persuaders. Persuadees were not asked whether their
# partner was human or AI, but after each conversation rated "My conversation
# partner interacted just like a human would" (rating_anthropo, 0-100).
# Studies 1-3, unconstrained AI vs all human persuader classes (control and
# Constrained AI excluded).
#   (1) Ratings by persuader type. Partner type was randomized, so this is a
#       randomized comparison (means, shares, AUC).
#   (2) Within AI and within human conversations, the association of the
#       rating with attitude change.
#   (3) The AI - human difference in attitude change within bands of the
#       rating, with an AI x band likelihood-ratio test.
# The rating is post-treatment, so (2) and (3) condition on a post-treatment
# measure and are associations, not causal effects.
#
#   post ~ pre + issue + study + attempt + ... + (1 | persuader) + (1 | persuadee)
#
# Outputs (under output/results/, consumer after `->`):
#   humanness_separation.rds        -> tab_humanness_ratings
#   humanness_coefs.rds             -> tab_humanness_associations (slopes,
#                                      overall and rating-adjusted AI - human)
#   humanness_bands.rds             -> tab_humanness_associations (bands)
#   humanness_band_interaction.rds  -> tab_humanness_associations (LRT)
# =============================================================================

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))
source(here::here("code/analysis/_lmm_helpers.R"))

message("\n==== si_17_humanlike.R ====")

studies <- load_studies(c("study_1", "study_2", "study_3"))
d <- purrr::imap_dfr(studies, ~ dplyr::mutate(.x, study = .y)) |>
  complete_case_filter(label = "si_17_humanlike") |>
  dplyr::filter(persuader_class != "control",
                !(persuader_class == "ai" & dplyr::coalesce(ai_constrained, FALSE)),
                !is.na(rating_anthropo)) |>
  dplyr::transmute(
    study = factor(study),
    persuadee = persuadee_id,
    persuader = dplyr::if_else(!is.na(persuader_id), persuader_id,
                               paste0("ai_", ai_model_full)),
    issue = issue_id, pre = pre_attitude, post = post_attitude,
    attempt = dplyr::coalesce(as.integer(persuadee_attempt_number), 1L),
    is_ai = as.integer(persuader_class == "ai"),
    anthropo = rating_anthropo
  )
print(dplyr::count(d, is_ai))

# =============================================================================
# 1. Ratings by persuader type (randomized comparison)
# =============================================================================
# AUC: probability that a random human conversation is rated more human-like
# than a random AI conversation (ties count half), via the rank-sum identity.
desc <- d |>
  dplyr::group_by(is_ai) |>
  dplyr::summarise(n = dplyr::n(), mean = mean(anthropo), median = median(anthropo),
                   share_ge75 = mean(anthropo >= 75), share_le25 = mean(anthropo <= 25),
                   .groups = "drop")
r <- rank(d$anthropo)
n1 <- sum(d$is_ai == 0); n0 <- sum(d$is_ai == 1)
auc_human <- (sum(r[d$is_ai == 0]) - n1 * (n1 + 1) / 2) / (n1 * n0)
human_median <- median(d$anthropo[d$is_ai == 0])
ai_at_or_above_human_median <- mean(d$anthropo[d$is_ai == 1] >= human_median)

humanness_separation <- dplyr::mutate(desc, auc_human = auc_human, human_median = human_median,
                                      ai_ge_human_median = ai_at_or_above_human_median)
save_rds(humanness_separation, "humanness_separation")
print(humanness_separation)

# =============================================================================
# 2. Associations with attitude change (conditions on a post-treatment rating)
# =============================================================================
fit <- function(f, data = d, REML = TRUE) {
  lme4::lmer(f, data = data, REML = REML,
             control = lme4::lmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 100000)))
}
coef_row <- function(m, term, label) {
  est <- lme4::fixef(m)[term]; se <- sqrt(diag(as.matrix(vcov(m))))[term]
  tibble::tibble(model = label, term = term, estimate = unname(est),
                 lower_ci = unname(est - 1.96 * se), upper_ci = unname(est + 1.96 * se),
                 p_value = unname(2 * pnorm(-abs(est / se))))
}
base_rhs <- "pre + issue + study + attempt + (1 | persuader) + (1 | persuadee)"

d$anthropo10 <- d$anthropo / 10
m0 <- fit(as.formula(paste("post ~ is_ai +", base_rhs)))
m1 <- fit(as.formula(paste("post ~ is_ai + anthropo10 +", base_rhs)))

d$anthro_ai <- d$anthropo10 * d$is_ai
d$anthro_hu <- d$anthropo10 * (1 - d$is_ai)
m2 <- fit(as.formula(paste("post ~ is_ai + anthro_ai + anthro_hu +", base_rhs)))

humanness_coefs <- dplyr::bind_rows(
  coef_row(m0, "is_ai", "AI - human"),
  coef_row(m1, "is_ai", "AI - human, adj. perceived humanness"),
  coef_row(m1, "anthropo10", "perceived humanness per 10 pts (pooled)"),
  coef_row(m2, "anthro_ai", "perceived humanness per 10 pts, AI convs"),
  coef_row(m2, "anthro_hu", "perceived humanness per 10 pts, human convs")
)
save_rds(humanness_coefs, "humanness_coefs")
print(humanness_coefs)

# =============================================================================
# 3. AI - human difference within bands of the rating
# =============================================================================
d$band <- cut(d$anthropo, c(-Inf, 25, 50, 75, Inf),
              labels = c("0-25", "26-50", "51-75", "76-100"), right = TRUE)
d$is_ai_f <- factor(d$is_ai)
m3 <- fit(as.formula(paste("post ~ is_ai_f * band +", base_rhs)))
humanness_bands <- emmeans::emmeans(m3, ~ is_ai_f | band, lmer.df = "asymptotic") |>
  emmeans::contrast(method = list(ai_minus_human = c(-1, 1))) |>
  summary(infer = TRUE) |> tibble::as_tibble() |>
  dplyr::transmute(band, estimate, lower_ci = asymp.LCL, upper_ci = asymp.UCL, p_value = p.value)
humanness_bands <- dplyr::left_join(humanness_bands,
  d |> dplyr::count(band, is_ai) |> tidyr::pivot_wider(names_from = is_ai, values_from = n,
                                                       names_prefix = "n_ai"), by = "band")
save_rds(humanness_bands, "humanness_bands")
print(humanness_bands)

int_test <- anova(fit(as.formula(paste("post ~ is_ai_f * band +", base_rhs)), REML = FALSE),
                  fit(as.formula(paste("post ~ is_ai_f + band +", base_rhs)), REML = FALSE))
humanness_band_interaction <- tibble::tibble(
  test = "AI x perceived-humanness band (LRT, ML fits)",
  chisq = int_test$Chisq[2], df = int_test$Df[2],
  p_value = int_test$`Pr(>Chisq)`[2])
save_rds(humanness_band_interaction, "humanness_band_interaction")
print(humanness_band_interaction)

message("\n  done.")
