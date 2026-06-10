# 02_mechanism.R
# =============================================================================
# Mechanism analysis ("Why did AI win?") + every RDS that backs the
# mechanism figure (mechanism_figure.{pdf,png}).
#
# Fits LMMs and writes tidy RDS outputs to `output/results/`. The figure
# script (code/figures/main/fig3_mechanism.R) re-loads these without
# re-fitting.
#
# This script's outputs feed two main-text figures:
#   Figure 2 (limits)    panel A: within-study contrast forest + inset
#   Figure 3 (mechanism) panel A: per-item effect of constraining AI on
#                                 partner ratings
#                        panel B: facts-vs-persuasive-impact scatter
# Main-text fits are strictly S1+S2 (no canvassers, no Study 3 AI models;
# the mechanism section is narrated chronologically over Studies 1 and 2).
# The S1-S3 analogues back the SI canvasser-inclusion robustness checks.
#
# Outputs (under output/results/, consumer after `->`):
#   mechanism_vs_coached_contrasts.rds   -> Study 2 contrasts vs the Coached
#                                   Elite Debater reference (SI / extract_numbers
#                                   cite the Coached - AI deltas; not a main
#                                   figure)
#   limits_contrasts.rds   -> Fig 2 panel A forest (vs Elite Debater
#                                   from Study 1; rows: AI (S2), Constrained
#                                   AI, Coached Elite Debater)
#   mechanism_vs_coached_inset.rds       -> inset for the Coached-reference contrasts
#   limits_inset.rds       -> Fig 2 panel A text columns
#                                   (avg. words/msg, avg. response delay s);
#                                   4 rows: Elite Debater (reference),
#                                   Coached Elite Debater, AI, Constrained AI
#   mechanism_constraint_effects.rds
#                                -> Fig 3 panel A forest (per-item effect
#                                    of constraining AI on partner ratings)
#   canvasser_facts_vs_te.rds -> Per-condition fact/TE table from the
#                                    S1-S3 fit (canvasser-inclusion analogue;
#                                    used by SI fig_facts_with_canvasser.R)
#   mechanism_facts_vs_te.rds
#                                -> Per-condition fact/TE table from the
#                                    S1+S2-only fit (no Study 3 anywhere);
#                                    backs the main-text Fig 3b
#   canvasser_facts_regression.rds  -> R^2 annotations from the S1+S2+S3 fit
#                                   (used by the SI canvasser-inclusion figure)
#   mechanism_facts_regression.rds
#                                -> R^2 annotations from the S1+S2-only fit
#                                   (backs the main-text Fig 3b)
#
# =============================================================================
# WHAT THIS SCRIPT FITS
# =============================================================================
# Figure 2 panel A (within-study contrasts vs AI; Studies 1+2):
#   One LMM with the same preregistered RHS as 01_main_attitudes.R, fit on
#   the union of arms the three contrasts touch (Control + Elite Debater
#   (S1) + Coached Elite Debater (S2) + AI (Info-prompted) S1 + AI
#   (Info-prompted) S2 + AI (Constrained) S2). Both AI study cells are kept
#   so the within-S1 and within-S2 contrasts read off a single fit. Reports
#   AI - Elite Debater (S1), AI - Coached Elite Debater (S2), and
#   AI - AI (Constrained) (S2), so the panel reads as "throughput closes
#   the gap". The same fit also yields the "vs Coached Elite Debater"
#   contrasts used by the SI / extract_numbers.
#
# Figure 3 panel A (per-item constraint effect on partner ratings;
# Studies 1+2 only):
#   One LMM per partner-rating item (7 items). RHS:
#     rating ~ pre + issue + study + group_panel_b + attempt
#            + (1 | persuadee)
#   where `group_panel_b` is {Human, AI (Info-prompted), AI (Constrained)}.
#   Reports the (Constrained - AI) contrast per item.
#   Persuader RE deliberately omitted: for the AI arm `persuader_id` is
#   just the model name (3 levels), so treating that as a random sample
#   from a population of AI persuaders inflates the contrast CIs whenever
#   the constrained-AI models disagree on a dimension. Persuadee-side
#   clustering is still absorbed by `(1 | persuadee)`.
#
# Figure 3 panel B (facts vs persuasive impact):
#   Aggregates `n_fact_claims` per condition and OLS-regresses TE on
#   log10(fact density), reporting R^2 overall and within humans / AI. The
#   main-text version (mechanism_facts_vs_te / mechanism_facts_regression)
#   uses the S1+S2-only TE inputs; the S1-S3 canvasser-inclusion analogue
#   (canvasser_facts_vs_te / canvasser_facts_regression) backs the SI
#   figure.
# =============================================================================

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))
source(here::here("code/analysis/_lmm_helpers.R"))

message("\n==== 02_mechanism.R ====")

# =============================================================================
# 1. Constants
# =============================================================================
# Partner-rating items. Persuadees rated their conversations on these
# seven sliders (0-100) at the end of the post-treatment survey in S1, S2
# and S3. Names follow the data column convention.
PARTNER_ITEMS <- c(
  "rating_learning", "rating_arguments",
  "rating_empathy", "rating_enjoyment", "rating_anthropo",
  "rating_deception", "rating_bias"
)

# Panel inset: response delay (seconds) per condition. Info-prompted
# AI replies under ~1 s (no deliberation). The constrained AI's delay is
# the mean of the truncated-normal sampler enforced by the experiment
# platform (92 s) -- chosen to approximate the S1 Elite Debater realized
# mean (95.0 s, computed from per-message timestamps; see manuscript
# Mechanism section). Elite Debater (Study 1) and Coached Elite Debater
# (Study 2) delays are realized means from per-message timestamps,
# pinned here to the manuscript-cited values so the figure inset stays
# stable across reruns. AI per-message `created_at` is a server-side
# artifact and so cannot be used to estimate the realized AI delay
# directly; the sampler mean is the actual design parameter of the
# condition. Words/msg is computed from data below.
PANEL_INSET_DELAYS_S <- list(
  "Elite Debater"         = "95",
  "Coached Elite Debater" = "95",
  "AI"                    = "<1",
  "AI (Constrained)"      = "92"
)

# =============================================================================
# 2. Data assembly (shared)
# =============================================================================
# Same `prep()` shape as 01_main_attitudes.R, but keeps the columns the
# mechanism panels need on top of the LMM design matrix:
#   * partner-rating items                (Panel B)
#   * n_persuader_msgs / persuader_words  (Panel A inset)
#   * n_fact_claims                       (Panel C)
prep <- function(df, study_label) {
  df |>
    complete_case_filter(label = paste("02_mech", study_label)) |>
    dplyr::transmute(
      study              = study_label,
      persuadee          = persuadee_id,
      persuader          = dplyr::case_when(
        !is.na(persuader_id)                            ~ persuader_id,
        persuader_class == "ai" & !is.na(ai_model_full) ~ paste0("ai_", ai_model_full),
        TRUE                                            ~ paste0("solo_", persuadee_session)
      ),
      issue              = issue_id,
      group              = persuader_class,
      pre                = pre_attitude,
      post               = post_attitude,
      attempt            = dplyr::coalesce(as.integer(persuadee_attempt_number), 1L),
      ai_constrained     = ai_constrained,
      ai_model_full      = ai_model_full,
      persuadee_session  = persuadee_session,
      n_persuader_msgs   = n_persuader_msgs,
      persuader_words    = persuader_words,
      # Session-level: TRUE iff at least one persuader message from this
      # session appears in the fact-check data for its role. Used in
      # Panel C to filter out sessions that were never actually
      # fact-checked, so the per-condition mean uses the correct
      # (smaller) denominator.
      fact_checked       = fact_checked,
      n_fact_claims      = n_fact_claims,
      dplyr::across(dplyr::all_of(PARTNER_ITEMS))
    )
}

studies <- load_studies(c("study_1", "study_2", "study_3"))

pooled_df <- dplyr::bind_rows(
  prep(studies$study_1, "study_1"),
  prep(studies$study_2, "study_2"),
  prep(studies$study_3, "study_3")
) |>
  dplyr::mutate(
    study = factor(study),
    group = forcats::fct_relevel(droplevels(group), "control")
  )

# Shared group-relabelling layer: same `group_unified` / `group_by_study`
# / `group_ai_separate` as 01_main_attitudes.R. Kept inline here so 02
# doesn't depend on 01 having run with the same in-memory state; the two
# scripts apply the same labels to the same underlying rows.
mech_df <- pooled_df |>
  dplyr::mutate(
    constrained_flag = dplyr::coalesce(ai_constrained, FALSE),
    group_unified = dplyr::case_when(
      group == "control"                ~ "Control",
      group == "ai" &  constrained_flag ~ "AI (Constrained)",
      group == "ai" & !constrained_flag ~ "AI (Info-prompted)",
      group == "random_lay_person"      ~ "Random Layperson",
      group == "elite_lay_person"       ~ "Elite Layperson",
      group == "canvasser"              ~ "Professional Canvasser",
      group == "elite_debater"          ~ "Elite Debater",
      group == "coached_elite_debater"  ~ "Coached Elite Debater",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::filter(!is.na(group_unified)) |>
  dplyr::mutate(
    group_by_study = dplyr::case_when(
      group_unified == "AI (Info-prompted)" & study == "study_1" ~ "AI (Info-prompted) S1",
      group_unified == "AI (Info-prompted)" & study == "study_2" ~ "AI (Info-prompted) S2",
      group_unified == "AI (Info-prompted)" & study == "study_3" ~ "AI (Info-prompted) S3",
      TRUE ~ group_unified
    )
  )

message(sprintf("  mech_df: %d rows, %d persuaders, %d persuadees, %d studies",
                nrow(mech_df),
                dplyr::n_distinct(mech_df$persuader),
                dplyr::n_distinct(mech_df$persuadee),
                dplyr::n_distinct(mech_df$study)))

# =============================================================================
# 3. Within-study contrasts vs AI (Figure 2 panel A)
# =============================================================================
# Figure 2 panel A plots three randomised within-study contrasts against
# AI (Info-prompted), so the figure no longer mixes S1 and S2 in any
# single bar:
#   * Elite Debater (S1) vs AI (Info-prompted) S1   [circle marker]
#   * Coached Elite Debater (S2) vs AI (Info-prompted) S2   [triangle]
#   * AI (Constrained) (S2) vs AI (Info-prompted) S2   [triangle]
#
# The LMM is fit on the union of every arm those contrasts touch
# (Control + Elite Debater (S1) + Coached Elite Debater (S2) +
# AI (Info-prompted) S1 + AI (Info-prompted) S2 + AI (Constrained) S2).
# Both AI study cells are needed so the within-S1 and within-S2 AI
# contrasts can be read directly off the same fit.
#
# We also emit the "vs Coached Elite Debater" contrast table
# (mechanism_vs_coached_contrasts.rds) from the same fit, used by the SI
# and extract_numbers.

message("\n---- 3. Panel A: within-study contrasts vs AI ----")

# Note: the `panel_b_*` object names here are historical; this block builds
# Figure 2 panel A (within-study contrasts vs AI), not a "panel B".
panel_b_mechanism_df <- mech_df |>
  # Mechanism section is narrated chronologically over Studies 1-2;
  # we strip every S3 row (S3 controls, S3 AI, and S3 canvassers) so
  # the fit has no Study 3 in the data, in the model, or anywhere.
  dplyr::filter(study %in% c("study_1", "study_2")) |>
  dplyr::filter(group_by_study %in% c(
    "Control",
    "Elite Debater",
    "Coached Elite Debater",
    "AI (Info-prompted) S1",
    "AI (Info-prompted) S2",
    "AI (Constrained)"
  )) |>
  dplyr::mutate(group_by_study = factor(group_by_study))

m_panel_b_mechanism <- fit_pooled_lmm(panel_b_mechanism_df,
                                      group_col = "group_by_study",
                                      ref       = "Control",
                                      label     = "limits_contrasts_lmm")

# emmeans wraps level names containing spaces / parens in extra outer
# parens (e.g. "(AI (Info-prompted) S2) - Elite Debater"). Strip the
# " - <ref>" suffix and the outer wrap inline -- the project-wide
# `clean_contrast_name()` helper only knows about Control as the
# reference, so we can't reuse it here.
strip_ref_suffix <- function(x, ref) {
  x <- sub(paste0(" - ", ref, "$"), "", as.character(x))
  sub("^\\((.+)\\)$", "\\1", x)
}

# Primary table: three within-study contrasts vs AI (Info-prompted).
# `display` carries the comparator's name (drives the figure row label);
# `study` carries the comparator's study (drives the figure shape: S1
# circle, S2 triangle).
em_panel_b <- emmeans::emmeans(m_panel_b_mechanism, ~ group_by_study,
                               lmer.df = "asymptotic")
panel_b_lvls <- as.character(em_panel_b@levels$group_by_study)
mk_contrast_vec <- function(plus, minus) {
  v <- numeric(length(panel_b_lvls))
  v[match(plus,  panel_b_lvls)] <-  1
  v[match(minus, panel_b_lvls)] <- -1
  v
}
panel_b_contrast_specs <- tibble::tribble(
  ~contrast_name,                                       ~display,                  ~study,    ~plus,                       ~minus,
  "AI (S1) - Elite Debater (S1)",                       "Elite Debater",           "Study 1", "AI (Info-prompted) S1",     "Elite Debater",
  "AI (S2) - Coached Elite Debater (S2)",               "Coached Elite Debater",   "Study 2", "AI (Info-prompted) S2",     "Coached Elite Debater",
  "AI (S2) - AI (Constrained) (S2)",                    "AI (Constrained)",        "Study 2", "AI (Info-prompted) S2",     "AI (Constrained)"
)
panel_b_contrast_list <- stats::setNames(
  Map(mk_contrast_vec, panel_b_contrast_specs$plus, panel_b_contrast_specs$minus),
  panel_b_contrast_specs$contrast_name
)

limits_contrasts <- emmeans::contrast(em_panel_b,
                                            method = panel_b_contrast_list,
                                            adjust = "none") |>
  summary(infer = TRUE, adjust = "none") |>
  tibble::as_tibble() |>
  dplyr::transmute(
    contrast  = as.character(contrast),
    estimate  = estimate,
    lower_ci  = asymp.LCL,
    upper_ci  = asymp.UCL,
    p_value   = p.value
  ) |>
  dplyr::left_join(
    panel_b_contrast_specs |>
      dplyr::select(contrast = contrast_name, display, study),
    by = "contrast"
  )
save_rds(limits_contrasts, "limits_contrasts")

message("\n  Panel A within-study contrasts vs AI:")
for (i in seq_len(nrow(limits_contrasts))) {
  r <- limits_contrasts[i, ]
  message(sprintf("    %-42s  est = %+5.2f pp  [%+5.2f, %+5.2f]   p = %.3g   (%s)",
                  r$contrast, r$estimate, r$lower_ci, r$upper_ci,
                  r$p_value, r$study))
}

# Contrasts vs Coached Elite Debater (Study 2): same fit, different
# reference. Used by the SI / extract_numbers for the AI - Coached and
# Constrained - Coached deltas.
mechanism_vs_coached_contrasts <- emmeans::emmeans(m_panel_b_mechanism, ~ group_by_study,
                                           lmer.df = "asymptotic") |>
  emmeans::contrast(method = "trt.vs.ctrl",
                    ref    = "Coached Elite Debater") |>
  summary(infer = TRUE, adjust = "none") |>
  tibble::as_tibble() |>
  dplyr::transmute(
    contrast  = strip_ref_suffix(contrast, "Coached Elite Debater"),
    estimate  = estimate,
    lower_ci  = asymp.LCL,
    upper_ci  = asymp.UCL,
    p_value   = p.value
  ) |>
  dplyr::filter(contrast %in% c("AI (Info-prompted) S2", "AI (Constrained)")) |>
  dplyr::mutate(
    display = dplyr::recode(contrast,
                            "AI (Info-prompted) S2" = "AI",
                            "AI (Constrained)"      = "AI (Constrained)")
  )
save_rds(mechanism_vs_coached_contrasts, "mechanism_vs_coached_contrasts")

# Panel A text columns: avg words/message and response delay (s) per
# condition. Words/msg is computed from the data subset used above
# (per-row persuader_words divided by per-row n_persuader_msgs, then
# averaged across rows in that arm); delay (s) is pinned to manuscript
# values via PANEL_INSET_DELAYS_S. Four rows total: Elite Debater (S1),
# Coached Elite Debater (S2), AI (Constrained) (S2), and AI (the new
# footer reference: pooled across S1 and S2 since AI's words/delay
# behaviour is essentially identical in the two studies). Order is
# fixed here; the figure script controls render order independently.
panel_b_inset_rows <- panel_b_mechanism_df |>
  dplyr::filter(group_by_study %in% c(
    "Elite Debater",
    "Coached Elite Debater",
    "AI (Info-prompted) S1",
    "AI (Info-prompted) S2",
    "AI (Constrained)"
  )) |>
  dplyr::mutate(
    words_per_msg = persuader_words / n_persuader_msgs,
    inset_group = dplyr::case_when(
      group_by_study %in% c("AI (Info-prompted) S1",
                            "AI (Info-prompted) S2")  ~ "AI",
      group_by_study == "Elite Debater"                ~ "Elite Debater",
      group_by_study == "Coached Elite Debater"        ~ "Coached Elite Debater",
      group_by_study == "AI (Constrained)"             ~ "AI (Constrained)"
    )
  ) |>
  dplyr::group_by(inset_group) |>
  dplyr::summarise(
    n_conv         = dplyr::n(),
    avg_words_msg  = mean(words_per_msg, na.rm = TRUE),
    .groups        = "drop"
  ) |>
  dplyr::mutate(
    display = inset_group,
    # PANEL_INSET_DELAYS_S is keyed on display label; coerce via [[ so
    # a factor input never silently degrades to integer indexing.
    avg_delay_s   = vapply(display, function(d) PANEL_INSET_DELAYS_S[[d]],
                           character(1L), USE.NAMES = FALSE),
    avg_words_lbl = sprintf("%.0f", avg_words_msg)
  ) |>
  dplyr::select(display, n_conv, avg_words_msg, avg_words_lbl, avg_delay_s)
save_rds(panel_b_inset_rows, "limits_inset")
# Inset for the Coached-reference contrasts: same content as limits_inset
# minus the Elite Debater reference row. Consumed by the SI / extract_numbers.
save_rds(
  panel_b_inset_rows |> dplyr::filter(display != "Elite Debater"),
  "mechanism_vs_coached_inset"
)

# =============================================================================
# 4. Panel B: per-item effect of constraining AI on partner ratings
# =============================================================================

message("\n---- 4. Panel B: per-item constraint effects ----")

# Restrict to the arms Panel B plots: every human class plus AI Info and
# AI Constrained. Collapse the persuader columns into Human / AI / AI
# (Constrained) so the per-item LMMs key off a 3-level factor.
panel_b_df <- mech_df |>
  # Strict S1+S2: the mechanism section is narrated chronologically
  # before the canvasser study is introduced, so the per-item constraint
  # contrast in Fig 3a is fit on Studies 1-2 only. Canvassers
  # (Professional Canvasser is S3-only) and the three Study 3 AI
  # variants drop out automatically.
  dplyr::filter(study %in% c("study_1", "study_2")) |>
  dplyr::filter(group_unified %in% c(
    "Random Layperson", "Elite Layperson",
    "Elite Debater", "Coached Elite Debater",
    "AI (Info-prompted)", "AI (Constrained)"
  )) |>
  dplyr::mutate(
    group_panel_b = dplyr::case_when(
      group_unified == "AI (Info-prompted)" ~ "AI (Info-prompted)",
      group_unified == "AI (Constrained)"   ~ "AI (Constrained)",
      TRUE                                  ~ "Human"
    ),
    group_panel_b = factor(group_panel_b,
                           levels = c("AI (Info-prompted)",
                                      "AI (Constrained)",
                                      "Human"))
  )

fit_partner_item <- function(item) {
  d <- panel_b_df |> dplyr::filter(!is.na(.data[[item]]))
  if (nrow(d) < 50L) return(NULL)
  fm <- stats::as.formula(sprintf(
    "%s ~ pre + issue + study + group_panel_b + attempt + (1 | persuadee)",
    item
  ))
  message(sprintf("  [item=%s] fit: %s (n=%d)", item, deparse1(fm), nrow(d)))
  m <- lme4::lmer(
    fm,
    data    = d,
    REML    = TRUE,
    control = lme4::lmerControl(optimizer = "bobyqa",
                                optCtrl   = list(maxfun = 100000))
  )
  emmeans::emmeans(m, ~ group_panel_b, lmer.df = "asymptotic") |>
    emmeans::contrast(method = "trt.vs.ctrl", ref = "AI (Info-prompted)") |>
    summary(infer = TRUE, adjust = "none") |>
    tibble::as_tibble() |>
    dplyr::transmute(
      item          = item,
      contrast      = clean_contrast_name(contrast),
      estimate      = estimate,
      lower_ci      = asymp.LCL,
      upper_ci      = asymp.UCL,
      p_value       = p.value,
      n_obs         = nrow(d)
    )
}

mechanism_constraint_effects <- purrr::map_dfr(PARTNER_ITEMS, fit_partner_item) |>
  dplyr::mutate(contrast_kind = dplyr::case_when(
    grepl("Constrained", contrast) ~ "Constrained - AI",
    grepl("Human",       contrast) ~ "Human - AI",
    TRUE                           ~ NA_character_
  ))
save_rds(mechanism_constraint_effects, "mechanism_constraint_effects")

# =============================================================================
# 5. Panel C: per-condition mean fact-checkable claims vs TE
# =============================================================================

message("\n---- 5. Panel C: facts vs TE ----")

# 5a. Per-condition mean fact-checkable claims. Aggregation matches the
#     m_ai_sep group definition that 01_main_attitudes.R uses: AI is
#     split by (model x study), AI constrained is split by model only.
#     Humans are kept at the class level.
ai_model_label <- function(x) {
  dplyr::case_when(
    x == "claude-4.1-opus" ~ "Claude Opus 4.1",
    x == "claude-4.6-opus" ~ "Claude Opus 4.6",
    x == "gemini-2.5-pro"  ~ "Gemini 2.5 Pro",
    x == "gpt-4o-latest"   ~ "ChatGPT-4o (latest)",
    x == "gpt-5-4"         ~ "GPT-5.4",
    x == "grok-4"          ~ "Grok 4.20",
    TRUE                   ~ NA_character_
  )
}

fact_per_condition <- mech_df |>
  dplyr::filter(group_unified != "Control") |>
  dplyr::mutate(
    ai_lbl = ai_model_label(ai_model_full),
    condition = dplyr::case_when(
      group == "ai" &  constrained_flag ~ paste0(ai_lbl, " (Constrained)"),
      group == "ai" & !constrained_flag ~ paste0(
        ai_lbl, " (Info) ",
        toupper(sub("study_", "S", study))
      ),
      TRUE                              ~ group_unified
    )
  ) |>
  # Only average over sessions that were actually fact-checked. The
  # fact-check CSVs cover only a fraction of sessions for several
  # conditions (e.g. ~16% of Random Layperson, ~29% of S3 Claude AI),
  # so including unchecked sessions in the denominator would
  # systematically understate the per-condition mean.
  dplyr::filter(fact_checked) |>
  dplyr::group_by(condition) |>
  dplyr::summarise(
    n_conv          = dplyr::n(),
    mean_fact_claims = mean(n_fact_claims, na.rm = TRUE),
    .groups         = "drop"
  )

# 5b. TE estimates per condition: come from 01_main_attitudes.R via
#     canvasser_te_inputs.rds (the tidy m_ai_sep contrast table). We join
#     on the same `condition` string (group label).
panel_c_te <- load_rds("canvasser_te_inputs") |>
  dplyr::transmute(condition = group, estimate, lower_ci, upper_ci)

canvasser_facts_vs_te <- fact_per_condition |>
  dplyr::inner_join(panel_c_te, by = "condition") |>
  dplyr::mutate(
    is_constrained = grepl("\\(Constrained\\)$", condition),
    is_ai_info     = grepl("\\(Info\\) S[123]$", condition),
    type           = dplyr::case_when(
      is_constrained ~ "AI (Constrained)",
      is_ai_info     ~ "AI",
      TRUE           ~ "Human"
    ),
    study = dplyr::case_when(
      grepl(" S1$",  condition)         ~ "Study 1",
      grepl(" S2$",  condition)         ~ "Study 2",
      grepl(" S3$",  condition)         ~ "Study 3",
      is_constrained                    ~ "Study 2",
      condition == "Random Layperson"   ~ "Study 1",
      condition == "Elite Layperson"    ~ "Study 1",
      condition == "Elite Debater"      ~ "Study 1",
      condition == "Coached Elite Debater" ~ "Study 2",
      condition == "Professional Canvasser" ~ "Study 3",
      TRUE                              ~ NA_character_
    )
  ) |>
  dplyr::select(condition, type, study, n_conv,
                mean_fact_claims, estimate, lower_ci, upper_ci)
save_rds(canvasser_facts_vs_te, "canvasser_facts_vs_te")

# 5c. Three OLS regressions on log10(mean_fact_claims): overall,
#     humans-only, AI-only (info + constrained pooled). Each row is one
#     fit; the figure puts the three R^2 values in its upper-left
#     annotation block.
run_ols <- function(d, label) {
  if (nrow(d) < 3L) return(NULL)
  fit  <- stats::lm(estimate ~ log10(mean_fact_claims), data = d)
  s    <- summary(fit)
  tibble::tibble(
    subset      = label,
    n           = nrow(d),
    slope       = stats::coef(fit)[["log10(mean_fact_claims)"]],
    slope_p     = s$coefficients["log10(mean_fact_claims)", "Pr(>|t|)"],
    r_squared   = s$r.squared
  )
}

canvasser_facts_regression <- dplyr::bind_rows(
  run_ols(canvasser_facts_vs_te,                                "Overall"),
  run_ols(canvasser_facts_vs_te |> dplyr::filter(type == "Human"), "Humans only"),
  run_ols(canvasser_facts_vs_te |> dplyr::filter(type %in% c("AI", "AI (Constrained)")),
                                                                    "AI only")
)
save_rds(canvasser_facts_regression, "canvasser_facts_regression")

# 5d. S1+S2-only analogues for the main-text Panel C. We rebuild the
#     facts-vs-TE table from scratch using the S1+S2-only TE inputs
#     (mechanism_te_inputs from 01_main_attitudes.R) so the panel
#     contains no Study 3 data at all -- neither Professional Canvassers
#     nor the Study 3 AI models (Claude 4.6, GPT-5.4, Grok 4.20). The
#     canvasser_facts_vs_te + canvasser_facts_regression objects above
#     are the source of truth for the SI canvasser-inclusion figure.
panel_c_te_s1_s2 <- load_rds("mechanism_te_inputs") |>
  dplyr::transmute(condition = group, estimate, lower_ci, upper_ci)

mechanism_facts_vs_te <- fact_per_condition |>
  dplyr::inner_join(panel_c_te_s1_s2, by = "condition") |>
  dplyr::mutate(
    is_constrained = grepl("\\(Constrained\\)$", condition),
    is_ai_info     = grepl("\\(Info\\) S[12]$", condition),
    type           = dplyr::case_when(
      is_constrained ~ "AI (Constrained)",
      is_ai_info     ~ "AI",
      TRUE           ~ "Human"
    ),
    study = dplyr::case_when(
      grepl(" S1$",  condition)            ~ "Study 1",
      grepl(" S2$",  condition)            ~ "Study 2",
      is_constrained                        ~ "Study 2",
      condition == "Random Layperson"      ~ "Study 1",
      condition == "Elite Layperson"       ~ "Study 1",
      condition == "Elite Debater"         ~ "Study 1",
      condition == "Coached Elite Debater" ~ "Study 2",
      TRUE                                  ~ NA_character_
    )
  ) |>
  dplyr::select(condition, type, study, n_conv,
                mean_fact_claims, estimate, lower_ci, upper_ci)
save_rds(mechanism_facts_vs_te,
         "mechanism_facts_vs_te")

mechanism_facts_regression <- dplyr::bind_rows(
  run_ols(mechanism_facts_vs_te, "Overall"),
  run_ols(mechanism_facts_vs_te |> dplyr::filter(type == "Human"),
          "Humans only"),
  run_ols(mechanism_facts_vs_te |>
            dplyr::filter(type %in% c("AI", "AI (Constrained)")),
          "AI only")
)
save_rds(mechanism_facts_regression, "mechanism_facts_regression")

# =============================================================================
# 6. Audit log
# =============================================================================

message("\n---- 6. audit log ----")

message("  Fig 2 panel A: within-study contrasts vs AI:")
for (i in seq_len(nrow(limits_contrasts))) {
  r <- limits_contrasts[i, ]
  message(sprintf("    %-25s  %+5.2f pp  [%+5.2f, %+5.2f]  (p = %.3g)",
                  r$display, r$estimate, r$lower_ci, r$upper_ci, r$p_value))
}

message("  Contrasts vs Coached Elite Debater (Study 2 reference; SI/numbers):")
for (i in seq_len(nrow(mechanism_vs_coached_contrasts))) {
  r <- mechanism_vs_coached_contrasts[i, ]
  message(sprintf("    %-25s  %+5.2f pp  [%+5.2f, %+5.2f]  (p = %.3g)",
                  r$display, r$estimate, r$lower_ci, r$upper_ci, r$p_value))
}

message("  Fig 2 panel A inset (words/msg from data; delay (s) per manuscript):")
for (i in seq_len(nrow(panel_b_inset_rows))) {
  r <- panel_b_inset_rows[i, ]
  message(sprintf("    %-25s  n = %4d   avg %.0f words/msg   delay = %s s",
                  r$display, r$n_conv, r$avg_words_msg, r$avg_delay_s))
}

message("  Fig 3 panel A (Constrained - AI), sorted by effect (most-negative first):")
panel_b_print <- mechanism_constraint_effects |>
  dplyr::filter(contrast_kind == "Constrained - AI") |>
  dplyr::arrange(estimate)
for (i in seq_len(nrow(panel_b_print))) {
  r <- panel_b_print[i, ]
  message(sprintf("    %-18s  %+6.2f pp  [%+6.2f, %+6.2f]  (n = %d)",
                  r$item, r$estimate, r$lower_ci, r$upper_ci, r$n_obs))
}

message("  Facts/conv, canvasser-inclusion (averaged over fact-checked sessions only):")
for (i in seq_len(nrow(canvasser_facts_vs_te))) {
  r <- canvasser_facts_vs_te[i, ]
  message(sprintf("    %-38s n_checked = %4d   mean = %5.2f facts/conv",
                  r$condition, r$n_conv, r$mean_fact_claims))
}

message("  Facts OLS (S1+S2+S3; backs SI fig_facts_with_canvasser):")
for (i in seq_len(nrow(canvasser_facts_regression))) {
  r <- canvasser_facts_regression[i, ]
  message(sprintf("    %-12s  n = %2d   slope = %+5.2f (p = %.3g)   R^2 = %.3f",
                  r$subset, r$n, r$slope, r$slope_p, r$r_squared))
}

message("  Facts OLS (S1+S2 only; backs main-text Fig 3b):")
for (i in seq_len(nrow(mechanism_facts_regression))) {
  r <- mechanism_facts_regression[i, ]
  message(sprintf("    %-12s  n = %2d   slope = %+5.2f (p = %.3g)   R^2 = %.3f",
                  r$subset, r$n, r$slope, r$slope_p, r$r_squared))
}

message("\n  done.")
