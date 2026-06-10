# 99_extract_numbers.R
# Reads the RDS files in output/results/ and writes output/results/_numbers.json:
# a flat map from stable string keys to scalar values. Every number quoted in
# the manuscript and SI (effect sizes, confidence intervals, p-values,
# R-squared, sample sizes) is keyed here so the text never hand-copies a value.
#
# Each block below guards on the RDS it needs, loads it, and appends one
# namespace of keys; key meanings are documented inline at the block that
# emits them. The Makefile runs this script last, after every analysis and SI
# script has written its RDS outputs.

source(here::here("code/analysis/_setup.R"))

message("\n==== 99_extract_numbers.R ====")

nums <- list()

# ---- helpers ------------------------------------------------------------
# have_rds(): does output/results/<name>.rds exist? Lets each block guard its
# inputs without spelling out the file path twice.
have_rds <- function(name) file.exists(file.path(RESULTS, paste0(name, ".rds")))

# emit_ci(): append the standard estimate / lower_ci / upper_ci / p_value
# quartet under <prefix>.<field>, returning the updated list.
emit_ci <- function(nums, prefix, estimate, lower_ci, upper_ci, p_value) {
  nums[[paste0(prefix, ".estimate")]] <- estimate
  nums[[paste0(prefix, ".lower_ci")]] <- lower_ci
  nums[[paste0(prefix, ".upper_ci")]] <- upper_ci
  nums[[paste0(prefix, ".p_value")]]  <- p_value
  nums
}

# ---- pooled S1-S3 model fixed-effects + Ns ------------------------------
if (have_rds("pooled_s1_s3_lmm")) {
  m  <- load_rds("pooled_s1_s3_lmm")
  fe <- summary(m)$coefficients
  group_rows <- grep("^group", rownames(fe), value = TRUE)
  for (r in group_rows) {
    nums[[sprintf("pooled_lmm.fixef.%s.estimate", r)]] <- unname(fe[r, "Estimate"])
    nums[[sprintf("pooled_lmm.fixef.%s.se",       r)]] <- unname(fe[r, "Std. Error"])
    nums[[sprintf("pooled_lmm.fixef.%s.t",        r)]] <- unname(fe[r, "t value"])
    if ("Pr(>|t|)" %in% colnames(fe)) {
      nums[[sprintf("pooled_lmm.fixef.%s.p", r)]] <- unname(fe[r, "Pr(>|t|)"])
    }
  }
  nums$pooled_lmm.n_obs       <- nobs(m)
  nums$pooled_lmm.n_persuader <- lme4::ngrps(m)[["persuader"]]
  nums$pooled_lmm.n_persuadee <- lme4::ngrps(m)[["persuadee"]]

  # AI-minus-human contrasts. Computed via emmeans with `ref = "ai"` (gives
  # "human - ai") and then sign-flipped so positive values mean AI more
  # persuasive than the human class. The "control - ai" row is dropped.
  em <- emmeans::emmeans(m, ~ group, lmer.df = "asymptotic")
  ai_vs <- emmeans::contrast(em, method = "trt.vs.ctrl", ref = "ai") |>
    summary(infer = TRUE, adjust = "none") |>
    tibble::as_tibble() |>
    dplyr::mutate(
      contrast = sub(" - ai$", "", as.character(contrast)),
      contrast = sub("^\\((.+)\\)$", "\\1", contrast),
      estimate = -estimate,                             # flip sign
      lo       = -asymp.UCL,                            # flipping swaps L/U
      hi       = -asymp.LCL
    ) |>
    dplyr::filter(contrast != "control")
  for (i in seq_len(nrow(ai_vs))) {
    cls <- ai_vs$contrast[i]
    nums <- emit_ci(nums, sprintf("pooled.ai_minus.%s", cls),
                    ai_vs$estimate[i], ai_vs$lo[i], ai_vs$hi[i], ai_vs$p.value[i])
  }
}

# ---- per-class treatment-vs-control contrasts (from robustness_main_spec)
# `robustness_main_spec.rds` is the tidy `tidy_te_vs_control()` table for
# the pooled body-text model; one row per non-control class.
if (have_rds("robustness_main_spec")) {
  tev <- load_rds("robustness_main_spec")
  for (i in seq_len(nrow(tev))) {
    cls <- tev$group[i]
    nums <- emit_ci(nums, sprintf("pooled.te.%s", cls),
                    tev$estimate[i], tev$lower_ci[i], tev$upper_ci[i], tev$p_value[i])
  }
}

# ---- Fig 2b AI reference scalar (dashed red line) -----------------------
if (have_rds("canvasser_ai_reference")) {
  nums$pooled.ai_reference_pp <- as.numeric(load_rds("canvasser_ai_reference"))
}

# ---- Fig 2b per-class between-persuader SDs -----------------------------
if (have_rds("canvasser_class_distributions")) {
  cd <- load_rds("canvasser_class_distributions")
  for (i in seq_len(nrow(cd))) {
    rc <- cd$raw_class[i]
    nums[[sprintf("pooled.tau.%s.raw",       rc)]] <- cd$model_estimated_sd_raw[i]
    nums[[sprintf("pooled.tau.%s.displayed", rc)]] <- cd$model_estimated_sd[i]
  }
}

# ---- N unique human persuaders ------------------------------------------
# Backs the Results statement on the total number of individual human
# persuaders. per_persuader_re has one row per random-effect group: real
# human IDs for human classes, "ai_<model>" for AI, "solo_<session>" for
# controls.
if (have_rds("per_persuader_re")) {
  pre <- load_rds("per_persuader_re")
  nums$pooled.n_human_persuaders <- pre |>
    dplyr::filter(!is.na(group), !group %in% c("ai", "control")) |>
    nrow()
}

# ---- Fig 2b per-persuader (empirical) and parametric tail probability ---
# Two complementary checks of the Results claim that no individual human
# persuader was more persuasive than AI:
#   1. Empirical: count how many of the per-(persuader x class) BLUP-based
#      estimates exceed AI. Computed as class_FE + BLUP. The same coached
#      human appears twice (once under elite_debater, once under
#      coached_elite_debater) because the pooled model has one BLUP per
#      persuader_id shared across classes; we keep both rows because the
#      "same human under different class FE" yields two distinct estimates
#      that the manuscript narrative cares about separately.
#   2. Parametric: per class, compute the predicted probability that a
#      hypothetical new persuader drawn from that class's implied Normal
#      (Fig 2b curve) would exceed AI. This gives a directly quantitative
#      claim that is unambiguous under any sensible sigma.
if (have_rds("per_persuader_re") &&
    have_rds("robustness_main_spec") &&
    have_rds("canvasser_ai_reference") &&
    have_rds("canvasser_class_distributions")) {

  ai_ref <- as.numeric(load_rds("canvasser_ai_reference"))
  cd     <- load_rds("canvasser_class_distributions")
  pre    <- load_rds("per_persuader_re")
  class_te <- load_rds("robustness_main_spec") |>
    dplyr::transmute(raw_class = group, class_estimate = estimate)

  human_classes <- c("random_lay_person", "elite_lay_person", "canvasser",
                     "elite_debater", "coached_elite_debater")

  per_persuader_emp <- pre |>
    dplyr::filter(!is.na(group), group %in% human_classes,
                  !grepl("^solo_|^ai_", persuader)) |>
    dplyr::transmute(persuader, raw_class = group, blup = re_intercept) |>
    dplyr::inner_join(class_te, by = "raw_class") |>
    dplyr::mutate(estimate  = class_estimate + blup,
                  gap_to_ai = estimate - ai_ref)

  nums$panelb.empirical.n_humans               <- nrow(per_persuader_emp)
  nums$panelb.empirical.n_unique_persuader_ids <- dplyr::n_distinct(per_persuader_emp$persuader)
  nums$panelb.empirical.n_above_ai             <- sum(per_persuader_emp$estimate > ai_ref)
  argmax_row <- per_persuader_emp[which.max(per_persuader_emp$estimate), ]
  nums$panelb.empirical.max_estimate_pp        <- argmax_row$estimate
  nums$panelb.empirical.min_gap_to_ai_pp       <- ai_ref - argmax_row$estimate
  nums$panelb.empirical.argmax_class           <- argmax_row$raw_class

  # Parametric: P(new persuader from class C > AI) = pnorm(ai, mu_c,
  # sigma_c, lower.tail = FALSE). sigma_c uses the *displayed* tau
  # (post-fallback for Elite Debater) so the number matches the curve
  # the reader actually sees in Fig 2b.
  pe_tbl <- tibble::tibble(
    raw_class = cd$raw_class,
    mu        = cd$group_te,
    sigma     = cd$model_estimated_sd,
    p_exceed  = stats::pnorm(ai_ref, mean = cd$group_te,
                             sd = cd$model_estimated_sd, lower.tail = FALSE)
  )
  for (i in seq_len(nrow(pe_tbl))) {
    nums[[sprintf("panelb.p_exceed_ai.%s", pe_tbl$raw_class[i])]] <- pe_tbl$p_exceed[i]
  }
  worst <- pe_tbl[which.max(pe_tbl$p_exceed), ]
  nums$panelb.p_exceed_ai.max_class <- worst$raw_class
  nums$panelb.p_exceed_ai.max_pct   <- 100 * worst$p_exceed
}

# =============================================================================
# S1+S2-only limits + mechanism numbers (Figures 2-3 + the "Under what conditions" prose)
# =============================================================================
# The mechanism section in the main text is narrated chronologically over
# Studies 1 and 2 only -- Study 3 (Professional Canvassers) is introduced
# later. Every number cited in that section therefore comes from a fit
# that excludes Study 3 entirely. The S1-S3 analogues above are reused
# for the SI canvasser-inclusion robustness check.

# ---- S1+S2 pooled AI reference (Fig 2b red dashed line) ----------------
if (have_rds("limits_ai_reference")) {
  nums$limits.ai_reference_pp <-
    as.numeric(load_rds("limits_ai_reference"))
}

# ---- S1+S2 per-class between-persuader SDs ------------------------------
if (have_rds("limits_class_distributions")) {
  cd <- load_rds("limits_class_distributions")
  for (i in seq_len(nrow(cd))) {
    rc <- cd$raw_class[i]
    nums[[sprintf("limits.tau.%s.raw",       rc)]] <- cd$model_estimated_sd_raw[i]
    nums[[sprintf("limits.tau.%s.displayed", rc)]] <- cd$model_estimated_sd[i]
    nums[[sprintf("limits.te.%s",            rc)]] <- cd$group_te[i]
  }
}

# ---- S1+S2 N unique human persuaders ------------------------------------
if (have_rds("limits_per_persuader_re")) {
  pre <- load_rds("limits_per_persuader_re")
  nums$limits.n_human_persuaders <- pre |>
    dplyr::filter(!is.na(group), !group %in% c("ai", "control")) |>
    nrow()
  nums$limits.n_unique_humans <- pre |>
    dplyr::filter(!is.na(group), !group %in% c("ai", "control")) |>
    dplyr::pull(persuader) |>
    dplyr::n_distinct()
}

# ---- S1+S2 empirical + parametric tail probability ----------------------
# Empirical: how many of the per-persuader BLUP-based estimates from the
# S1+S2 pooled fit exceed the S1+S2 pooled AI reference. Each persuader
# contributes one row; the 43 Coached Elite Debaters appear twice (under
# elite_debater pre-coaching and coached_elite_debater post-coaching)
# because the pooled model has one BLUP per persuader_id shared across
# classes, yielding one row per (persuader_id, class) cell.
if (have_rds("limits_per_persuader_re") &&
    have_rds("pooled_s1_s2_lmm") &&
    have_rds("limits_ai_reference") &&
    have_rds("limits_class_distributions")) {

  ai_ref <- as.numeric(load_rds("limits_ai_reference"))
  cd     <- load_rds("limits_class_distributions")
  pre    <- load_rds("limits_per_persuader_re")

  # The class-level TE from the S1+S2 pooled fit -- this is the FE we
  # add to each BLUP. Pulled fresh here (cheaper than refitting the same
  # contrast in 01_main_attitudes.R as a separate RDS) by re-running the
  # vs-control contrast on pooled_s1_s2_lmm.
  m_s12 <- load_rds("pooled_s1_s2_lmm")
  em_s12 <- tryCatch(
    emmeans::emmeans(m_s12, ~ group, lmer.df = "asymptotic", nesting = NULL),
    error = function(e) NULL
  )
  s12_class_te <- if (!is.null(em_s12)) {
    grps <- as.character(em_s12@levels$group)
    ctrl_idx <- which(grps == "control")
    tibble::tibble(
      raw_class      = grps,
      class_estimate = as.numeric(stats::predict(em_s12) -
                                    stats::predict(em_s12)[ctrl_idx])
    ) |>
      dplyr::filter(raw_class != "control")
  } else NULL

  human_classes_s1_s2 <- c("random_lay_person", "elite_lay_person",
                           "elite_debater", "coached_elite_debater")

  if (!is.null(s12_class_te)) {
    per_persuader_emp <- pre |>
      dplyr::filter(!is.na(group),
                    group %in% human_classes_s1_s2,
                    !grepl("^solo_|^ai_", persuader)) |>
      dplyr::transmute(persuader, raw_class = group, blup = re_intercept) |>
      dplyr::inner_join(s12_class_te, by = "raw_class") |>
      dplyr::mutate(estimate  = class_estimate + blup,
                    gap_to_ai = estimate - ai_ref)

    nums$limits.empirical.n_humans               <- nrow(per_persuader_emp)
    nums$limits.empirical.n_unique_persuader_ids <- dplyr::n_distinct(per_persuader_emp$persuader)
    nums$limits.empirical.n_above_ai             <- sum(per_persuader_emp$estimate > ai_ref)
    argmax_row <- per_persuader_emp[which.max(per_persuader_emp$estimate), ]
    nums$limits.empirical.max_estimate_pp        <- argmax_row$estimate
    nums$limits.empirical.min_gap_to_ai_pp       <- ai_ref - argmax_row$estimate
    nums$limits.empirical.argmax_class           <- argmax_row$raw_class
  }

  pe_tbl <- tibble::tibble(
    raw_class = cd$raw_class,
    mu        = cd$group_te,
    sigma     = cd$model_estimated_sd,
    p_exceed  = stats::pnorm(ai_ref, mean = cd$group_te,
                             sd = cd$model_estimated_sd, lower.tail = FALSE)
  )
  for (i in seq_len(nrow(pe_tbl))) {
    nums[[sprintf("limits.p_exceed_ai.%s", pe_tbl$raw_class[i])]] <- pe_tbl$p_exceed[i]
  }
  worst <- pe_tbl[which.max(pe_tbl$p_exceed), ]
  nums$limits.p_exceed_ai.max_class <- worst$raw_class
  nums$limits.p_exceed_ai.max_pct   <- 100 * worst$p_exceed
}

# ---- S1+S2 unshrunken per-persuader fixed-effect estimates --------------
# Produced by code/analysis/si_10_raw_per_persuader.R. Companion to the EB BLUP
# numbers above (limits.empirical.*). Body-text sentence reports K1/K2/K3
# and p_min from the lenient one-sided test against the AI reference.
if (have_rds("si_raw_per_persuader") &&
    have_rds("limits_ai_reference")) {
  raw_pp <- load_rds("si_raw_per_persuader")
  ai_ref <- as.numeric(load_rds("limits_ai_reference"))
  nums$limits.raw_per_persuader.n_total            <- as.integer(nrow(raw_pp))
  nums$limits.raw_per_persuader.ai_ref_pp          <- ai_ref
  nums$limits.raw_per_persuader.n_above_ai         <- as.integer(sum(raw_pp$estimate    > ai_ref))
  nums$limits.raw_per_persuader.n_lower_ci_above_ai <- as.integer(sum(raw_pp$lower_ci    > ai_ref))
  nums$limits.raw_per_persuader.n_p_lt_05          <- as.integer(sum(raw_pp$p_one_sided < 0.05))
  nums$limits.raw_per_persuader.p_min              <- min(raw_pp$p_one_sided)
  argmax_row <- raw_pp[which.max(raw_pp$estimate), ]
  nums$limits.raw_per_persuader.max_estimate_pp    <- argmax_row$estimate
  nums$limits.raw_per_persuader.argmax_class       <- argmax_row$group
}

# ---- S1+S2 per-issue AI vs pooled-humans (mechanism section) ------------
if (have_rds("issue_level_ai_vs_humans_s1_s2")) {
  iv <- load_rds("issue_level_ai_vs_humans_s1_s2")
  nums$issues_s1_s2.n_total       <- as.integer(nrow(iv))
  nums$issues_s1_s2.n_sig_p_lt_05 <- as.integer(sum(iv$p_value < 0.05, na.rm = TRUE))
  sig <- iv |> dplyr::filter(p_value < 0.05)
  if (nrow(sig) > 0L) {
    nums$issues_s1_s2.estimate_min_pp <- min(sig$estimate)
    nums$issues_s1_s2.estimate_max_pp <- max(sig$estimate)
  }
  nonsig <- iv |> dplyr::filter(p_value >= 0.05)
  nums$issues_s1_s2.nonsig_issue_ids <-
    paste(as.character(nonsig$issue_id), collapse = ", ")
}

# ---- S1+S2 per-subgroup AI vs pooled-humans (mechanism section) ---------
if (have_rds("subgroups_ai_vs_humans_s1_s2")) {
  sv <- load_rds("subgroups_ai_vs_humans_s1_s2")
  nums$subgroups_s1_s2.n_variables  <- as.integer(dplyr::n_distinct(sv$subgroup))
  nums$subgroups_s1_s2.n_levels     <- as.integer(nrow(sv))
  nums$subgroups_s1_s2.n_sig_p_lt_05 <-
    as.integer(sum(sv$p_value < 0.05, na.rm = TRUE))
  sig <- sv |> dplyr::filter(p_value < 0.05)
  if (nrow(sig) > 0L) {
    nums$subgroups_s1_s2.estimate_min_pp <- min(sig$estimate)
    nums$subgroups_s1_s2.estimate_max_pp <- max(sig$estimate)
  }
}

# ---- S1+S2 moderator interactions (mechanism section) -------------------
if (have_rds("moderator_effects_s1_s2")) {
  me <- load_rds("moderator_effects_s1_s2")
  for (i in seq_len(nrow(me))) {
    stem <- sprintf("mod_s1_s2.%s.%s", me$moderator[i], me$level[i])
    nums <- emit_ci(nums, stem, me$estimate[i], me$lower_ci[i],
                    me$upper_ci[i], me$p_value[i])
  }
  per_mod_p <- me |>
    dplyr::group_by(moderator) |>
    dplyr::summarise(p_int = dplyr::first(p_interaction),
                     q_bh  = dplyr::first(q_bh), .groups = "drop")
  for (i in seq_len(nrow(per_mod_p))) {
    nums[[sprintf("mod_s1_s2.%s.p_interaction", per_mod_p$moderator[i])]] <-
      per_mod_p$p_int[i]
    nums[[sprintf("mod_s1_s2.%s.q_bh",          per_mod_p$moderator[i])]] <-
      per_mod_p$q_bh[i]
  }
}

# ---- attrition rates ----------------------------------------------------
# Post-treatment attrition (matched-only denominator). Pre-treatment dropout
# rates are emitted as a separate `pre_treatment_dropout.*` namespace so
# manuscript prose can cite both transparently.
if (have_rds("attrition")) {
  a <- load_rds("attrition")
  for (i in seq_len(nrow(a$per_study))) {
    s <- a$per_study$study[i]
    nums[[sprintf("attrition.%s.pct", s)]]         <- a$per_study$pct_attrited[i]
    nums[[sprintf("attrition.%s.n_matched", s)]]   <- a$per_study$n_matched[i]
    nums[[sprintf("attrition.%s.n_attrited", s)]]  <- a$per_study$n_attrited[i]
    nums[[sprintf("attrition.%s.n_assigned", s)]]  <- a$per_study$n_assigned[i]
  }
  for (i in seq_len(nrow(a$differential_tests))) {
    s <- a$differential_tests$study[i]
    nums[[sprintf("attrition_differential.%s.p", s)]] <- a$differential_tests$p.value[i]
  }
  if (!is.null(a$pre_treatment) && !is.null(a$pre_treatment$per_study)) {
    pt <- a$pre_treatment$per_study
    for (i in seq_len(nrow(pt))) {
      s <- pt$study[i]
      nums[[sprintf("pre_treatment_dropout.%s.pct", s)]]   <- pt$pct_pre_dropout[i]
      nums[[sprintf("pre_treatment_dropout.%s.n", s)]]     <- pt$n_pre_dropout[i]
      nums[[sprintf("pre_treatment_dropout.%s.match_rate", s)]] <- pt$match_rate[i]
    }
  }
}

# ---- Fig 2a within-study contrasts vs AI (Info-prompted) ---------------
# Primary table for Figure 2 panel A. One row per comparator; AI is the
# (within-study) reference. The three within-study contrasts are:
#   * Elite Debater (S1) vs AI-S1            -> te_ai_vs_ed_s1
#   * Coached Elite Debater (S2) vs AI-S2    -> te_ai_vs_coached_s2
#   * AI (Constrained) (S2) vs AI-S2         -> te_ai_vs_constrained_s2
# Estimates are signed AI - comparator (positive = AI advantage).
if (have_rds("limits_contrasts")) {
  pb <- load_rds("limits_contrasts")
  key_for_b <- function(disp) {
    if (disp == "Elite Debater")              "te_ai_vs_ed_s1"
    else if (disp == "Coached Elite Debater") "te_ai_vs_coached_s2"
    else if (disp == "AI (Constrained)")      "te_ai_vs_constrained_s2"
    else NA_character_
  }
  for (i in seq_len(nrow(pb))) {
    stem <- key_for_b(pb$display[i])
    if (is.na(stem)) next
    nums <- emit_ci(nums, sprintf("limits.panel_a.%s", stem),
                    pb$estimate[i], pb$lower_ci[i], pb$upper_ci[i], pb$p_value[i])
  }
}

# ---- Fig 2a contrasts vs Coached Elite Debater (Study 2 reference) ------
# Study 2 contrasts measured against the Coached Elite Debater reference.
# Source of the "Constrained AI - Coached" near-tie cited in the SI; the
# main-text figure uses the within-study limits.panel_a.te_ai_vs_*.* keys
# above.
if (have_rds("mechanism_vs_coached_contrasts")) {
  pa <- load_rds("mechanism_vs_coached_contrasts")
  key_for <- function(disp) {
    if (disp == "AI")                "te_ai_vs_coached"
    else if (disp == "AI (Constrained)") "te_constrained_vs_coached"
    else NA_character_
  }
  for (i in seq_len(nrow(pa))) {
    stem <- key_for(pa$display[i])
    if (is.na(stem)) next
    nums <- emit_ci(nums, sprintf("limits.panel_a.%s", stem),
                    pa$estimate[i], pa$lower_ci[i], pa$upper_ci[i], pa$p_value[i])
  }
}

# ---- Fig 2a inset, all arms (realized words/msg + response delay) -------
# Exposes all 4 rows: ai, constrained, coached, elite_debater.
if (have_rds("limits_inset")) {
  pin <- load_rds("limits_inset")
  inset_key <- function(disp) {
    if (disp == "AI")                       "ai"
    else if (disp == "AI (Constrained)")    "constrained"
    else if (disp == "Coached Elite Debater") "coached"
    else if (disp == "Elite Debater")       "elite_debater"
    else NA_character_
  }
  for (i in seq_len(nrow(pin))) {
    arm <- inset_key(pin$display[i])
    if (is.na(arm)) next
    nums[[sprintf("limits.inset.%s.avg_words_msg", arm)]] <- pin$avg_words_msg[i]
    # avg_delay_s is character ("<1" / "95") and intentionally so -- the
    # "<1" value is a manuscript-level descriptor, not a numeric mean.
    nums[[sprintf("limits.inset.%s.avg_delay_s",   arm)]] <- pin$avg_delay_s[i]
  }
}

# ---- Fig 2a inset vs Coached Elite Debater (Study 2 reference) ----------
if (have_rds("mechanism_vs_coached_inset")) {
  pin <- load_rds("mechanism_vs_coached_inset")
  inset_key <- function(disp) {
    if (disp == "AI")                 "ai"
    else if (disp == "AI (Constrained)") "constrained"
    else NA_character_
  }
  for (i in seq_len(nrow(pin))) {
    arm <- inset_key(pin$display[i])
    if (is.na(arm)) next
    nums[[sprintf("limits.panel_a.inset.%s.avg_words_msg", arm)]] <- pin$avg_words_msg[i]
    nums[[sprintf("limits.panel_a.inset.%s.avg_delay_s",   arm)]] <- pin$avg_delay_s[i]
  }
}

# ---- Fig 3b facts/conv summaries ----------------------------------------
# Coached Elite Debater is the strongest human comparator; the two AI
# stems are session-weighted means (weighted by n_conv) across the AI
# sub-conditions that appear in Fig 3b. Matches the "approximately 37"
# / "approximately 12" descriptors in the manuscript and the
# "approximately 4.5" Coached value.
if (have_rds("canvasser_facts_vs_te")) {
  fc <- load_rds("canvasser_facts_vs_te")
  coached <- fc |> dplyr::filter(condition == "Coached Elite Debater")
  if (nrow(coached) == 1L) {
    nums$mechanism.facts.coached_elite_debater_per_conv <- coached$mean_fact_claims
  }
  ai_info_s2 <- fc |> dplyr::filter(type == "AI", study == "Study 2")
  if (nrow(ai_info_s2) >= 1L) {
    nums$mechanism.facts.ai_info_s2_weighted_per_conv <-
      stats::weighted.mean(ai_info_s2$mean_fact_claims, ai_info_s2$n_conv)
  }
  ai_con <- fc |> dplyr::filter(type == "AI (Constrained)")
  if (nrow(ai_con) >= 1L) {
    nums$mechanism.facts.ai_constrained_weighted_per_conv <-
      stats::weighted.mean(ai_con$mean_fact_claims, ai_con$n_conv)
  }
}

# ---- Fig 3b OLS R^2 (overall / humans / ai) -----------------------------
# Main-text Figure 3b excludes Professional Canvassers and is backed by
# mechanism_facts_regression.rds; canvasser_facts_regression.rds (with
# canvassers) backs the SI canvasser-inclusion scatter. Both R^2 trios are
# exposed under separate key stems.
r2_key <- function(subset) {
  if (subset == "Overall")     "overall"
  else if (subset == "Humans only") "humans"
  else if (subset == "AI only")     "ai"
  else NA_character_
}
if (have_rds("mechanism_facts_regression")) {
  pr <- load_rds("mechanism_facts_regression")
  for (i in seq_len(nrow(pr))) {
    k <- r2_key(pr$subset[i])
    if (is.na(k)) next
    nums[[sprintf("mechanism.r2.%s", k)]] <- pr$r_squared[i]
  }
}
if (have_rds("canvasser_facts_regression")) {
  pr <- load_rds("canvasser_facts_regression")
  for (i in seq_len(nrow(pr))) {
    k <- r2_key(pr$subset[i])
    if (is.na(k)) next
    nums[[sprintf("mechanism.r2_with_canvasser.%s", k)]] <- pr$r_squared[i]
  }
}

# ---- AI-vs-human gap net of log fact density (mechanism section) --------
# Produced by code/analysis/si_07_ai_gap_net_of_facts.R. Backs the body-text
# claim that fact density appears to account for essentially all of AI's
# persuasive advantage over human persuaders. Four specs:
#   all_ai             = lumps Constrained + info-prompted AI vs all
#                        humans (S1+S2 sample; headline claim).
#   unconstrained_ai   = drops Constrained AI rows so the contrast is
#                        Unconstrained AI vs humans (S1+S2 sample).
#   all_ai_with_s3     = same as all_ai but with Study 3 (Professional
#                        Canvassers + S3 AI models) added; backs the
#                        canvasser-robustness parenthetical.
#   unconstrained_ai_with_s3 = same as unconstrained_ai but with
#                              Study 3 added.
# Per-doubling-of-facts effect = log(2) * beta_log_n_facts.
if (have_rds("si_ai_gap_net_of_facts")) {
  ag <- load_rds("si_ai_gap_net_of_facts")
  spec_key <- function(s) {
    if (s == "all_ai_vs_humans")                  "all_ai"
    else if (s == "unconstrained_ai_vs_humans")   "unconstrained_ai"
    else if (s == "all_ai_vs_humans_with_s3")     "all_ai_with_s3"
    else if (s == "unconstrained_ai_vs_humans_with_s3") "unconstrained_ai_with_s3"
    else NA_character_
  }
  for (i in seq_len(nrow(ag))) {
    k <- spec_key(ag$spec[i])
    if (is.na(k)) next
    stem <- sprintf("mechanism.ai_gap_net_of_facts.%s", k)
    nums[[paste0(stem, ".n_obs")]]                 <- as.integer(ag$n_obs[i])
    nums[[paste0(stem, ".beta_is_human_pp")]]      <- ag$beta_is_human[i]
    nums[[paste0(stem, ".se_is_human")]]           <- ag$se_is_human[i]
    nums[[paste0(stem, ".lower_is_human_pp")]]     <- ag$lower_is_human[i]
    nums[[paste0(stem, ".upper_is_human_pp")]]     <- ag$upper_is_human[i]
    nums[[paste0(stem, ".p_is_human")]]            <- ag$p_is_human[i]
    nums[[paste0(stem, ".beta_log_n_facts_pp")]]   <- ag$beta_log_n_facts[i]
    nums[[paste0(stem, ".p_log_n_facts")]]         <- ag$p_log_n_facts[i]
    nums[[paste0(stem, ".pp_per_doubling_facts")]] <- log(2) * ag$beta_log_n_facts[i]
    nums[[paste0(stem, ".pp_per_doubling_lower")]] <- log(2) * ag$lower_log_n_facts[i]
    nums[[paste0(stem, ".pp_per_doubling_upper")]] <- log(2) * ag$upper_log_n_facts[i]
  }
}

# ---- Fig 4a contrasts (Study 4 LMM) -------------------------------------
# `realworld_te_vs_control.rds` is the tidy emmeans trt.vs.ctrl table
# (one row per non-control arm); `realworld_ai_vs_canvasser.rds` is
# the preregistered single-row custom contrast.
if (have_rds("realworld_te_vs_control")) {
  pa <- load_rds("realworld_te_vs_control")
  te_stem <- function(g) {
    if (g == "canvasser")  "te_canvasser"
    else if (g == "ai")    "te_ai"
    else NA_character_
  }
  for (i in seq_len(nrow(pa))) {
    stem <- te_stem(pa$group[i])
    if (is.na(stem)) next
    nums <- emit_ci(nums, sprintf("realworld.panel_a.%s", stem),
                    pa$estimate[i], pa$lower_ci[i], pa$upper_ci[i], pa$p_value[i])
  }
}
if (have_rds("realworld_ai_vs_canvasser")) {
  ac <- load_rds("realworld_ai_vs_canvasser")
  if (nrow(ac) >= 1L) {
    nums <- emit_ci(nums, "realworld.panel_a.ai_vs_canvasser",
                    ac$estimate[1], ac$lower_ci[1], ac$upper_ci[1], ac$p_value[1])
  }
}

# ---- Fig 4b extensive + intensive margins (per-arm + AI - Canvasser) ----
# `realworld_margins.rds` is a 2-list: `per_group` (per-arm Wilson /
# Welch summaries) and `deltas` (the two AI - Canvasser deltas). We
# flatten both into a small set of named scalars so the manuscript can
# cite e.g. "78% of AI persuadees donated, vs 72% of canvassers" by name.
if (have_rds("realworld_margins")) {
  mg   <- load_rds("realworld_margins")
  pg   <- mg$per_group
  pick <- function(grp, col) {
    row <- pg[pg$group == grp, , drop = FALSE]
    if (nrow(row) == 1L) row[[col]] else NA_real_
  }
  nums$realworld.panel_b.extensive.ai_pct      <- pick("ai",        "p_donate_pct")
  nums$realworld.panel_b.extensive.canv_pct    <- pick("canvasser", "p_donate_pct")
  nums$realworld.panel_b.extensive.control_pct <- pick("control",   "p_donate_pct")
  nums$realworld.panel_b.intensive.ai_mean      <- pick("ai",        "mean_donor")
  nums$realworld.panel_b.intensive.canv_mean    <- pick("canvasser", "mean_donor")
  nums$realworld.panel_b.intensive.control_mean <- pick("control",   "mean_donor")

  ext <- mg$deltas[mg$deltas$margin == "extensive", , drop = FALSE]
  if (nrow(ext) == 1L) {
    nums$realworld.panel_b.extensive.delta          <- ext$estimate
    nums$realworld.panel_b.extensive.delta_lower_ci <- ext$lower_ci
    nums$realworld.panel_b.extensive.delta_upper_ci <- ext$upper_ci
  }
  int <- mg$deltas[mg$deltas$margin == "intensive", , drop = FALSE]
  if (nrow(int) == 1L) {
    nums$realworld.panel_b.intensive.delta          <- int$estimate
    nums$realworld.panel_b.intensive.delta_lower_ci <- int$lower_ci
    nums$realworld.panel_b.intensive.delta_upper_ci <- int$upper_ci
  }
}

# ---- Fig 4b per-strategy composite Welch t (7 preregistered pairs) -----
# Drives Fig 4b (Panel B). RDS is pre-sorted desc by composite
# estimate; we expose per-strategy fields under fixed short keys
# matching the canonical action-persuasion-paper labels.
if (have_rds("realworld_strategies")) {
  sb <- load_rds("realworld_strategies")
  for (i in seq_len(nrow(sb))) {
    stem <- sb$strategy[i]
    nums <- emit_ci(nums, sprintf("realworld.panel_b.strategy.%s", stem),
                    sb$estimate[i], sb$lower_ci[i], sb$upper_ci[i], sb$p_value[i])
  }
  nums$realworld.panel_b.n_strategies_p_lt_05 <-
    as.integer(sum(sb$p_value < 0.05, na.rm = TRUE))
  top3 <- utils::head(sb, 3L)$display
  nums$realworld.panel_b.top3_strategies <- paste(top3, collapse = ", ")
}

# ---- Partner perception (text-only; no figure panel) -------------------
# Partner perception was dropped from the main figure and is reported in
# the manuscript text only (full per-item table in SI). We expose two
# counts: across the full 7-item battery, and across the 4 positive-
# valence items (learning, arguments, empathy, enjoyment) which form
# the natural conceptual cluster the text emphasises.
if (have_rds("realworld_perception")) {
  pc <- load_rds("realworld_perception")
  positive_subset <- c("rating_learning", "rating_arguments",
                       "rating_empathy",  "rating_enjoyment")
  pc_pos <- pc |> dplyr::filter(item %in% positive_subset)
  nums$realworld.perception.n_significant_full <-
    as.integer(sum(pc$p_value < 0.05, na.rm = TRUE))
  nums$realworld.perception.n_significant_positive_4 <-
    as.integer(sum(pc_pos$p_value < 0.05, na.rm = TRUE))
}
# Full-battery mechanism count (for SI table cross-reference; per-item
# rows are not plotted in the main figure -- they live as the faint
# dots overlaid on each Panel B strategy composite).
if (have_rds("realworld_mechanism")) {
  mc <- load_rds("realworld_mechanism")
  nums$realworld.mechanism.n_significant_full <-
    as.integer(sum(mc$p_value < 0.05, na.rm = TRUE))
}

# ---- Per-issue AI vs pooled-humans (Results: per-issue heterogeneity) ---
# Backs the "across N of 10 policy issues at p < .05 (X.X to Y.Y pp)"
# claim. Numbers come from issue_level_ai_vs_humans.rds (emmeans-based
# AI - mean(human classes) contrast per issue). We expose total counts
# plus the min/max significant estimate and the issue id holding each
# extreme, so the manuscript can name them if narratively useful.
if (have_rds("issue_level_ai_vs_humans")) {
  iv <- load_rds("issue_level_ai_vs_humans")
  nums$issues.n_total       <- as.integer(nrow(iv))
  nums$issues.n_sig_p_lt_05 <- as.integer(sum(iv$p_value < 0.05, na.rm = TRUE))
  sig <- iv |> dplyr::filter(p_value < 0.05)
  if (nrow(sig) > 0L) {
    nums$issues.estimate_min_pp     <- min(sig$estimate)
    nums$issues.estimate_max_pp     <- max(sig$estimate)
    nums$issues.estimate_min_issue  <- sig$issue_id[which.min(sig$estimate)]
    nums$issues.estimate_max_issue  <- sig$issue_id[which.max(sig$estimate)]
  }
  nonsig <- iv |> dplyr::filter(p_value >= 0.05)
  # nonsig issue list (comma-joined, "" when all are sig)
  nums$issues.nonsig_issue_ids <-
    paste(as.character(nonsig$issue_id), collapse = ", ")
}

# ---- Per-subgroup AI vs pooled-humans (Results: subgroup heterogeneity) -
# Backs the "across all 14 ... subgroups we examined (X.X to Y.Y pp)"
# claim. We expose: number of subgroup variables (14), total levels
# fitted, number significant at p < .05, and the min/max estimate
# across significant levels.
if (have_rds("subgroups_ai_vs_humans")) {
  sv <- load_rds("subgroups_ai_vs_humans")
  nums$subgroups.n_variables  <- as.integer(dplyr::n_distinct(sv$subgroup))
  nums$subgroups.n_levels     <- as.integer(nrow(sv))
  nums$subgroups.n_sig_p_lt_05 <-
    as.integer(sum(sv$p_value < 0.05, na.rm = TRUE))
  sig <- sv |> dplyr::filter(p_value < 0.05)
  if (nrow(sig) > 0L) {
    nums$subgroups.estimate_min_pp <- min(sig$estimate)
    nums$subgroups.estimate_max_pp <- max(sig$estimate)
  }
}

# ---- Moderator interactions (Results: moderator interactions) -----------
# `moderator_effects.rds` has one row per (moderator, level) with the
# AI - mean(humans) emmeans contrast within that level, plus the joint
# Wald-chi^2 interaction p-value for the moderator (repeated across its
# rows; we collapse to one p per moderator). New keys:
#   mod.<name>.<level>.estimate / .lower_ci / .upper_ci / .p_value
#   mod.<name>.p_interaction
if (have_rds("moderator_effects")) {
  me <- load_rds("moderator_effects")
  for (i in seq_len(nrow(me))) {
    stem <- sprintf("mod.%s.%s", me$moderator[i], me$level[i])
    nums[[sprintf("%s.estimate", stem)]] <- me$estimate[i]
    nums[[sprintf("%s.lower_ci", stem)]] <- me$lower_ci[i]
    nums[[sprintf("%s.upper_ci", stem)]] <- me$upper_ci[i]
    nums[[sprintf("%s.p_value",  stem)]] <- me$p_value[i]
  }
  per_mod_p <- me |>
    dplyr::group_by(moderator) |>
    dplyr::summarise(p_int = dplyr::first(p_interaction), .groups = "drop")
  for (i in seq_len(nrow(per_mod_p))) {
    nums[[sprintf("mod.%s.p_interaction", per_mod_p$moderator[i])]] <-
      per_mod_p$p_int[i]
  }
}

# ---- Study 2 per-study AI TE + AI - Coached contrast (Results: coaching) -
# The manuscript says "narrowing but not closing the gap to the Study 2
# AI estimate of X.X pp (delta = Y.Y pp, p < .01)". We extract the
# per-study AI vs control treatment effect from study2_prereg_lmm.rds
# and a single-contrast emmeans for AI - Coached Elite Debater.
if (have_rds("study2_prereg_lmm")) {
  s2 <- load_rds("study2_prereg_lmm")
  em <- tryCatch(
    emmeans::emmeans(s2, ~ group, lmer.df = "asymptotic", nesting = NULL),
    error = function(e) NULL
  )
  if (!is.null(em)) {
    grps <- as.character(em@levels$group)
    emit_contrast <- function(stem, vec) {
      ctr <- emmeans::contrast(em, list(x = vec)) |>
        summary(infer = TRUE, adjust = "none") |>
        tibble::as_tibble()
      nums[[sprintf("%s.estimate", stem)]] <<- ctr$estimate[1]
      nums[[sprintf("%s.lower_ci", stem)]] <<- ctr$asymp.LCL[1]
      nums[[sprintf("%s.upper_ci", stem)]] <<- ctr$asymp.UCL[1]
      nums[[sprintf("%s.p_value",  stem)]] <<- ctr$p.value[1]
    }
    if (all(c("control", "ai") %in% grps)) {
      emit_contrast(
        "study2.te.ai",
        ifelse(grps == "ai", 1, ifelse(grps == "control", -1, 0))
      )
    }
    if (all(c("control", "coached_elite_debater") %in% grps)) {
      emit_contrast(
        "study2.te.coached_elite_debater",
        ifelse(grps == "coached_elite_debater", 1,
               ifelse(grps == "control", -1, 0))
      )
    }
    if (all(c("coached_elite_debater", "ai") %in% grps)) {
      emit_contrast(
        "study2.ai_minus_coached",
        ifelse(grps == "ai",                     1,
               ifelse(grps == "coached_elite_debater", -1, 0))
      )
    }
  }
}

# ---- Study 1 throughput (Results: Study 1 engagement) -------------------
# Words-per-message are computed from the data (persuader_words
# / n_persuader_msgs averaged across conversations within an arm).
# Per-persuader-message response times are NOT in the data, so
# they are pinned as design-history constants below with documented
# provenance. The constants are surfaced in _numbers.json so the
# manuscript references a single source of truth and so any future
# update lives in one place.
STUDY1_DELAYS_S <- list(
  # Elite Debater mean per-message reply time in Study 1 (~95 s). The
  # data does not carry per-persuader-message timestamps,
  # so the value is pinned here. The Study 2 Constrained AI condition was
  # calibrated to a 92 s mean (truncated-normal sampler); see
  # PANEL_INSET_DELAYS_S in 02_mechanism.R.
  elite_debater = 95L,
  # AI inference latency in Study 1 was sub-second across all generations;
  # we surface the descriptor as a character "<1" rather than a numeric
  # mean.
  ai            = "<1"
)
if (have_rds("_data/study_1")) {
  s1 <- readRDS(file.path(RESULTS, "_data/study_1.rds")) |>
    dplyr::filter(!is.na(post_attitude), !is.na(pre_attitude),
                  !is.na(persuader_words), !is.na(n_persuader_msgs),
                  n_persuader_msgs > 0L)
  per_class_words <- s1 |>
    dplyr::mutate(wpm = persuader_words / n_persuader_msgs) |>
    dplyr::group_by(persuader_class) |>
    dplyr::summarise(avg_words_msg = mean(wpm, na.rm = TRUE), .groups = "drop")
  pick_wpm <- function(cls) {
    row <- per_class_words[per_class_words$persuader_class == cls, , drop = FALSE]
    if (nrow(row) == 1L) row$avg_words_msg else NA_real_
  }
  nums$throughput.s1.elite_debater.avg_words_msg <- pick_wpm("elite_debater")
  nums$throughput.s1.ai.avg_words_msg            <- pick_wpm("ai")
  nums$throughput.s1.elite_debater.avg_delay_s   <- STUDY1_DELAYS_S$elite_debater
  nums$throughput.s1.ai.avg_delay_s              <- STUDY1_DELAYS_S$ai
}

# ---- Overall treatment-dialogue length + total counts (intro/abstract) --
# Total participant and conversation counts pool across S1-S4 (referenced
# in the abstract sample-size claim). Per-scope engagement stats (mean +
# median persuader turns and conversation duration) are emitted under two
# scopes: `dialogue.s1_s3.*` for the Studies 1-3 results paragraph and
# `dialogue.s4.*` for the Study 4 setup paragraph. Both filter to
# non-control treatment conversations with a non-NA, positive turn count
# and duration.
study_caches <- file.path(RESULTS, "_data",
                          c("study_1.rds", "study_2.rds",
                            "study_3.rds", "study_4.rds"))
if (all(file.exists(study_caches))) {
  raw_df <- purrr::map_dfr(study_caches, function(p) {
    df <- readRDS(p)
    if (!all(c("study_id", "persuader_class", "persuadee_id", "persuader_id")
             %in% names(df))) {
      return(NULL)
    }
    outcome_col <- if (grepl("study_4", p)) "donation_amount" else "post_attitude"
    has_outcome <- if (outcome_col %in% names(df)) !is.na(df[[outcome_col]]) else FALSE
    matched     <- dplyr::coalesce(df[["successfully_matched"]], FALSE)
    attrited    <- if ("attrited" %in% names(df)) {
      dplyr::coalesce(df[["attrited"]], FALSE)
    } else FALSE
    df |> dplyr::transmute(study_id, persuader_class, persuadee_id, persuader_id,
                           n_persuader_msgs = .data[["n_persuader_msgs"]],
                           conv_duration_s  = .data[["conv_duration_s"]],
                           in_analytic = matched & !attrited & has_outcome)
  })
  if (nrow(raw_df) > 0L) {
    # Headline counts use the final analytic sample (see header note):
    # matched, non-attrited conversations with a non-missing analysed
    # outcome. This reconciles exactly with the "Final" column of
    # tab_sample_sizes.
    human_classes_all <- c("random_lay_person", "elite_lay_person",
                           "canvasser", "elite_debater",
                           "coached_elite_debater")
    analytic_df <- raw_df |> dplyr::filter(in_analytic)
    nums$dialogue.n_sessions_total  <- as.integer(nrow(raw_df))
    nums$dialogue.n_conv_total      <- as.integer(nrow(analytic_df))
    nums$dialogue.n_persuadees      <-
      as.integer(dplyr::n_distinct(analytic_df$persuadee_id, na.rm = TRUE))
    nums$dialogue.n_human_persuaders <- as.integer(
      analytic_df |>
        dplyr::filter(persuader_class %in% human_classes_all,
                      !is.na(persuader_id)) |>
        dplyr::pull(persuader_id) |>
        dplyr::n_distinct()
    )
    nums$dialogue.n_participants    <-
      nums$dialogue.n_persuadees + nums$dialogue.n_human_persuaders
  }
  emit_dialogue_scope <- function(scope_label, study_filter) {
    df <- raw_df |>
      dplyr::filter(study_id %in% study_filter,
                    !is.na(persuader_class), persuader_class != "control",
                    !is.na(n_persuader_msgs), n_persuader_msgs > 0L,
                    !is.na(conv_duration_s),  conv_duration_s  > 0)
    if (nrow(df) == 0L) return(invisible(NULL))
    prefix <- sprintf("dialogue.%s", scope_label)
    nums[[sprintf("%s.n_conv",                 prefix)]] <<- as.integer(nrow(df))
    nums[[sprintf("%s.mean_persuader_turns",   prefix)]] <<- mean(df$n_persuader_msgs)
    nums[[sprintf("%s.median_persuader_turns", prefix)]] <<-
      stats::median(df$n_persuader_msgs)
    nums[[sprintf("%s.mean_duration_min",      prefix)]] <<-
      mean(df$conv_duration_s) / 60
    nums[[sprintf("%s.median_duration_min",    prefix)]] <<-
      stats::median(df$conv_duration_s) / 60
  }
  emit_dialogue_scope("s1_s3", c("study_1", "study_2", "study_3"))
  emit_dialogue_scope("s4",    c("study_4"))
}

# ---- Selection-tournament (Study 0) scale + scope ----------------------
# Raw counts from the data, no complete-case filter (these are
# descriptive scale claims, not outcome-model denominators). Back the
# tournament references in the Results overview, the persuader-class
# paragraph, and the Methods (Selected Laypeople derivation).
s0_cache <- file.path(RESULTS, "_data", "study_0.rds")
if (file.exists(s0_cache)) {
  s0 <- readRDS(s0_cache)
  if (all(c("tournament_round", "persuader_id", "persuadee_id")
          %in% names(s0))) {
    r1_persuaders <- s0 |>
      dplyr::filter(tournament_round == "R1") |>
      dplyr::pull(persuader_id)
    all_persuaders <- s0$persuader_id
    all_persuadees <- s0$persuadee_id

    nums$tournament.n_rounds                <- 4L
    nums$tournament.n_r1_persuaders         <-
      as.integer(dplyr::n_distinct(r1_persuaders, na.rm = TRUE))
    nums$tournament.n_persuaders_all_rounds <-
      as.integer(dplyr::n_distinct(all_persuaders, na.rm = TRUE))
    nums$tournament.n_conversations         <- as.integer(nrow(s0))
    nums$tournament.n_persuadees            <-
      as.integer(dplyr::n_distinct(all_persuadees, na.rm = TRUE))
    nums$tournament.n_participants          <- as.integer(
      length(unique(stats::na.omit(c(all_persuadees, all_persuaders))))
    )
    nums$tournament.n_advanced_to_s1        <- 87L
    nums$tournament.invite_threshold_pct    <- 10L
  }
}

# ---- Per-conversation performance bonuses (Results; Table 1) -----------
# `bonuses.rds` is one row per bonused class (Selected Laypeople, Elite
# Debaters, Coached Elite Debaters), produced by 04_bonuses.R from the
# preregistered formula
#   Individual Bonus = £15 × (persuader_TE / study_AI_TE) × N_conversations
# applied to the pooled-model per-persuader TE estimates. Exposed scalars
# (per raw_class):
#   bonuses.<class>.n_persuaders
#   bonuses.<class>.mean_n_conversations
#   bonuses.<class>.mean_per_conv_gbp           (£15 × mean rel.eff.;
#                                                matches Table 1 "Per-
#                                                conversation Incentive"
#                                                column averages.)
#   bonuses.<class>.mean_total_gbp              (per-persuader sum,
#                                                averaged across persuaders;
#                                                backs the body-text average
#                                                per-conversation bonus.)
#   bonuses.<class>.median_total_gbp
#   bonuses.<class>.mean_relative_effectiveness (unitless; 1.0 = parity
#                                                with that study's AI.)
if (have_rds("bonuses")) {
  b <- load_rds("bonuses")
  for (i in seq_len(nrow(b))) {
    cls <- b$raw_class[i]
    nums[[sprintf("bonuses.%s.n_persuaders",                cls)]] <- b$n_persuaders[i]
    nums[[sprintf("bonuses.%s.mean_n_conversations",        cls)]] <- b$mean_n_conversations[i]
    nums[[sprintf("bonuses.%s.mean_per_conv_gbp",           cls)]] <- b$mean_per_conv_gbp[i]
    nums[[sprintf("bonuses.%s.mean_total_gbp",              cls)]] <- b$mean_total_bonus_gbp[i]
    nums[[sprintf("bonuses.%s.median_total_gbp",            cls)]] <- b$median_total_bonus_gbp[i]
    nums[[sprintf("bonuses.%s.mean_relative_effectiveness", cls)]] <- b$mean_relative_effectiveness[i]
  }
}

# ---- Professional Canvassers career-experience self-reports ------------
# Backs the Results and Methods statements on canvassers' typical career
# conversations and funds raised. These are summaries of the Study 3
# PRE-survey items canvassing-conversations and canvassing-funds-raised
# (free-text), parsed into numeric `_num` fields in the data; here we
# surface per-cohort medians so the manuscript numbers are reproducible
# from the data.
#
# Cohort definition: matched canvassers with at least one persuader message
# in the data, deduplicated to one row per persuader_id (each
# canvasser ran many conversations; we want one self-report per person).
# Three cohorts:
#   * `s3`    = canvassers who matched in Study 3
#   * `s4`    = canvassers who matched in Study 4 (mostly the same people)
#   * `union` = either Study 3 or Study 4 (the "Studies 3 & 4" cohort the
#               Methods text references)
#
# Companion audit CSV at output/results/_data/canvasser_headline_audit.csv
# carries one row per canvasser with the raw self-report strings alongside
# the parsed numerics so reviewers can verify the parsing rules.
source(here::here("code/analysis/_load_data.R"))

.canvasser_summary <- function(label, df_pids) {
  d  <- df_pids
  cn <- d$persuader_career_conversations
  fr <- d$persuader_career_funds_raised
  yr <- d$persuader_career_years
  cn_v <- cn[!is.na(cn)]
  fr_v <- fr[!is.na(fr)]
  yr_v <- yr[!is.na(yr)]
  list(
    label    = label,
    n_total  = nrow(d),
    conv     = list(n_valid = length(cn_v),
                    median  = if (length(cn_v)) stats::median(cn_v) else NA_real_,
                    mean    = if (length(cn_v)) mean(cn_v)          else NA_real_,
                    min     = if (length(cn_v)) min(cn_v)            else NA_real_,
                    max     = if (length(cn_v)) max(cn_v)            else NA_real_),
    funds    = list(n_valid = length(fr_v),
                    median  = if (length(fr_v)) stats::median(fr_v) else NA_real_,
                    mean    = if (length(fr_v)) mean(fr_v)          else NA_real_,
                    min     = if (length(fr_v)) min(fr_v)            else NA_real_,
                    max     = if (length(fr_v)) max(fr_v)            else NA_real_),
    years    = list(n_valid = length(yr_v),
                    median  = if (length(yr_v)) stats::median(yr_v) else NA_real_,
                    mean    = if (length(yr_v)) mean(yr_v)          else NA_real_,
                    min     = if (length(yr_v)) min(yr_v)            else NA_real_,
                    max     = if (length(yr_v)) max(yr_v)            else NA_real_)
  )
}

canv_studies <- load_studies(c("study_3", "study_4"))

canv_per_pid <- purrr::map_dfr(names(canv_studies), function(sid) {
  df <- canv_studies[[sid]]
  df |>
    dplyr::filter(persuader_class == "canvasser",
                  !is.na(persuader_id),
                  !is.na(n_persuader_msgs), n_persuader_msgs > 0) |>
    dplyr::transmute(
      study_id = sid,
      persuader_id,
      persuader_career_conversations,
      persuader_career_funds_raised,
      persuader_career_years,
      persuader_career_experience_raw
    )
})

# Per-canvasser self-report (one row per (study_id, persuader_id), then
# collapse to one per persuader_id by taking the first non-NA value -
# the same Prolific user reported the same career stats in both studies).
canv_one_per_pid <- canv_per_pid |>
  dplyr::group_by(persuader_id) |>
  dplyr::summarise(
    in_s3 = any(study_id == "study_3"),
    in_s4 = any(study_id == "study_4"),
    persuader_career_conversations = dplyr::first(stats::na.omit(persuader_career_conversations)),
    persuader_career_funds_raised  = dplyr::first(stats::na.omit(persuader_career_funds_raised)),
    persuader_career_years         = dplyr::first(stats::na.omit(persuader_career_years)),
    persuader_career_experience_raw= dplyr::first(stats::na.omit(persuader_career_experience_raw)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    study_membership = dplyr::case_when(
      in_s3 & in_s4 ~ "both",
      in_s3         ~ "s3_only",
      in_s4         ~ "s4_only",
      TRUE          ~ "neither"
    )
  )

s3_pids    <- canv_one_per_pid |> dplyr::filter(in_s3)
s4_pids    <- canv_one_per_pid |> dplyr::filter(in_s4)
union_pids <- canv_one_per_pid

emit_canv <- function(prefix, summ) {
  nums[[sprintf("canvasser.%s.n_total", prefix)]]                <<- summ$n_total
  nums[[sprintf("canvasser.%s.career_conversations.n_valid", prefix)]] <<- summ$conv$n_valid
  nums[[sprintf("canvasser.%s.career_conversations.median",  prefix)]] <<- summ$conv$median
  nums[[sprintf("canvasser.%s.career_conversations.mean",    prefix)]] <<- summ$conv$mean
  nums[[sprintf("canvasser.%s.career_conversations.min",     prefix)]] <<- summ$conv$min
  nums[[sprintf("canvasser.%s.career_conversations.max",     prefix)]] <<- summ$conv$max
  nums[[sprintf("canvasser.%s.funds_raised.n_valid", prefix)]]   <<- summ$funds$n_valid
  nums[[sprintf("canvasser.%s.funds_raised.median",  prefix)]]   <<- summ$funds$median
  nums[[sprintf("canvasser.%s.funds_raised.mean",    prefix)]]   <<- summ$funds$mean
  nums[[sprintf("canvasser.%s.funds_raised.min",     prefix)]]   <<- summ$funds$min
  nums[[sprintf("canvasser.%s.funds_raised.max",     prefix)]]   <<- summ$funds$max
  nums[[sprintf("canvasser.%s.career_years.n_valid", prefix)]]   <<- summ$years$n_valid
  nums[[sprintf("canvasser.%s.career_years.median",  prefix)]]   <<- summ$years$median
  nums[[sprintf("canvasser.%s.career_years.mean",    prefix)]]   <<- summ$years$mean
}
emit_canv("s3",    .canvasser_summary("s3",    s3_pids))
emit_canv("s4",    .canvasser_summary("s4",    s4_pids))
emit_canv("union", .canvasser_summary("union", union_pids))

# Audit CSV: one row per canvasser, with raw + parsed values so reviewers
# can verify the free-text parsing rules.
canv_audit_path <- file.path(RESULTS_DATA, "canvasser_headline_audit.csv")
canv_audit <- canv_one_per_pid |>
  dplyr::transmute(
    persuader_id,
    study_membership,
    career_conversations_num = persuader_career_conversations,
    career_funds_raised_num  = persuader_career_funds_raised,
    career_years_num         = persuader_career_years,
    career_experience_raw    = persuader_career_experience_raw
  ) |>
  dplyr::arrange(persuader_id)
readr::write_csv(canv_audit, canv_audit_path)
message(sprintf("  wrote canvasser audit (%d rows) to %s",
                nrow(canv_audit), canv_audit_path))

# ---- Elite Debaters: cohort descriptors (Results; Methods) -------------
# Reads the LLM-annotated audit CSV (committed; reproduced by
# code/analysis/elite_debater/annotate.py against the rubric in
# code/analysis/elite_debater/rubric.md) and emits per-criterion totals.
# Years and country count are computed directly from the data.
debater_audit_path <- file.path(PROJ_ROOT, "code", "analysis", "elite_debater", "audit.csv")
if (file.exists(debater_audit_path)) {
  da <- readr::read_csv(debater_audit_path, show_col_types = FALSE)
  bool_cols <- c(
    "is_world_champion",
    "is_continental_champion",
    "is_continental_champion_any_division",
    "reached_continental_semis_or_better",
    "reached_major_semis_or_better",
    "is_best_speaker_at_major",
    "is_continental_finalist"
  )
  emit_for <- list(
    is_world_champion                    = "n_world_champion",
    is_continental_champion              = "n_continental_champion",
    is_continental_champion_any_division = "n_continental_champion_any_division",
    reached_continental_semis_or_better  = "n_continental_semis_or_better",
    reached_major_semis_or_better        = "n_major_semis_or_better",
    is_best_speaker_at_major             = "n_best_speaker_at_major",
    is_continental_finalist              = "n_continental_finalist"
  )
  nums$debater.n_total <- as.integer(nrow(da))
  for (col in bool_cols) {
    if (!col %in% names(da)) next
    vals <- da[[col]]
    if (is.character(vals)) vals <- toupper(vals) == "TRUE"
    nums[[sprintf("debater.%s", emit_for[[col]])]] <-
      as.integer(sum(vals, na.rm = TRUE))
  }
}

# Elite-debater years + countries (computed directly from the data).
# Years parsing rule: prefer bare-numeric self-reports; for entries that
# combine "X years university, Y in school" or include school years
# explicitly, use the total years of debating including school. This is
# the rule that produces the manuscript's headline mean of 8.9 (a
# first-number-only rule yields 8.8). Both rules are documented here so
# any future change can be made deliberately.
parse_debating_years <- function(s) {
  if (is.na(s)) return(NA_real_)
  s <- trimws(as.character(s))
  if (s == "") return(NA_real_)
  # Try strict numeric parse first (handles "10", "10.", "5.5").
  num <- suppressWarnings(as.numeric(sub("[^0-9.].*$", "", s)))
  if (!is.na(num) && grepl("^[0-9.]+\\s*$", s)) return(num)
  if (!is.na(num) && grepl("^[0-9.]+\\s*\\.\\s*$", s)) return(num)
  # Free-text cases. Pull out all integer-ish tokens.
  nums <- as.numeric(stats::na.omit(unlist(regmatches(
    s, gregexpr("[0-9]+(?:\\.[0-9]+)?", s)))))
  nums <- nums[!is.na(nums) & nums > 0 & nums < 50]
  if (length(nums) == 0L) return(NA_real_)
  low <- tolower(s)
  has_uni    <- grepl("universit|college|undergrad", low)
  has_school <- grepl("\\bschool\\b|high school|high.?school", low)
  # "4 university, 6 in school" -> sum the first two numbers (explicit split).
  # Requires both keywords AND at least 2 numbers.
  if (has_uni && has_school && length(nums) >= 2L) return(sum(nums[1:2]))
  # Otherwise (incl. "15 (including school and university)" /
  # "9 years including 2 years of high school" / "12 years.") -> take
  # the first number, which the respondent reported as the total.
  nums[1]
}

# Load Study 1 via load_study() so the new persuader_debating_achievements
# / persuader_debating_years columns from COLUMN_SPEC are present (cache
# is rebuilt automatically when the spec changes).
s1 <- tryCatch(load_study("study_1"), error = function(e) NULL)
if (!is.null(s1)) {
  ed <- s1 |>
    dplyr::filter(persuader_class == "elite_debater",
                  !is.na(persuader_id)) |>
    dplyr::distinct(persuader_id, .keep_all = TRUE)

  # Years: parse with the rule above; record both n_total and n_valid for
  # transparency, plus mean/median/sd/min/max.
  years_raw <- ed$persuader_debating_years
  years_num <- vapply(years_raw, parse_debating_years, numeric(1))
  years_v   <- years_num[!is.na(years_num)]
  nums$debater.career_years.n_total <- as.integer(length(years_num))
  nums$debater.career_years.n_valid <- as.integer(length(years_v))
  if (length(years_v) > 0L) {
    nums$debater.career_years.mean   <- mean(years_v)
    nums$debater.career_years.median <- stats::median(years_v)
    nums$debater.career_years.sd     <- stats::sd(years_v)
    nums$debater.career_years.min    <- min(years_v)
    nums$debater.career_years.max    <- max(years_v)
  }

  # Years audit CSV: one row per debater with raw + parsed value.
  years_audit_path <- file.path(RESULTS_DATA, "elite_debater_years_audit.csv")
  years_audit <- tibble::tibble(
    persuader_id       = ed$persuader_id,
    debating_years_raw = ed$persuader_debating_years,
    debating_years_num = years_num
  ) |> dplyr::arrange(persuader_id)
  readr::write_csv(years_audit, years_audit_path)
  message(sprintf("  wrote elite-debater years audit (%d rows) to %s",
                  nrow(years_audit), years_audit_path))
}

# Countries: distinct countries appearing in any of the 56 elite debaters'
# nationality strings. Compound entries (e.g. "Australian, Swiss,
# American" or "Canadian/American/Jordanian") each contribute multiple
# distinct countries to the union. Parents-of mentions (e.g. "American
# (British/Spanish parents)") are stripped before tokenizing, so the
# parents' citizenships are NOT counted. Nationality text is not in
# COLUMN_SPEC, so we re-stream data/study_1.jsonl here.
norm_country <- function(s) {
  if (is.na(s) || s == "") return(character(0))
  s <- gsub("&#x2F;", "/", s, fixed = TRUE)
  s <- gsub("&#39;",  "'", s, fixed = TRUE)
  s <- gsub("&amp;",  "&", s, fixed = TRUE)
  s <- gsub("\\([^)]*parents?[^)]*\\)", "", s, ignore.case = TRUE)
  parts <- strsplit(s, "[,/]| and |[-]", perl = TRUE)[[1L]]
  out <- character(0)
  for (p in parts) {
    t <- tolower(trimws(p))
    t <- sub("^(i am a|i am|the)\\s+", "", t)
    t <- sub("[[:space:].]+$", "", t)
    t <- gsub("\\([^)]*\\)", "", t)
    t <- trimws(t)
    if (t == "") next
    key <- switch(
      t,
      "australian"="Australia", "american"="United States",
      "us"="United States", "british"="United Kingdom",
      "uk"="United Kingdom", "english"="United Kingdom",
      "scottish"="United Kingdom", "welsh"="United Kingdom",
      "serbian"="Serbia", "croatian"="Croatia", "romanian"="Romania",
      "german"="Germany", "irish"="Ireland", "spanish"="Spain",
      "swiss"="Switzerland", "bangladeshi"="Bangladesh",
      "canadian"="Canada", "jordanian"="Jordan", "dutch"="Netherlands",
      "filipino"="Philippines", "ghanaian"="Ghana", "greek"="Greece",
      "hong kong sar"="Hong Kong", "south african"="South Africa",
      "new zealander"="New Zealand", "indian"="India",
      "israeli"="Israel", "italian"="Italy", "malaysian"="Malaysia",
      "mexican"="Mexico", "nigerian"="Nigeria", "russian"="Russia",
      "singapore"="Singapore", "singaporean"="Singapore",
      "slovenian"="Slovenia",
      tools::toTitleCase(t)
    )
    out <- c(out, key)
  }
  unique(out)
}
elite_nat_path <- file.path(DATA, "study_1.jsonl")
if (file.exists(elite_nat_path) && !is.null(nums$debater.n_total)) {
  raw <- jsonlite::stream_in(file(elite_nat_path, open = "rb"),
                             verbose = FALSE)
  ed_idx <- which(raw$treatment$persuader_class == "elite_debater")
  pids   <- raw$persuader$participant_id[ed_idx]
  nats   <- raw$persuader$demographics$nationality_text[ed_idx]
  ok     <- !is.na(pids) & !duplicated(pids)
  nats   <- nats[ok]
  all_countries <- unique(unlist(lapply(nats, norm_country)))
  nums$debater.countries_n <- as.integer(length(all_countries))
}

# ---- coaching behaviour shift (si_08_coaching_behaviour_shift.R) --------
# Persuader-level paired t on the 43 returning Elite Debaters: did coaching
# shift their behaviour toward AI-like throughput? Two outcomes: words per
# persuader message and fact-checkable claims per conversation. Reported in
# the Discussion mechanism paragraph; SI table at \label{si:coaching-shift}.
if (have_rds("coaching_behaviour_shift")) {
  cbs <- load_rds("coaching_behaviour_shift")
  for (i in seq_len(nrow(cbs))) {
    r   <- cbs[i, ]
    pfx <- sprintf("coaching_shift.%s", r$outcome)
    nums[[sprintf("%s.estimate",      pfx)]] <- r$estimate
    nums[[sprintf("%s.lower_ci",      pfx)]] <- r$lower_ci
    nums[[sprintf("%s.upper_ci",      pfx)]] <- r$upper_ci
    nums[[sprintf("%s.t_stat",        pfx)]] <- r$t_stat
    nums[[sprintf("%s.df",            pfx)]] <- r$df
    nums[[sprintf("%s.p_value",       pfx)]] <- r$p_value
    nums[[sprintf("%s.baseline_mean", pfx)]] <- r$baseline_mean
    nums[[sprintf("%s.pct_change",    pfx)]] <- r$pct_change
    nums[[sprintf("%s.n_pairs",       pfx)]] <- r$n_pairs
    nums[[sprintf("%s.n_conv_s1",     pfx)]] <- r$n_conv_s1
    nums[[sprintf("%s.n_conv_s2",     pfx)]] <- r$n_conv_s2
    nums[[sprintf("%s.lmm_estimate",  pfx)]] <- r$lmm_estimate
    nums[[sprintf("%s.lmm_p_value",   pfx)]] <- r$lmm_p_value
  }
}

# ---- fact-checking accuracy (si_09_fact_accuracy.R) ---------------------
# Per-study and pooled proportion of extracted claims rated accurate
# (web-search veracity > 50/100), plus total claims checked. Backs the SI
# accuracy section and the accuracy-vs-impact figure.
if (have_rds("fact_accuracy_by_study")) {
  fas <- load_rds("fact_accuracy_by_study")
  for (i in seq_len(nrow(fas))) {
    s <- sub("study_", "s", fas$study[i])
    nums[[sprintf("fact_accuracy.%s.n_conv", s)]]               <- as.integer(fas$n_conv[i])
    nums[[sprintf("fact_accuracy.%s.n_claims", s)]]             <- as.integer(fas$n_claims[i])
    nums[[sprintf("fact_accuracy.%s.mean_claims_per_conv", s)]] <- fas$mean_claims_per_conv[i]
    nums[[sprintf("fact_accuracy.%s.prop_accurate", s)]]        <- fas$prop_accurate[i]
    nums[[sprintf("fact_accuracy.%s.mean_veracity", s)]]        <- fas$mean_veracity[i]
  }
  nums$fact_accuracy.overall.n_conv        <- as.integer(sum(fas$n_conv))
  nums$fact_accuracy.overall.n_claims      <- as.integer(sum(fas$n_claims))
  nums$fact_accuracy.overall.prop_accurate <- sum(fas$n_claims * fas$prop_accurate) / sum(fas$n_claims)
  nums$fact_accuracy.overall.mean_veracity <- sum(fas$n_claims * fas$mean_veracity) / sum(fas$n_claims)
}
if (have_rds("fact_accuracy_by_condition")) {
  fac <- load_rds("fact_accuracy_by_condition") |>
    dplyr::filter(!is.na(prop_accurate), !is.na(n_claims))
  # Claim-weighted pooled accuracy for AI (info + constrained) vs human arms.
  agg <- fac |>
    dplyr::mutate(grp = ifelse(type == "Human", "human", "ai")) |>
    dplyr::group_by(grp) |>
    dplyr::summarise(
      prop_accurate = sum(prop_accurate * n_claims) / sum(n_claims),
      n_claims      = sum(n_claims),
      .groups       = "drop"
    )
  for (i in seq_len(nrow(agg))) {
    nums[[sprintf("fact_accuracy.%s_pooled.prop_accurate", agg$grp[i])]] <- agg$prop_accurate[i]
    nums[[sprintf("fact_accuracy.%s_pooled.n_claims",      agg$grp[i])]] <- as.integer(agg$n_claims[i])
  }
}

# ---- repeated participation (si_11) -------------------------------------
# Descriptive multiplicity, cross-study overlap, the never-same-issue
# verification, and the first-session / carryover robustness contrasts.
if (have_rds("repeat_summary")) {
  rs <- load_rds("repeat_summary")
  nums$repeat.n_persuadees_s1s3    <- as.integer(rs$n_persuadees_s1s3)
  nums$repeat.n_multi_session      <- as.integer(rs$n_multi_session)
  nums$repeat.pct_multi_session    <- rs$pct_multi_session
  nums$repeat.max_sessions_s1s3    <- as.integer(rs$max_sessions_s1s3)
  # Number of (persuadee, issue) pairs seen more than once across S1-S3.
  # The design assigned a previously-unseen issue at each session; this is
  # the realized count of exceptions (verification, not assumed = 0).
  nums$repeat.n_duplicate_issue_s1s3 <- as.integer(rs$n_duplicate_issue_s1s3)
}
if (have_rds("repeat_overlap")) {
  ov <- load_rds("repeat_overlap")
  nums$repeat.n_cross_study_s1s2s3 <- as.integer(ov$n_cross_s1s2s3)
  nums$repeat.n_cross_study_s0_s4  <- as.integer(ov$n_in_multiple)
  nums$repeat.n_unique_all_pools   <- as.integer(ov$n_unique_total)
}
if (have_rds("repeat_issue_check")) {
  ic <- load_rds("repeat_issue_check")
  for (i in seq_len(nrow(ic))) {
    nums[[sprintf("repeat.issue_check.%s.n_duplicate", ic$study[i])]] <-
      as.integer(ic$n_duplicate_issue_pairs[i])
    nums[[sprintf("repeat.issue_check.%s.max_same_issue", ic$study[i])]] <-
      as.integer(ic$max_sessions_same_issue[i])
  }
}
if (have_rds("repeat_first_session")) {
  fs <- load_rds("repeat_first_session")
  for (i in seq_len(nrow(fs))) {
    nums <- emit_ci(nums, sprintf("repeat.first_session.%s", fs$group[i]),
                    fs$estimate[i], fs$lower_ci[i], fs$upper_ci[i], fs$p_value[i])
  }
}
if (have_rds("repeat_first_session_ai_minus")) {
  fm <- load_rds("repeat_first_session_ai_minus")
  for (i in seq_len(nrow(fm))) {
    nums <- emit_ci(nums, sprintf("repeat.first_session.ai_minus.%s", fm$group[i]),
                    fm$estimate[i], fm$lower_ci[i], fm$upper_ci[i], fm$p_value[i])
  }
}
if (have_rds("repeat_carryover")) {
  co <- load_rds("repeat_carryover")
  nums$repeat.carryover.p_interaction <- co$p_interaction
  nums$repeat.carryover.lrt_chisq     <- co$lrt_chisq
  nums$repeat.carryover.lrt_df        <- as.integer(co$lrt_df)
  for (i in seq_len(nrow(co$slopes))) {
    s <- co$slopes[i, ]
    nums <- emit_ci(nums, sprintf("repeat.carryover.%s", s$group),
                    s$estimate, s$lower_ci, s$upper_ci, s$p_value)
  }
}

dir.create(RESULTS, showWarnings = FALSE, recursive = TRUE)
out_path <- file.path(RESULTS, "_numbers.json")
jsonlite::write_json(nums, out_path, auto_unbox = TRUE, pretty = TRUE,
                     na = "null", null = "null", digits = 6)
message(sprintf("  wrote %d named scalars to %s", length(nums), out_path))
