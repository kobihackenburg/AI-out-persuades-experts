# 03_donation.R
# =============================================================================
# Donation analysis ("Does AI's advantage extend from attitudes to
# consequential real-world behavior?") +
# every RDS that backs Figure 4 (realworld_figure.{pdf,png}). Study 4 only.
#
# This script produces more outputs than Fig 4 renders directly: it emits
# the per-item Welch t tables for both batteries (7 partner-perception
# items + 14 mechanism items) and the per-condition extensive/intensive
# margin decomposition. These are needed because:
#   1. The manuscript text cites the extensive + intensive AI-Canvasser
#      deltas inline (surfaced via _numbers.json).
#   2. The SI reports per-item AI-Canvasser estimates for the full 14-item
#      mechanism battery and the full 7-item perception battery. Both are
#      sourced directly from the per-item RDS here.
#   3. Panel B (Fig 4b) overlays faint per-item dots on each strategy
#      composite; that overlay reads its dot positions from
#      realworld_mechanism.rds.
#
# Outputs (under output/results/, consumer after `->`):
#   study4_donation_lmm.rds              -> manuscript-line covariate sanity
#                                           check; refit-free reload
#   realworld_te_vs_control.rds       -> Fig 4a forest (Canvasser, AI vs Control)
#   realworld_ai_vs_canvasser.rds     -> Fig 4a inline preregistered
#                                           headline AI - Canvasser contrast
#   realworld_strategies.rds          -> Fig 4b: 7 mechanism-strategy
#                                           composites (preregistered pairs;
#                                           canonical labels from the
#                                           action-persuasion paper).
#                                           Welch t on the per-persuadee
#                                           2-item mean, AI vs Canvasser,
#                                           sorted desc by estimate
#   realworld_margins.rds             -> per-condition extensive +
#                                           intensive margins AND the two
#                                           AI - Canvasser deltas. NOT
#                                           plotted; consumed by the
#                                           manuscript text and the SI
#                                           margin table
#   realworld_perception.rds          -> ALL 7 partner-rating items
#                                           (AI - Canvasser, Welch t 95% CI).
#                                           Not plotted in the main figure;
#                                           full table goes to SI
#   realworld_mechanism.rds           -> ALL 14 donation-mechanism items
#                                           (AI - Canvasser, Welch t 95% CI),
#                                           sorted by estimate (desc).
#                                           Drives the faint per-item dots
#                                           overlaid on each Panel B row;
#                                           full table goes to SI
#
# =============================================================================
# WHAT THIS SCRIPT FITS
# =============================================================================
# Panel A (Study 4 only):
#   Preregistered LMM:
#     donation_amount ~ pre_attitude + pre_donation_willingness + age +
#                       ordered(ideology_coded) + group + (1 | persuader)
#   group reference = "control"; reports emmeans trt.vs.ctrl contrasts for
#   Canvasser and AI (the two pointranges in Fig 4a) plus the preregistered
#   AI - Canvasser key contrast (the inline manuscript number). Random
#   intercept stratifies by persuader so the canvasser SE reflects between-
#   canvasser variance; control rows get a synthetic `solo_<session>` id so
#   they still contribute to the reference mean without inflating the RE.
#
# Panel B - margin decomposition (NOT plotted; used by manuscript text):
#   Per-condition margins computed directly from the data (NOT from the
#   LMM): extensive margin via prop.test() Wilson 95% CI on P(donate > 0);
#   intensive margin via t.test() Welch 95% CI on mean donation | donate > 0.
#   The two AI - Canvasser deltas use closed-form Wald on proportions and a
#   Welch t-difference on donor-only means.
#
# Panel B - strategy composites (drives Fig 4b):
#   The 14 mechanism items were preregistered as 7 conceptual pairs (see
#   the Study 4 preregistration); the canonical
#   strategy names come from Hackenburg et al. "Artificial intelligence can
#   persuade people to take political actions".
#   For each strategy we compute the per-persuadee composite mean of its
#   2 items, then run a Welch t (AI vs Canvasser) on that composite. The
#   composite is missing when either constituent item is missing.
#
# Per-item batteries - perception + mechanism (both full; not a figure panel):
#   Per-item Welch t (AI vs Canvasser), descriptive only (no preregistration,
#   no multiple-comparisons correction). Control excluded because most
#   Control participants had no conversation partner. Mechanism rows pre-
#   sorted by estimate (desc) so the figure's faint-dot overlay can pick
#   up the row order off the RDS without re-sorting.
# =============================================================================

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))

message("\n==== 03_donation.R ====")

# =============================================================================
# 1. Constants
# =============================================================================
# Partner-rating items (mirrors PARTNER_ITEMS in 02_mechanism.R so the
# Study 4 partner-rating SI table uses the same item set as Studies 1-2).
PARTNER_ITEMS <- c(
  "rating_learning", "rating_arguments",
  "rating_empathy", "rating_enjoyment", "rating_anthropo",
  "rating_deception", "rating_bias"
)

# Donation-mechanism items (Study 4 only). 14 items hypothesized to drive
# real-money giving; see the manuscript Methods.
MECH_ITEMS <- c(
  "mech_advocacy", "mech_commitment", "mech_consistency",
  "mech_disappointment", "mech_emotion", "mech_empathy",
  "mech_followthrough", "mech_identity", "mech_impact",
  "mech_knowledge", "mech_learning", "mech_mental_image",
  "mech_prior_decision", "mech_regret"
)

# Preregistered 7 strategy pairs. Names (top-level list keys) are the
# short snake_case form used by 99_extract_numbers.R; the `display`
# field is the canonical titlecase label from Hackenburg et al.
# "Artificial intelligence can persuade people to take political
# actions", and is what appears on the Fig 4b y-axis. The two `mech_*`
# items per strategy match the preregistered pair structure in
# the Study 4 preregistration.
STRATEGY_PAIRS <- list(
  emotional_activation       = list(display = "Emotional Activation",
                                    items   = c("mech_emotion", "mech_empathy")),
  implementation_intentions  = list(display = "Implementation Intentions",
                                    items   = c("mech_mental_image", "mech_prior_decision")),
  identity_labeling          = list(display = "Identity Labeling",
                                    items   = c("mech_identity", "mech_followthrough")),
  commitment_escalation      = list(display = "Commitment Escalation",
                                    items   = c("mech_commitment", "mech_consistency")),
  anticipated_regret         = list(display = "Anticipated Regret",
                                    items   = c("mech_regret", "mech_disappointment")),
  # ASCII colon (not en-dash) so default grDevices::pdf + png devices
  # render cleanly without cairo/X11. The cited paper typesets these
  # as "Information -- Issue" / "Information -- Impact Efficacy" (LaTeX
  # en-dash); a colon is the closest punctuation that survives
  # non-cairo devices and still cleanly reads as "category: subtype".
  info_issue                 = list(display = "Information: Issue",
                                    items   = c("mech_learning", "mech_knowledge")),
  info_impact_efficacy       = list(display = "Information: Impact Efficacy",
                                    items   = c("mech_advocacy", "mech_impact"))
)
stopifnot(setequal(unlist(lapply(STRATEGY_PAIRS, `[[`, "items")), MECH_ITEMS))

# =============================================================================
# 2. Data assembly
# =============================================================================
# Same CONSORT cascade as 01/02 (`complete_case_filter()`), but with
# `require_attitudes = FALSE` because the Study 4 outcome is donations, not
# attitudes. The LMM-side filter then drops rows missing any LMM covariate.

study_4 <- load_study("study_4")

donation_df <- study_4 |>
  complete_case_filter(require_attitudes = FALSE, label = "03_donation") |>
  dplyr::transmute(
    persuadee         = persuadee_id,
    # `persuader` ID for the LMM random intercept `(1 | persuader)`.
    # Each AI conversation and each control conversation gets its own
    # UNIQUE persuader level so that AI and control behave like 1500+
    # independent "single-instance persuaders." Canvasser rows keep their
    # real persuader_id so the random intercept absorbs between-canvasser
    # variation across the 19 canvassers.
    #
    # An earlier draft collapsed all AI conversations under a single
    # `paste0("ai_", ai_model_full)` level. That made the AI group's
    # effective persuader-level replicate count equal to 1 and
    # inflated the AI fixed-effect SE substantially (Panel A AI CI
    # half-width was ~10.6 pp under the collapsed coding vs ~6.7 pp
    # under per-conversation coding; p went from 6.5e-04 to 2.1e-08).
    # The Canvasser CI was unaffected (it uses real persuader IDs in
    # both codings). Per-conversation coding matches the prereg framing.
    persuader         = dplyr::case_when(
      persuader_class == "canvasser" & !is.na(persuader_id) ~ as.character(persuader_id),
      persuader_class == "ai"      ~ paste0("ai_",      persuadee_session),
      persuader_class == "control" ~ paste0("control_", persuadee_session),
      TRUE                         ~ NA_character_
    ),
    group             = forcats::fct_relevel(droplevels(persuader_class), "control"),
    donation          = donation_amount,
    pre_attitude      = pre_attitude,
    pre_donation_will = pre_donation_willingness,
    age               = age,
    ideology          = ordered(ideology_coded),
    # carry partner-rating + mechanism items through for Panel C
    dplyr::across(dplyr::all_of(c(PARTNER_ITEMS, MECH_ITEMS)))
  )

# LMM-side complete-cases: drop any row missing a covariate so the lmer
# call sees a fully rectangular design matrix.
lmm_df <- donation_df |>
  dplyr::filter(!is.na(donation), !is.na(pre_attitude),
                !is.na(pre_donation_will), !is.na(age), !is.na(ideology))

message(sprintf(
  "  lmm_df: %d rows, %d persuaders, %d persuadees (per-arm: %s)",
  nrow(lmm_df), dplyr::n_distinct(lmm_df$persuader),
  dplyr::n_distinct(lmm_df$persuadee),
  paste(sprintf("%s=%d", names(table(lmm_df$group)), as.integer(table(lmm_df$group))),
        collapse = ", ")
))

# =============================================================================
# 3. Preregistered LMM
# =============================================================================
message("\n---- 3. preregistered LMM ----")

study4_donation_lmm <- lme4::lmer(
  donation ~ pre_attitude + pre_donation_will + age + ideology + group +
             (1 | persuader),
  data    = lmm_df,
  REML    = TRUE,
  control = lme4::lmerControl(optimizer = "bobyqa",
                              optCtrl   = list(maxfun = 100000))
)
save_rds(study4_donation_lmm, "study4_donation_lmm")

if (lme4::isSingular(study4_donation_lmm)) {
  message("  [WARN] lmer reports singular fit (likely persuader RE near zero).")
}

# =============================================================================
# 4. Panel A: TE vs Control + preregistered AI - Canvasser
# =============================================================================
message("\n---- 4. Panel A contrasts ----")

em <- emmeans::emmeans(study4_donation_lmm, ~ group, lmer.df = "asymptotic")

realworld_te_vs_control <- emmeans::contrast(em, method = "trt.vs.ctrl",
                                                ref = "control") |>
  summary(infer = TRUE, adjust = "none") |>
  tibble::as_tibble() |>
  dplyr::transmute(
    group     = sub(" - control$", "", as.character(contrast)),
    estimate  = estimate,
    lower_ci  = asymp.LCL,
    upper_ci  = asymp.UCL,
    p_value   = p.value
  )
save_rds(realworld_te_vs_control, "realworld_te_vs_control")

# Preregistered AI - Canvasser key contrast. We build it as a custom
# contrast vector (not via `pairs()` / `method = "pairwise"`) so the
# reported p-value is unadjusted -- Tukey multiplicity adjustment is
# appropriate for the all-pairs family but NOT for a single
# preregistered headline contrast. The 3-vector below is keyed to factor
# levels in their emmeans order: control, canvasser, ai.
group_levels   <- em@levels$group
stopifnot(identical(group_levels, c("control", "canvasser", "ai")))
ai_canv_vector <- c(control = 0, canvasser = -1, ai = 1)

realworld_ai_vs_canvasser <- emmeans::contrast(em,
    method = list("ai - canvasser" = ai_canv_vector[group_levels])) |>
  summary(infer = TRUE, adjust = "none") |>
  tibble::as_tibble() |>
  dplyr::transmute(
    contrast = as.character(contrast),
    estimate,
    lower_ci = asymp.LCL,
    upper_ci = asymp.UCL,
    p_value  = p.value
  )
save_rds(realworld_ai_vs_canvasser, "realworld_ai_vs_canvasser")

# =============================================================================
# 5. Panel B: extensive + intensive margins (per-condition + AI-Canvasser delta)
# =============================================================================
message("\n---- 5. Panel B margins ----")

# Use the full complete-case-filtered donation set (NOT the LMM subset):
# panel A is covariate-adjusted, but the descriptive margin decomposition
# in panel B is computed on the raw donation column without further
# conditioning.
panel_b_df <- donation_df |>
  dplyr::filter(!is.na(donation)) |>
  dplyr::mutate(donated = donation > 0)

per_group <- panel_b_df |>
  dplyr::group_by(group) |>
  dplyr::summarise(
    n          = dplyr::n(),
    n_donate   = sum(donated),
    p_donate   = mean(donated),
    n_donor    = sum(donated),
    mean_donor = mean(donation[donated]),
    sd_donor   = stats::sd(donation[donated]),
    .groups    = "drop"
  ) |>
  dplyr::mutate(
    # Wilson 95% CI on the extensive margin (P(donate > 0)).
    ext_ci = purrr::map2(n_donate, n,
                         ~ stats::prop.test(.x, .y)$conf.int),
    lo_ext = purrr::map_dbl(ext_ci, 1L) * 100,
    hi_ext = purrr::map_dbl(ext_ci, 2L) * 100,
    p_donate_pct = p_donate * 100,
    # Welch t 95% CI on the intensive margin (mean | donor).
    se_donor = dplyr::if_else(n_donor > 1L,
                              sd_donor / sqrt(n_donor),
                              NA_real_),
    lo_int   = dplyr::if_else(n_donor > 1L,
                              mean_donor - stats::qt(0.975, n_donor - 1L) * se_donor,
                              NA_real_),
    hi_int   = dplyr::if_else(n_donor > 1L,
                              mean_donor + stats::qt(0.975, n_donor - 1L) * se_donor,
                              NA_real_)
  ) |>
  dplyr::select(-ext_ci)

# AI - Canvasser deltas: closed-form Wald (extensive, pp scale) + Welch t
# (intensive, mean donation among donors).
ai_row   <- per_group |> dplyr::filter(group == "ai")
canv_row <- per_group |> dplyr::filter(group == "canvasser")

ext_delta <- {
  est <- ai_row$p_donate_pct - canv_row$p_donate_pct
  se  <- 100 * sqrt(
    ai_row$p_donate   * (1 - ai_row$p_donate)   / ai_row$n +
    canv_row$p_donate * (1 - canv_row$p_donate) / canv_row$n
  )
  tibble::tibble(margin = "extensive", estimate = est,
                 lower_ci = est - 1.96 * se,
                 upper_ci = est + 1.96 * se)
}

int_delta <- {
  est <- ai_row$mean_donor - canv_row$mean_donor
  se  <- sqrt(ai_row$sd_donor^2   / ai_row$n_donor +
              canv_row$sd_donor^2 / canv_row$n_donor)
  # Welch-Satterthwaite df.
  df_w <- (ai_row$sd_donor^2 / ai_row$n_donor +
           canv_row$sd_donor^2 / canv_row$n_donor)^2 /
          ((ai_row$sd_donor^2 / ai_row$n_donor)^2 / (ai_row$n_donor - 1L) +
           (canv_row$sd_donor^2 / canv_row$n_donor)^2 / (canv_row$n_donor - 1L))
  tibble::tibble(margin = "intensive", estimate = est,
                 lower_ci = est - stats::qt(0.975, df_w) * se,
                 upper_ci = est + stats::qt(0.975, df_w) * se)
}

realworld_margins <- list(
  per_group = per_group,
  deltas    = dplyr::bind_rows(ext_delta, int_delta)
)
save_rds(realworld_margins, "realworld_margins")

# =============================================================================
# 6. Panel C: per-item Welch t (AI - Canvasser)
# =============================================================================
message("\n---- 6. Panel C per-item Welch t ----")

# Compute AI - Canvasser difference of means + Welch 95% CI for one item.
# Returns a 1-row tibble; NA-safe if either arm has < 2 non-NA values.
welch_item <- function(df, item) {
  ai   <- df[[item]][df$group == "ai"]
  canv <- df[[item]][df$group == "canvasser"]
  ai   <- ai[!is.na(ai)]
  canv <- canv[!is.na(canv)]
  if (length(ai) < 2L || length(canv) < 2L) {
    return(tibble::tibble(item = item, n_ai = length(ai), n_canv = length(canv),
                          estimate = NA_real_, lower_ci = NA_real_,
                          upper_ci = NA_real_, p_value = NA_real_))
  }
  tt <- stats::t.test(ai, canv)
  tibble::tibble(
    item     = item,
    n_ai     = length(ai),
    n_canv   = length(canv),
    estimate = unname(tt$estimate[1] - tt$estimate[2]),
    lower_ci = tt$conf.int[1],
    upper_ci = tt$conf.int[2],
    p_value  = tt$p.value
  )
}

realworld_perception <- purrr::map_dfr(PARTNER_ITEMS,
                                          ~ welch_item(donation_df, .x))
save_rds(realworld_perception, "realworld_perception")

# Mechanism items: pre-sort by estimate (desc) so the figure renders the
# largest AI advantage at the top of its facet without re-sorting.
realworld_mechanism <- purrr::map_dfr(MECH_ITEMS,
                                         ~ welch_item(donation_df, .x)) |>
  dplyr::arrange(dplyr::desc(estimate))
save_rds(realworld_mechanism, "realworld_mechanism")

# =============================================================================
# 6b. Panel B: per-strategy composite Welch t (7 preregistered pairs)
# =============================================================================
# For each of the 7 preregistered strategies, compute the per-persuadee
# composite (item_a + item_b) / 2 (NA if either item NA), then run a
# Welch t (AI vs Canvasser) on that composite. This is the headline
# statistical object for Fig 4b: 1 row per strategy,
# bold pointrange + 95% CI.
message("\n---- 6b. Panel B per-strategy Welch t (composite) ----")

welch_composite <- function(df, items) {
  composite <- rowMeans(df[, items, drop = FALSE], na.rm = FALSE)
  ai   <- composite[df$group == "ai"]
  canv <- composite[df$group == "canvasser"]
  ai   <- ai[!is.na(ai)]
  canv <- canv[!is.na(canv)]
  if (length(ai) < 2L || length(canv) < 2L) {
    return(tibble::tibble(n_ai = length(ai), n_canv = length(canv),
                          estimate = NA_real_, lower_ci = NA_real_,
                          upper_ci = NA_real_, p_value = NA_real_))
  }
  tt <- stats::t.test(ai, canv)
  tibble::tibble(
    n_ai     = length(ai),
    n_canv   = length(canv),
    estimate = unname(tt$estimate[1] - tt$estimate[2]),
    lower_ci = tt$conf.int[1],
    upper_ci = tt$conf.int[2],
    p_value  = tt$p.value
  )
}

realworld_strategies <- purrr::imap_dfr(STRATEGY_PAIRS, function(spec, key) {
  out <- welch_composite(donation_df, spec$items)
  tibble::tibble(
    strategy = key,
    display  = spec$display,
    item_a   = spec$items[1],
    item_b   = spec$items[2]
  ) |>
    dplyr::bind_cols(out)
}) |>
  dplyr::arrange(dplyr::desc(estimate))
save_rds(realworld_strategies, "realworld_strategies")

# =============================================================================
# 7. Audit log
# =============================================================================
message("\n---- 7. audit log ----")

message("  Panel A TE vs control:")
for (i in seq_len(nrow(realworld_te_vs_control))) {
  r <- realworld_te_vs_control[i, ]
  message(sprintf("    %-10s  %+6.2f pp  [%+6.2f, %+6.2f]  (p = %.3g)",
                  r$group, r$estimate, r$lower_ci, r$upper_ci, r$p_value))
}

message("  Panel A AI - Canvasser (preregistered headline):")
r <- realworld_ai_vs_canvasser
message(sprintf("    %-20s  %+6.2f pp  [%+6.2f, %+6.2f]  (p = %.3g)",
                r$contrast, r$estimate, r$lower_ci, r$upper_ci, r$p_value))

message("  Panel B per-condition margins:")
for (i in seq_len(nrow(per_group))) {
  r <- per_group[i, ]
  message(sprintf(
    "    %-10s  n=%4d  P(donate)=%5.1f%% [%4.1f, %4.1f]   mean|donor=%5.1f [%4.1f, %4.1f] (n_donor=%d)",
    r$group, r$n, r$p_donate_pct, r$lo_ext, r$hi_ext,
    r$mean_donor, r$lo_int, r$hi_int, r$n_donor))
}
message("  Panel B AI - Canvasser deltas:")
for (i in seq_len(nrow(realworld_margins$deltas))) {
  r <- realworld_margins$deltas[i, ]
  message(sprintf("    %-10s  %+6.2f pp  [%+6.2f, %+6.2f]",
                  r$margin, r$estimate, r$lower_ci, r$upper_ci))
}

n_perc_sig     <- sum(realworld_perception$p_value < 0.05, na.rm = TRUE)
n_mech_sig     <- sum(realworld_mechanism$p_value  < 0.05, na.rm = TRUE)
n_strategy_sig <- sum(realworld_strategies$p_value < 0.05, na.rm = TRUE)

message(sprintf("  Panel B strategies (composite Welch t):  %d / %d at p < .05",
                n_strategy_sig, nrow(realworld_strategies)))
message("  Panel B strategies (sorted by composite estimate):")
for (i in seq_len(nrow(realworld_strategies))) {
  r <- realworld_strategies[i, ]
  flag <- if (!is.na(r$p_value) && r$p_value < 0.05) " *" else ""
  message(sprintf("    %-32s  %+6.2f pp  [%+6.2f, %+6.2f]  (p = %.3g)%s",
                  r$display, r$estimate, r$lower_ci, r$upper_ci, r$p_value, flag))
}

message(sprintf("  Panel C perception:  %d / %d items at p < .05  (full 7-item battery; main fig shows 4)",
                n_perc_sig, nrow(realworld_perception)))
message(sprintf("  Panel C mechanisms:  %d / %d items at p < .05  (full 14-item battery; not plotted directly -- drives Panel B faint per-item dots + SI table)",
                n_mech_sig, nrow(realworld_mechanism)))
message("  Per-item AI - Canvasser deltas (sorted desc; mechanism battery):")
for (i in seq_len(nrow(realworld_mechanism))) {
  r <- realworld_mechanism[i, ]
  flag <- if (!is.na(r$p_value) && r$p_value < 0.05) " *" else ""
  message(sprintf("    %-22s  %+6.2f pp  [%+6.2f, %+6.2f]  (p = %.3g)%s",
                  r$item, r$estimate, r$lower_ci, r$upper_ci, r$p_value, flag))
}

message("\n  done.")
