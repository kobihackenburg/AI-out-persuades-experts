# si_02_subgroups.R
# Subgroup splits + moderator interaction tests for two parallel universes:
#
#   * S1+S2 only -- the main-text mechanism section ("Under what
#     conditions do humans rival AI?") is narrated chronologically
#     before the Professional Canvasser study is introduced, so the
#     numbers cited there must come from an S1+S2-only fit (no
#     canvassers, no S3 AI models).
#
#   * S1-S3 -- the canvasser-inclusion robustness check that lives in
#     the SI; refits the same models on the full universe and shows the
#     same conclusions hold.
#
# Outputs (under output/results/, consumer after `->`):
#   subgroups_ai_vs_humans_s1_s2.rds     -> main-text per-subgroup contrast
#                                           ("AI vs pooled humans, by
#                                           subgroup level")
#   moderator_effects_s1_s2.rds          -> main-text moderator effect table
#   subgroups_ai_vs_humans.rds           -> SI canvasser-inclusion contrast
#   moderator_effects.rds                -> SI canvasser-inclusion moderator
#                                           effect table
#
# The per-subgroup and per-moderator fit objects are kept in memory only;
# the AI-vs-humans contrast / moderator-effect tables derived from them
# are the consumed deliverables.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))

message("\n==== si_02_subgroups.R ====")

studies <- load_studies(c("study_1", "study_2", "study_3"))

HUMAN_CLASSES_S1_S3 <- c("random_lay_person", "elite_lay_person",
                         "canvasser", "elite_debater",
                         "coached_elite_debater")
HUMAN_CLASSES_S1_S2 <- setdiff(HUMAN_CLASSES_S1_S3, "canvasser")

# -----------------------------------------------------------------------------
# Pooled frame builder (S1+S2 or S1+S2+S3 depending on `study_labels`).
# -----------------------------------------------------------------------------
build_pooled <- function(study_labels) {
  pooled <- dplyr::bind_rows(
    purrr::map(study_labels, function(lbl) {
      studies[[lbl]] |> dplyr::mutate(study = lbl)
    })
  ) |>
    dplyr::filter(!is.na(post_attitude), !is.na(pre_attitude)) |>
    # Canonical persuader RE coding (see 01_main_attitudes::prep). Without
    # this AI + control rows have NA persuader_id and lmer silently drops
    # them via `(1 | persuader_re)`.
    dplyr::mutate(
      persuader_re = dplyr::case_when(
        !is.na(persuader_id)                            ~ persuader_id,
        persuader_class == "ai" & !is.na(ai_model_full) ~ paste0("ai_", ai_model_full),
        TRUE                                            ~ paste0("solo_", persuadee_session)
      )
    )

  # Derived moderator splits. Median-defined dichotomies are computed on
  # the current pooled frame so the cut-point is internally consistent
  # within each universe (the S1+S2 median differs slightly from the
  # S1-S3 median for several scales).
  pooled <- pooled |>
    dplyr::mutate(
      mod_pre_agreement   = factor(ifelse(pre_attitude > 50, "agree", "disagree"),
                                   levels = c("disagree", "agree")),
      mod_ideology        = factor(dplyr::case_when(
                                      ideology_coded <= 1 ~ "left",
                                      ideology_coded >= 3 ~ "right",
                                      TRUE                ~ NA_character_),
                                   levels = c("left", "right")),
      mod_issue_knowledge = factor(ifelse(pre_issue_knowledge >
                                            stats::median(pre_issue_knowledge, na.rm = TRUE),
                                          "high", "low"),
                                   levels = c("low", "high")),
      mod_dogmatism       = factor(ifelse(dogmatism >
                                            stats::median(dogmatism, na.rm = TRUE),
                                          "high", "low"),
                                   levels = c("low", "high")),
      age_bucket          = factor(dplyr::case_when(
                                      age < 30 ~ "<30",
                                      age < 50 ~ "30-49",
                                      TRUE     ~ "50+")),
      empathic_trust_high = factor(ifelse(empathic_trust >
                                            stats::median(empathic_trust, na.rm = TRUE),
                                          "high", "low"))
    )
  pooled
}

SUBGROUPS <- c(
  "gender", "age_bucket", "ethnicity", "education", "income",
  "party", "ideology_text", "political_knowledge_hi",
  "ai_trust_high", "mod_dogmatism", "mod_pre_agreement",
  "mod_ideology", "mod_issue_knowledge", "empathic_trust_high"
)

# -----------------------------------------------------------------------------
# Per-subgroup pooled LMM
# -----------------------------------------------------------------------------
fit_subgroup <- function(name, pooled) {
  if (!name %in% names(pooled)) {
    message(sprintf("  [skip] %s (column not in the data)", name))
    return(NULL)
  }
  df <- pooled |>
    dplyr::filter(!is.na(.data[[name]])) |>
    dplyr::group_by(.data[[name]]) |>
    dplyr::group_modify(~ {
      fit <- tryCatch(
        lme4::lmer(
          post_attitude ~ pre_attitude + issue_id + persuader_class + study
                       + (1 | persuader_re) + (1 | persuadee_id),
          data    = .x,
          REML    = TRUE,
          control = lme4::lmerControl(optimizer = "bobyqa")
        ),
        error = function(e) NULL
      )
      tibble::tibble(fit = list(fit), n = nrow(.x))
    }) |>
    dplyr::ungroup() |>
    dplyr::rename(level = 1) |>
    dplyr::mutate(level = as.character(level))
  dplyr::bind_cols(tibble::tibble(subgroup = name), df)
}

# -----------------------------------------------------------------------------
# AI vs pooled-humans contrast for one fit
# -----------------------------------------------------------------------------
contrast_ai_vs_humans <- function(fit, human_classes) {
  if (is.null(fit)) return(NULL)
  em <- tryCatch(
    emmeans::emmeans(fit, ~ persuader_class, lmer.df = "asymptotic",
                     nesting = NULL),
    error = function(e) NULL
  )
  if (is.null(em)) return(NULL)
  grps      <- as.character(em@levels$persuader_class)
  ai_idx    <- which(grps == "ai")
  human_idx <- which(grps %in% human_classes)
  if (length(ai_idx) != 1L || length(human_idx) < 1L) return(NULL)
  ctr_coefs <- rep(0, length(grps))
  ctr_coefs[ai_idx]    <- 1
  ctr_coefs[human_idx] <- -1 / length(human_idx)
  res <- emmeans::contrast(em, list(ai_vs_humans = ctr_coefs)) |>
    summary(infer = TRUE, adjust = "none") |>
    tibble::as_tibble()
  tibble::tibble(
    n_humans = length(human_idx),
    estimate = res$estimate[1],
    se       = res$SE[1],
    lower_ci = res$asymp.LCL[1],
    upper_ci = res$asymp.UCL[1],
    p_value  = res$p.value[1]
  )
}

# -----------------------------------------------------------------------------
# Moderator interaction tests
# -----------------------------------------------------------------------------
MODERATORS <- tibble::tribble(
  ~moderator,                ~kind,          ~label,
  "pre_attitude",            "continuous",   "Pre-treatment attitude (0-100)",
  "pre_importance",          "continuous",   "Pre-treatment issue importance (0-100)",
  "pre_issue_knowledge",     "continuous",   "Pre-treatment issue knowledge (0-100)",
  "age",                     "continuous",   "Age (years)",
  "gender",                  "categorical",  "Gender",
  "education",               "categorical",  "Education",
  "income",                  "categorical",  "Income bracket",
  "ethnicity",               "categorical",  "Ethnicity",
  "party",                   "categorical",  "Party affiliation",
  "ideology_coded",          "continuous",   "Ideology (0 = Left, 4 = Right)",
  "political_knowledge_n",   "continuous",   "Political knowledge (0-4 correct)",
  "dogmatism",               "continuous",   "Dogmatism scale",
  "empathic_trust",          "continuous",   "Empathic-trust scale",
  "ai_trust",                "continuous",   "AI-trust scale"
)

contrast_ai_vs_humans_at <- function(em, mod_name, human_classes) {
  pc_lvls <- as.character(em@levels$persuader_class)
  ai_pos  <- which(pc_lvls == "ai")
  hum_pos <- which(pc_lvls %in% human_classes)
  if (length(ai_pos) != 1L || length(hum_pos) < 1L) return(NULL)
  coefs <- rep(0, length(pc_lvls))
  coefs[ai_pos]  <- 1
  coefs[hum_pos] <- -1 / length(hum_pos)
  emmeans::contrast(em, list(ai_vs_humans = coefs)) |>
    summary(infer = TRUE, adjust = "none") |>
    tibble::as_tibble() |>
    dplyr::rename_with(~ sub("\\.", "_", .x))
}

joint_wald_interaction_p <- function(fit, mod_name) {
  tryCatch({
    cf   <- summary(fit)$coefficients
    pat  <- sprintf("(^persuader_class[^:]+:%s|^%s:persuader_class)",
                    mod_name, mod_name)
    rows <- grep(pat, rownames(cf), value = TRUE, perl = TRUE)
    if (length(rows) < 1L) return(NA_real_)
    b   <- cf[rows, "Estimate"]
    V   <- as.matrix(stats::vcov(fit))[rows, rows, drop = FALSE]
    chi <- as.numeric(t(b) %*% solve(V) %*% b)
    stats::pchisq(chi, df = length(b), lower.tail = FALSE)
  }, error = function(e) {
    message(sprintf("  [wald p error] %s: %s", mod_name, conditionMessage(e)))
    NA_real_
  })
}

fit_one_moderator <- function(fm, mod, pooled) {
  d <- pooled[!is.na(pooled[[mod]]), , drop = FALSE]
  tryCatch(
    lme4::lmer(
      stats::as.formula(fm),
      data    = d,
      REML    = TRUE,
      control = lme4::lmerControl(optimizer = "bobyqa")
    ),
    error = function(e) {
      message(sprintf("  [fit error] %s: %s", mod, conditionMessage(e)))
      NULL
    }
  )
}

build_moderators <- function(pooled, human_classes) {
  moderators <- MODERATORS |>
    dplyr::mutate(
      formula = sprintf(
        "post_attitude ~ pre_attitude + issue_id + persuader_class * %s + study + (1 | persuader_re) + (1 | persuadee_id)",
        moderator
      )
    )
  moderators$fit <- purrr::map2(moderators$formula, moderators$moderator,
                                fit_one_moderator, pooled = pooled)

  moderator_effects <- purrr::map_dfr(seq_len(nrow(moderators)), function(i) {
    mod_name <- moderators$moderator[i]
    kind     <- moderators$kind[i]
    fit      <- moderators$fit[[i]]
    if (is.null(fit)) return(NULL)

    em <- tryCatch({
      if (kind == "continuous") {
        qs <- stats::quantile(pooled[[mod_name]], probs = c(0.25, 0.75),
                              na.rm = TRUE)
        at <- setNames(list(unname(qs)), mod_name)
        emmeans::emmeans(fit,
                         stats::as.formula(paste0("~ persuader_class | ", mod_name)),
                         at = at, lmer.df = "asymptotic", nesting = NULL)
      } else {
        emmeans::emmeans(fit,
                         stats::as.formula(paste0("~ persuader_class | ", mod_name)),
                         lmer.df = "asymptotic", nesting = NULL)
      }
    }, error = function(e) {
      message(sprintf("  [emmeans error] %s: %s", mod_name, conditionMessage(e)))
      NULL
    })
    if (is.null(em)) return(NULL)

    ctr <- contrast_ai_vs_humans_at(em, mod_name, human_classes)
    if (is.null(ctr)) return(NULL)
    p_int <- joint_wald_interaction_p(fit, mod_name)

    level_raw <- as.character(ctr[[mod_name]])
    level_lbl <- if (kind == "continuous") {
      fmt <- if (mod_name == "age") "%.0f" else "%.1f"
      sorted_vals <- sort(suppressWarnings(as.numeric(level_raw)))
      is_low      <- suppressWarnings(as.numeric(level_raw)) == sorted_vals[1]
      ifelse(is_low,
             sprintf("Low (Q1 = %s)",  sprintf(fmt, as.numeric(level_raw))),
             sprintf("High (Q3 = %s)", sprintf(fmt, as.numeric(level_raw))))
    } else {
      level_raw
    }

    tibble::tibble(
      moderator     = mod_name,
      kind          = kind,
      level         = level_lbl,
      estimate      = ctr$estimate,
      se            = ctr$SE,
      lower_ci      = ctr$asymp_LCL,
      upper_ci      = ctr$asymp_UCL,
      p_value       = ctr$p_value,
      p_interaction = p_int
    )
  })

  bh <- moderator_effects |>
    dplyr::distinct(moderator, p_interaction) |>
    dplyr::mutate(q_bh = stats::p.adjust(p_interaction, method = "BH"))

  moderator_effects <- moderator_effects |>
    dplyr::left_join(bh, by = c("moderator", "p_interaction"))

  list(moderators = moderators, moderator_effects = moderator_effects)
}

# -----------------------------------------------------------------------------
# Run the full pipeline for one universe
# -----------------------------------------------------------------------------
run_universe <- function(study_labels, human_classes, suffix) {
  message(sprintf("\n  ---- universe '%s' (studies: %s) ----",
                  suffix, paste(study_labels, collapse = ", ")))
  pooled <- build_pooled(study_labels) |>
    dplyr::filter(persuader_class %in% c("ai", "control", human_classes)) |>
    dplyr::mutate(persuader_class = droplevels(factor(persuader_class)))

  # subgroups
  subgroups <- purrr::map(SUBGROUPS, fit_subgroup, pooled = pooled) |>
    dplyr::bind_rows()

  subgroups_ai_vs_humans <- purrr::map_dfr(seq_len(nrow(subgroups)), function(i) {
    row <- subgroups[i, ]
    ctr <- contrast_ai_vs_humans(row$fit[[1]], human_classes)
    if (is.null(ctr)) return(NULL)
    tibble::tibble(subgroup = row$subgroup, level = row$level, n = row$n) |>
      dplyr::bind_cols(ctr)
  })

  # moderators
  mod_pack <- build_moderators(pooled, human_classes)

  # save under the appropriate suffix (only the consumed contrast +
  # moderator-effect tables; the raw per-subgroup / per-moderator fits
  # stay in memory)
  if (suffix == "s1_s2") {
    save_rds(subgroups_ai_vs_humans,     "subgroups_ai_vs_humans_s1_s2")
    save_rds(mod_pack$moderator_effects, "moderator_effects_s1_s2")
  } else {
    save_rds(subgroups_ai_vs_humans,     "subgroups_ai_vs_humans")
    save_rds(mod_pack$moderator_effects, "moderator_effects")
  }

  message("\n  Moderator interaction tests (sorted by p_int):")
  mef_print <- mod_pack$moderator_effects |>
    dplyr::distinct(moderator, p_interaction, q_bh) |>
    dplyr::arrange(p_interaction)
  for (i in seq_len(nrow(mef_print))) {
    r <- mef_print[i, ]
    message(sprintf("    %-22s  p_int = %s  q_BH = %s",
                    r$moderator,
                    if (is.na(r$p_interaction)) "    NA"
                    else if (r$p_interaction < 0.001) " <.001"
                    else sprintf("%6.3f", r$p_interaction),
                    if (is.na(r$q_bh)) "    NA"
                    else if (r$q_bh < 0.001) " <.001"
                    else sprintf("%6.3f", r$q_bh)))
  }
}

run_universe(c("study_1", "study_2"),             HUMAN_CLASSES_S1_S2, "s1_s2")
run_universe(c("study_1", "study_2", "study_3"),  HUMAN_CLASSES_S1_S3, "s1_s3")

message("  done.")
