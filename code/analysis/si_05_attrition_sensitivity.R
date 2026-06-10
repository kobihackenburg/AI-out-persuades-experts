# si_05_attrition_sensitivity.R
# Lee (2009) trimming bounds on the headline AI-vs-human contrasts in the
# studies where differential attrition was detected (Studies 1 and 2; see
# si_03_attrition.R).
#
# For a two-arm contrast with treatment t (lower attrition) and control c
# (higher attrition), Lee's bounds are constructed by trimming the lower-
# attrition arm by the "trim share"
#     q = (resp_rate_t - resp_rate_c) / resp_rate_t
# from each tail of its observed outcome distribution. The lower bound is
# the mean(treatment | trim top q) - mean(control), and the upper bound is
# mean(treatment | trim bottom q) - mean(control). When response rates are
# equal across arms, q = 0 and both bounds collapse to the naive estimate.
#
# Here we apply the same logic on a per-contrast basis: for each "AI vs
# human-class" contrast in Studies 1 and 2, we identify the higher-attrition
# arm (the one with the larger fraction of attrited persuadees), set q
# from the response-rate difference, and trim the lower-attrition arm.
# We adjust raw post_attitude by partialling out pre_attitude (the
# manuscript's primary covariate) before bounding to keep the bounds
# comparable in scale to the headline pp estimates. Lower/upper bounds and
# the naive estimate are all reported.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))

message("\n==== si_05_attrition_sensitivity.R ====")

studies <- load_studies(c("study_1", "study_2"))

# Per-arm matching: ai_constrained TRUE rows split out as their own arm.
arm_label <- function(cls, constrained) {
  cls <- as.character(cls)
  con <- !is.na(constrained) & constrained
  ifelse(cls == "ai" & con, "ai_constrained",
         ifelse(cls == "ai", "ai_info", cls))
}

# Contrasts we want to bound. Treatment is always "ai" (or its variant);
# the "human" comparator is one of the human persuader classes that was
# the strongest baseline in that study.
CONTRASTS <- tibble::tribble(
  ~study,    ~ai_arm,           ~human_arm,              ~label,
  "study_1", "ai_info",         "elite_debater",         "AI vs. Elite Debaters (S1)",
  "study_1", "ai_info",         "random_lay_person",     "AI vs. Random Laypeople (S1)",
  "study_2", "ai_info",         "coached_elite_debater", "Info-prompted AI vs. Coached Elite Debaters (S2)",
  "study_2", "ai_constrained",  "coached_elite_debater", "Constrained AI vs. Coached Elite Debaters (S2)",
  "study_2", "ai_info",         "ai_constrained",        "Info-prompted vs. Constrained AI (S2)"
)

# Residualise post on pre via a single covariate regression so we are
# bounding the same thing the headline LMM is estimating (group-vs-group
# difference in pre-adjusted post-attitude). For each contrast we fit
# the residualisation on the matched-and-final union of the two arms so
# the residual scale is consistent within the contrast.
prepare_arm <- function(df) {
  df |>
    dplyr::mutate(
      arm        = arm_label(persuader_class, ai_constrained),
      matched    = dplyr::coalesce(successfully_matched, FALSE),
      attrited_b = dplyr::coalesce(attrited, FALSE)
    ) |>
    dplyr::filter(matched, !is.na(pre_attitude))
}

lee_bounds <- function(arm_df, t_arm, c_arm) {
  d <- arm_df |> dplyr::filter(arm %in% c(t_arm, c_arm))
  if (nrow(d) == 0L) return(NULL)

  # Response rates in each arm.
  rr <- d |>
    dplyr::group_by(arm) |>
    dplyr::summarise(
      n        = dplyr::n(),
      n_obs    = sum(!attrited_b & !is.na(post_attitude)),
      resp     = n_obs / dplyr::n(),
      .groups  = "drop"
    )
  r_t <- rr$resp[rr$arm == t_arm]
  r_c <- rr$resp[rr$arm == c_arm]

  # Residualise post_attitude on pre_attitude using only complete cases.
  obs <- d |> dplyr::filter(!attrited_b & !is.na(post_attitude))
  fit <- stats::lm(post_attitude ~ pre_attitude, data = obs)
  obs$y_resid <- as.numeric(stats::residuals(fit)) +
                 mean(obs$post_attitude, na.rm = TRUE)

  yt <- obs$y_resid[obs$arm == t_arm]
  yc <- obs$y_resid[obs$arm == c_arm]

  naive <- mean(yt, na.rm = TRUE) - mean(yc, na.rm = TRUE)

  # Identify the lower-attrition (higher-response) arm and trim it.
  if (r_t >= r_c) {
    q <- (r_t - r_c) / r_t
    sorted   <- sort(yt)
    k        <- floor(q * length(sorted))
    lb_mean  <- if (k > 0) mean(sorted[seq.int(1, length(sorted) - k)]) else mean(sorted)
    ub_mean  <- if (k > 0) mean(sorted[seq.int(k + 1, length(sorted))]) else mean(sorted)
    lb <- lb_mean - mean(yc)
    ub <- ub_mean - mean(yc)
    trimmed_arm <- t_arm
  } else {
    q <- (r_c - r_t) / r_c
    sorted   <- sort(yc)
    k        <- floor(q * length(sorted))
    lb_mean  <- if (k > 0) mean(sorted[seq.int(k + 1, length(sorted))]) else mean(sorted)
    ub_mean  <- if (k > 0) mean(sorted[seq.int(1, length(sorted) - k)]) else mean(sorted)
    lb <- mean(yt) - lb_mean
    ub <- mean(yt) - ub_mean
    trimmed_arm <- c_arm
  }

  # Naive CI from a two-sample t on the residualised outcome (used only
  # to substantiate the "naive est. (95% CI)" column in the SI table).
  tt <- stats::t.test(yt, yc, var.equal = FALSE)

  tibble::tibble(
    n_t        = sum(rr$n[rr$arm == t_arm]),
    n_c        = sum(rr$n[rr$arm == c_arm]),
    resp_t     = r_t,
    resp_c     = r_c,
    trim_q     = abs(r_t - r_c) / max(r_t, r_c),
    trimmed    = trimmed_arm,
    naive_est  = naive,
    naive_lo   = unname(tt$conf.int[1]),
    naive_hi   = unname(tt$conf.int[2]),
    lee_lower  = lb,
    lee_upper  = ub
  )
}

rows <- purrr::pmap_dfr(CONTRASTS, function(study, ai_arm, human_arm, label) {
  df  <- prepare_arm(studies[[study]])
  res <- lee_bounds(df, ai_arm, human_arm)
  if (is.null(res)) return(NULL)
  tibble::tibble(study = study, contrast = label,
                 ai_arm = ai_arm, human_arm = human_arm) |>
    dplyr::bind_cols(res)
})

save_rds(rows, "lee_bounds")

message("  Lee-bound summary (pp, residualised post | pre):")
for (i in seq_len(nrow(rows))) {
  r <- rows[i, ]
  message(sprintf(
    "    %-55s naive %+5.2f  [%+5.2f, %+5.2f]   Lee [%+5.2f, %+5.2f]   trim q=%.3f (%s)",
    r$contrast, r$naive_est, r$naive_lo, r$naive_hi,
    r$lee_lower, r$lee_upper, r$trim_q, r$trimmed
  ))
}
message("  done.")
