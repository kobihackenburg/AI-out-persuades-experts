# si_01_issue_level.R
# Per-issue persuasive effects backing two distinct claims:
#
#   1. The main-text mechanism section ("Under what conditions do humans
#      rival AI?") is narrated chronologically over Studies 1 and 2 only,
#      so the main-text per-issue claim uses an S1+S2-only fit (no
#      canvassers, and no S3 AI models).
#
#   2. The Professional Canvasser paragraph in the next section notes
#      that refitting the mechanism analyses on the full S1-S3 universe
#      gives the same conclusions -- the canvasser-inclusion robustness.
#      That refit lives in the SI.
#
# Four outputs:
#   issue_level_s1_s2.rds              per-issue lmer fit (S1+S2 only;
#                                      backs main-text per-issue figure).
#   issue_level_ai_vs_humans_s1_s2.rds per-issue AI-vs-pooled-humans
#                                      emmeans contrast (S1+S2 only;
#                                      cited in the main-text mechanism
#                                      section).
#   issue_level_ai_vs_humans.rds       canvasser-inclusion AI-vs-pooled-
#                                      humans contrast on the full S1-S3
#                                      universe (SI table).

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))

message("\n==== si_01_issue_level.R ====")

studies <- load_studies(c("study_1", "study_2", "study_3"))

HUMAN_CLASSES_S1_S3 <- c("random_lay_person", "elite_lay_person",
                         "canvasser", "elite_debater",
                         "coached_elite_debater")
HUMAN_CLASSES_S1_S2 <- setdiff(HUMAN_CLASSES_S1_S3, "canvasser")

build_issue_df <- function(study_labels) {
  dplyr::bind_rows(
    purrr::map(study_labels, function(lbl) {
      studies[[lbl]] |> dplyr::mutate(study = lbl)
    })
  ) |>
    dplyr::filter(!is.na(post_attitude), !is.na(pre_attitude)) |>
    dplyr::transmute(
      study, issue_id,
      # Canonical persuader RE coding matching 01_main_attitudes::prep:
      # real persuader_id for humans, ai_<model> for AI, solo_<session>
      # singleton for controls (which have no persuader). Without the
      # control fallback lmer silently drops every control row because
      # `(1 | persuader)` cannot ingest NAs.
      persuader = dplyr::case_when(
        !is.na(persuader_id)                            ~ persuader_id,
        persuader_class == "ai" & !is.na(ai_model_full) ~ paste0("ai_", ai_model_full),
        TRUE                                            ~ paste0("solo_", persuadee_session)
      ),
      persuadee = persuadee_id,
      group     = forcats::fct_relevel(persuader_class, "control"),
      pre       = pre_attitude,
      post      = post_attitude
    )
}

fit_issue_level <- function(df) {
  df |>
    dplyr::mutate(group = droplevels(group)) |>
    dplyr::group_by(issue_id) |>
    dplyr::group_modify(~ {
      fit <- tryCatch(
        lme4::lmer(
          post ~ pre + group + study + (1 | persuader) + (1 | persuadee),
          data    = .x |> dplyr::mutate(group = droplevels(group)),
          REML    = TRUE,
          control = lme4::lmerControl(optimizer = "bobyqa")
        ),
        error = function(e) NULL
      )
      if (is.null(fit)) return(tibble::tibble(fit = list(NULL)))
      tibble::tibble(fit = list(fit), n = nrow(.x))
    }) |>
    dplyr::ungroup()
}

ai_vs_humans_contrast <- function(issue_level_tbl, human_classes) {
  purrr::map_dfr(seq_len(nrow(issue_level_tbl)), function(i) {
    issue <- as.character(issue_level_tbl$issue_id[i])
    fit   <- issue_level_tbl$fit[[i]]
    if (is.null(fit)) return(NULL)
    em <- tryCatch(
      emmeans::emmeans(fit, ~ group, lmer.df = "asymptotic", nesting = NULL),
      error = function(e) NULL
    )
    if (is.null(em)) return(NULL)
    grps      <- as.character(em@levels$group)
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
      issue_id  = issue,
      n_humans  = length(human_idx),
      estimate  = res$estimate[1],
      se        = res$SE[1],
      lower_ci  = res$asymp.LCL[1],
      upper_ci  = res$asymp.UCL[1],
      p_value   = res$p.value[1]
    )
  })
}

# ---- (1) S1+S2-only fit (main-text mechanism section) -------------------
message("\n  S1+S2-only per-issue fit (main-text mechanism section)")
issue_df_s1_s2 <- build_issue_df(c("study_1", "study_2")) |>
  # canvassers exist only in Study 3, so the filter above already removes
  # them, but be explicit:
  dplyr::filter(group != "canvasser")
issue_level_s1_s2 <- fit_issue_level(issue_df_s1_s2)
save_rds(issue_level_s1_s2, "issue_level_s1_s2")

issue_level_ai_vs_humans_s1_s2 <- ai_vs_humans_contrast(
  issue_level_s1_s2,
  HUMAN_CLASSES_S1_S2
)
save_rds(issue_level_ai_vs_humans_s1_s2, "issue_level_ai_vs_humans_s1_s2")

# ---- (2) Canvasser-inclusion (S1-S3) fit (SI robustness) ----------------
message("\n  S1-S3 per-issue fit (SI canvasser-inclusion robustness)")
issue_df <- build_issue_df(c("study_1", "study_2", "study_3"))
issue_level <- fit_issue_level(issue_df)

issue_level_ai_vs_humans <- ai_vs_humans_contrast(
  issue_level,
  HUMAN_CLASSES_S1_S3
)
save_rds(issue_level_ai_vs_humans, "issue_level_ai_vs_humans")

message("  done.")
