# si_19_persuader_strategies.R
# =============================================================================
# Human persuaders' self-reported strategies (Studies 1-3).
#
# After each conversation, human persuaders rated how much each of nine
# statements described what they did (0-100; SI "Persuader post-conversation
# assessment"). Self-reports, so descriptive only. Reads
# output/results/_py/si_19_strategy_reports.csv, written by
# code/analysis/py/si_19_strategy_reports.py.
#
# Outputs (under output/results/, consumer after `->`):
#   strategy_counts.rds            -> SI prose (conversations / persuaders per class)
#   strategy_top_shares.rds        -> tab_strategy_top_shares
#   strategy_perm_tests.rds        -> tab_strategy_top_shares (permutation rows)
#   strategy_mean_ratings.rds      -> tab_strategy_mean_ratings
#   strategy_coaching_shift.rds    -> tab_strategy_coaching_shift
#   strategy_validity.rds          -> SI prose (self-rated facts vs fact-checked claims)
#   strategy_info_led_coverage.rds -> tab_strategy_info_led + 99_extract_numbers
#   strategy_info_led_te.rds       -> tab_strategy_info_led
#   strategy_info_led_contrasts.rds -> tab_strategy_info_led + 99_extract_numbers
#
# Sections:
#   1. Top-rated strategy per conversation (argmax over the eight strategies,
#      excluding "relied on instinct"; ties split), averaged within persuader
#      and then across persuaders in a class. Permutation tests shuffle class
#      labels across persuaders (chi-square statistic on conversation counts).
#   2. Mean rating of each item: rating ~ class + (1 | persuader); contrasts
#      vs Random Laypeople, Holm across items x contrasts.
#   3. Coaching shift for debaters observed before and after coaching: paired
#      persuader-level change in top-strategy shares, and
#      rating ~ coached + (1 | persuader).
#   4. Validity: self-rated "facts" vs log(1 + fact-checked claims), within
#      class and across class means.
#   5. Information-led vs non-information-led human conversations. A human
#      conversation is non-information-led if its highest rating among the
#      non-informational strategies exceeds its "facts and evidence" rating.
#      Two definitions of the non-informational set: stories, partner's values
#      and open questions / reflective listening (main), and stories and
#      partner's values only (no_deep). Preregistered pooled LMM with human
#      conversations split by strategy.
# =============================================================================

source(here::here("code/analysis/_setup.R"))
source(here::here("code/analysis/_load_data.R"))
source(here::here("code/analysis/_lmm_helpers.R"))

message("\n==== si_19_persuader_strategies.R ====")

set.seed(20261013)
N_PERM <- 5000

STRATEGIES <- c(
  strat_information     = "Facts and evidence",
  strat_deep_canvassing = "Open questions, reflective listening",
  strat_storytelling    = "Story",
  strat_moral_reframing = "Partner's values",
  strat_norms           = "Social norms",
  strat_debate          = "Many reasons, rebuttals",
  strat_mega            = "Combined strategies",
  strat_deception       = "Made-up or exaggerated information"
)
ITEMS <- c(STRATEGIES, strat_none = "Relied on instinct")

CLASSES <- c(random_lay_person     = "Random Laypeople",
             elite_lay_person      = "Selected Laypeople",
             elite_debater         = "Elite Debaters",
             coached_elite_debater = "Coached Elite Debaters",
             canvasser             = "Professional Canvassers")

NON_INFO_DEFINITIONS <- list(
  main    = c("strat_storytelling", "strat_moral_reframing", "strat_deep_canvassing"),
  no_deep = c("strat_storytelling", "strat_moral_reframing")
)

strat <- readr::read_csv(file.path(RESULTS_PY, "si_19_strategy_reports.csv"))

studies <- load_studies(c("study_1", "study_2", "study_3"))
cc <- purrr::imap_dfr(studies, ~ dplyr::mutate(.x, study = .y)) |>
  complete_case_filter(label = "si_19")

d <- cc |>
  dplyr::filter(persuader_class %in% names(CLASSES), !is.na(persuader_id)) |>
  dplyr::select(row_id, study, persuader_class, persuader_id, fact_checked, n_fact_claims) |>
  dplyr::inner_join(strat, by = "row_id") |>
  dplyr::filter(dplyr::if_any(dplyr::all_of(names(STRATEGIES)), ~ !is.na(.x))) |>
  dplyr::mutate(class = factor(persuader_class, levels = names(CLASSES)),
                unit = paste(persuader_class, persuader_id, sep = "|"))

strategy_counts <- d |>
  dplyr::group_by(class) |>
  dplyr::summarise(n_conversations = dplyr::n(), n_persuaders = dplyr::n_distinct(persuader_id),
                   .groups = "drop")
save_rds(strategy_counts, "strategy_counts")
print(strategy_counts)

# =============================================================================
# 1. Top-rated strategy shares and permutation tests
# =============================================================================
top_long <- d |>
  dplyr::select(row_id, unit, class, dplyr::all_of(names(STRATEGIES))) |>
  tidyr::pivot_longer(dplyr::all_of(names(STRATEGIES)), names_to = "item", values_to = "rating") |>
  dplyr::filter(!is.na(rating)) |>
  dplyr::group_by(row_id) |>
  dplyr::filter(rating == max(rating)) |>
  dplyr::mutate(weight = 1 / dplyr::n()) |>
  dplyr::ungroup()

# persuader-unit x strategy matrix of (tie-split) conversation counts; a
# debater observed before and after coaching is two units. Units in order of
# first appearance, not by id, so the permutation draws do not depend on the
# (pseudonymised, in the public release) id labels.
unit_mat <- top_long |>
  dplyr::group_by(unit, item) |>
  dplyr::summarise(w = sum(weight), .groups = "drop") |>
  tidyr::pivot_wider(names_from = item, values_from = w, values_fill = 0)
unit_mat <- unit_mat[match(unique(top_long$unit), unit_mat$unit), ]
unit_class <- sub("\\|.*$", "", unit_mat$unit)
P <- as.matrix(unit_mat[, names(STRATEGIES)])
rownames(P) <- unit_mat$unit

unit_shares <- sweep(P, 1, rowSums(P), "/")
strategy_top_shares <- purrr::map_dfr(names(CLASSES), function(cl) {
  idx <- unit_class == cl
  tibble::tibble(class = CLASSES[[cl]], item = names(STRATEGIES), strategy = unname(STRATEGIES),
                 persuader_mean_share = colMeans(unit_shares[idx, , drop = FALSE]),
                 conversation_share = colSums(P[idx, , drop = FALSE]) / sum(P[idx, ]),
                 n_persuaders = sum(idx))
})
save_rds(strategy_top_shares, "strategy_top_shares")
print(tidyr::pivot_wider(dplyr::select(strategy_top_shares, strategy, class, persuader_mean_share),
                         names_from = class, values_from = persuader_mean_share), width = Inf)

chi_stat <- function(mat, labels) {
  tab <- rowsum(mat, labels)
  expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  keep <- expected > 0
  sum((tab[keep] - expected[keep])^2 / expected[keep])
}

perm_test <- function(classes) {
  idx <- unit_class %in% classes
  mat <- P[idx, , drop = FALSE]
  lab <- unit_class[idx]
  obs <- chi_stat(mat, lab)
  null <- replicate(N_PERM, chi_stat(mat, sample(lab)))
  tibble::tibble(comparison = paste(CLASSES[classes], collapse = " vs "),
                 n_persuaders = sum(idx), chisq = obs,
                 p_perm = (sum(null >= obs) + 1) / (N_PERM + 1))
}

uncoached <- setdiff(names(CLASSES), "coached_elite_debater")
omnibus <- dplyr::bind_rows(
  dplyr::mutate(perm_test(uncoached), test = "omnibus, four uncoached classes"),
  dplyr::mutate(perm_test(names(CLASSES)), test = "omnibus, all five classes (debaters appear twice)")
)
pairwise <- purrr::map_dfr(setdiff(names(CLASSES), "random_lay_person"),
                           ~ perm_test(c("random_lay_person", .x))) |>
  dplyr::mutate(test = "vs Random Laypeople", p_holm = p.adjust(p_perm, "holm"))
strategy_perm_tests <- dplyr::bind_rows(omnibus, pairwise)
save_rds(strategy_perm_tests, "strategy_perm_tests")
print(strategy_perm_tests, width = Inf)

# =============================================================================
# 2. Mean ratings by class
# =============================================================================
strategy_mean_ratings <- purrr::map_dfr(names(ITEMS), function(it) {
  dat <- dplyr::filter(d, !is.na(.data[[it]]))
  m <- lme4::lmer(stats::as.formula(paste(it, "~ class + (1 | persuader_id)")), data = dat,
                  control = lme4::lmerControl(optimizer = "bobyqa"))
  emm <- emmeans::emmeans(m, ~ class, lmer.df = "asymptotic")
  means <- as.data.frame(emm) |>
    dplyr::transmute(class = CLASSES[as.character(class)], estimate = emmean,
                     conf.low = asymp.LCL, conf.high = asymp.UCL, type = "mean")
  contr <- as.data.frame(summary(emmeans::contrast(emm, "trt.vs.ctrl", ref = 1, adjust = "none"),
                                 infer = TRUE)) |>
    dplyr::transmute(class = CLASSES[sub(" - random_lay_person$", "", as.character(contrast))],
                     estimate, conf.low = asymp.LCL, conf.high = asymp.UCL, p = p.value,
                     type = "difference vs Random Laypeople")
  dplyr::bind_rows(means, contr) |> dplyr::mutate(item = it, strategy = ITEMS[[it]], .before = 1)
})
is_contrast <- strategy_mean_ratings$type != "mean"
strategy_mean_ratings$p_holm <- NA_real_
strategy_mean_ratings$p_holm[is_contrast] <- p.adjust(strategy_mean_ratings$p[is_contrast], "holm")
save_rds(strategy_mean_ratings, "strategy_mean_ratings")
print(dplyr::filter(strategy_mean_ratings, type == "mean") |>
        dplyr::select(strategy, class, estimate) |>
        tidyr::pivot_wider(names_from = class, values_from = estimate), width = Inf)

# =============================================================================
# 3. Coaching shift (debaters observed before and after coaching)
# =============================================================================
coached_people <- intersect(d$persuader_id[d$persuader_class == "elite_debater"],
                            d$persuader_id[d$persuader_class == "coached_elite_debater"])
deb_units <- unit_class %in% c("elite_debater", "coached_elite_debater") &
  sub("^[^|]*\\|", "", rownames(P)) %in% coached_people

share_shift <- purrr::map_dfr(names(STRATEGIES), function(it) {
  sh <- tibble::tibble(unit = rownames(P)[deb_units], share = unit_shares[deb_units, it]) |>
    dplyr::mutate(person = sub("^[^|]*\\|", "", unit),
                  phase = ifelse(startsWith(unit, "coached"), "after", "before")) |>
    dplyr::select(-unit) |>
    tidyr::pivot_wider(names_from = phase, values_from = share)
  tt <- stats::t.test(sh$after, sh$before, paired = TRUE)
  tibble::tibble(item = it, strategy = STRATEGIES[[it]], measure = "top-strategy share",
                 before = mean(sh$before), after = mean(sh$after),
                 difference = unname(tt$estimate), conf.low = tt$conf.int[1], conf.high = tt$conf.int[2],
                 p = tt$p.value, n_persuaders = nrow(sh))
})

deb <- dplyr::filter(d, persuader_id %in% coached_people,
                     persuader_class %in% c("elite_debater", "coached_elite_debater")) |>
  dplyr::mutate(coached = as.integer(persuader_class == "coached_elite_debater"))
rating_shift <- purrr::map_dfr(names(ITEMS), function(it) {
  m <- lme4::lmer(stats::as.formula(paste(it, "~ coached + (1 | persuader_id)")), data = deb,
                  control = lme4::lmerControl(optimizer = "bobyqa"))
  td <- broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE) |> dplyr::filter(term == "coached")
  tibble::tibble(item = it, strategy = ITEMS[[it]], measure = "mean rating",
                 before = lme4::fixef(m)[["(Intercept)"]],
                 after = lme4::fixef(m)[["(Intercept)"]] + td$estimate,
                 difference = td$estimate, conf.low = td$conf.low, conf.high = td$conf.high,
                 p = 2 * stats::pnorm(-abs(td$statistic)), n_persuaders = length(coached_people))
})
strategy_coaching_shift <- dplyr::bind_rows(share_shift, rating_shift)
save_rds(strategy_coaching_shift, "strategy_coaching_shift")
print(strategy_coaching_shift, n = Inf, width = Inf)

# =============================================================================
# 4. Validity: self-rated facts vs fact-checked claims
# =============================================================================
fc <- dplyr::filter(d, fact_checked, !is.na(n_fact_claims), !is.na(strat_information)) |>
  dplyr::mutate(log_claims = log1p(n_fact_claims))
cor_row <- function(x, y, level, n_label) {
  ct <- stats::cor.test(x, y)
  tibble::tibble(level = level, n = n_label, r = unname(ct$estimate),
                 conf.low = ct$conf.int[1], conf.high = ct$conf.int[2], p = ct$p.value)
}
within_class <- purrr::map_dfr(names(CLASSES), function(cl) {
  x <- dplyr::filter(fc, persuader_class == cl)
  if (nrow(x) < 10) return(NULL)
  dplyr::mutate(cor_row(x$strat_information, x$log_claims, "within class", nrow(x)),
                class = CLASSES[[cl]], .before = 1)
})
fc_resid <- fc |>
  dplyr::group_by(class) |>
  dplyr::mutate(info_c = strat_information - mean(strat_information),
                claims_c = log_claims - mean(log_claims)) |>
  dplyr::ungroup()
class_means <- fc |>
  dplyr::group_by(class) |>
  dplyr::summarise(mean_info = mean(strat_information), mean_claims = mean(n_fact_claims),
                   mean_log_claims = mean(log_claims), n = dplyr::n(), .groups = "drop")
correlations <- dplyr::bind_rows(
  within_class,
  dplyr::mutate(cor_row(fc_resid$info_c, fc_resid$claims_c, "pooled within class", nrow(fc_resid)),
                class = "All human classes", .before = 1),
  dplyr::mutate(cor_row(class_means$mean_info, class_means$mean_log_claims, "across class means",
                        nrow(class_means)), class = "Class means", .before = 1)
)
strategy_validity <- list(correlations = correlations, class_means = class_means)
save_rds(strategy_validity, "strategy_validity")
print(class_means)
print(correlations, width = Inf)

# =============================================================================
# 5. Information-led vs non-information-led human conversations
# =============================================================================
base <- cc |>
  dplyr::transmute(
    row_id,
    persuadee = persuadee_id,
    persuader = dplyr::case_when(
      !is.na(persuader_id)                            ~ persuader_id,
      persuader_class == "ai" & !is.na(ai_model_full) ~ paste0("ai_", ai_model_full),
      TRUE                                            ~ paste0("solo_", persuadee_session)
    ),
    issue = issue_id, pre = pre_attitude, post = post_attitude,
    attempt = dplyr::coalesce(as.integer(persuadee_attempt_number), 1L),
    persuader_class,
    ai_constrained = dplyr::coalesce(ai_constrained, FALSE)
  )

run_definition <- function(cols, name) {
  style <- strat |>
    dplyr::mutate(non_info = do.call(pmax, c(dplyr::pick(dplyr::all_of(cols)), na.rm = TRUE)),
                  human_style = dplyr::if_else(non_info > strat_information,
                                               "human_non_info_led", "human_info_led")) |>
    dplyr::select(row_id, human_style)

  dd <- base |>
    dplyr::left_join(style, by = "row_id") |>
    dplyr::mutate(group = dplyr::case_when(
      persuader_class == "control"             ~ "control",
      persuader_class == "ai" & ai_constrained ~ "ai_constrained",
      persuader_class == "ai"                  ~ "ai",
      !is.na(human_style)                      ~ human_style,
      TRUE                                     ~ NA_character_
    ))
  n_human <- sum(!dd$persuader_class %in% c("control", "ai"))
  dd <- dplyr::filter(dd, !is.na(group))
  message(sprintf("  [%s] human conversations with strategy report: %d of %d",
                  name, sum(grepl("^human", dd$group)), n_human))

  coverage <- dd |>
    dplyr::filter(grepl("^human", group)) |>
    dplyr::count(persuader_class, group) |>
    tidyr::pivot_wider(names_from = group, values_from = n, values_fill = 0) |>
    dplyr::mutate(share_non_info_led = human_non_info_led / (human_non_info_led + human_info_led))
  overall <- mean(dd$group[grepl("^human", dd$group)] == "human_non_info_led")
  coverage <- dplyr::bind_rows(coverage, tibble::tibble(persuader_class = "all_humans",
                                                        share_non_info_led = overall)) |>
    dplyr::mutate(definition = name, .before = 1)

  m <- fit_pooled_lmm(dd, "group", label = paste("si_19 strategy split", name))
  te <- tidy_te_vs_control(m, "group") |> dplyr::mutate(definition = name, .before = 1)

  em <- emmeans::emmeans(m, ~ group, lmer.df = "asymptotic")
  lv <- as.character(em@levels$group)
  vec <- function(pos, neg) { v <- setNames(numeric(length(lv)), lv); v[pos] <- 1; v[neg] <- -1; v }
  contr <- emmeans::contrast(em, list(
    ai_minus_non_info_led_humans       = vec("ai", "human_non_info_led"),
    ai_minus_info_led_humans           = vec("ai", "human_info_led"),
    non_info_led_minus_info_led_humans = vec("human_non_info_led", "human_info_led")
  )) |>
    summary(infer = TRUE) |>
    tibble::as_tibble() |>
    dplyr::transmute(definition = name, contrast, estimate,
                     lower_ci = asymp.LCL, upper_ci = asymp.UCL, p_value = p.value)

  list(coverage = coverage, te = te, contrasts = contr)
}

res <- purrr::imap(NON_INFO_DEFINITIONS, run_definition)
strategy_info_led_coverage  <- purrr::map_dfr(res, "coverage")
strategy_info_led_te        <- purrr::map_dfr(res, "te")
strategy_info_led_contrasts <- purrr::map_dfr(res, "contrasts")
save_rds(strategy_info_led_coverage,  "strategy_info_led_coverage")
save_rds(strategy_info_led_te,        "strategy_info_led_te")
save_rds(strategy_info_led_contrasts, "strategy_info_led_contrasts")
print(strategy_info_led_coverage, n = Inf)
print(strategy_info_led_te, n = Inf)
print(strategy_info_led_contrasts, n = Inf)

message("\n  done.")
