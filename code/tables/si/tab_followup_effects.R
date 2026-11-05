# tab_followup_effects.R
# SI Table: pooled Studies 1-3 effects immediately and at follow-up in the
# re-surveyed conversations, with the share of each effect retained.
#   Panel 1 - each persuader class vs control (preregistered pooled LMM)
#   Panel 2 - AI, Constrained AI and pooled human persuaders vs control, and
#             AI minus pooled humans; share retained = follow-up / immediate
#             effect, with persuadee-clustered bootstrap CIs for AI, humans
#             and their difference.
# Reads RDS written by code/analysis/si_13_followup.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/tables/table_style.R"))

message("\n==== code/tables/si/tab_followup_effects.R ====")

CLASS_LABELS <- c(PERSUADER_CLASS_LABELS, ai_constrained = "Constrained AI",
                  human = "All human persuaders")

fmt_cell <- function(estimate, lower_ci, upper_ci) {
  ifelse(is.na(estimate), "",
         sprintf("%+.2f [%+.2f, %+.2f]", estimate, lower_ci, upper_ci))
}
fmt_share <- function(estimate, lower_ci = NA_real_, upper_ci = NA_real_) {
  ifelse(is.na(lower_ci), sprintf("%.0f%%", 100 * estimate),
         sprintf("%.0f%% [%.0f, %.0f]", 100 * estimate, 100 * lower_ci, 100 * upper_ci))
}

by_class <- load_rds("followup_by_class")
gap      <- load_rds("followup_ai_vs_human")
ret      <- load_rds("followup_retention")

panel_class <- by_class |>
  dplyr::mutate(cell = fmt_cell(estimate, lower_ci, upper_ci)) |>
  dplyr::select(group, wave, cell) |>
  tidyr::pivot_wider(names_from = wave, values_from = cell) |>
  dplyr::mutate(order = match(group, c(PERSUADER_CLASS_ORDER, "ai_constrained"))) |>
  dplyr::arrange(order) |>
  dplyr::transmute(panel = "By persuader class", Persuader = unname(CLASS_LABELS[group]),
                   Immediate = immediate, `Follow-up` = `follow-up`, `Share retained` = "")

lmm <- ret$lmm
share_ci <- function(q) {
  r <- lmm[lmm$quantity == q, ]
  fmt_share(r$estimate, r$lower_ci, r$upper_ci)
}
pick <- function(spec) gap[gap$spec == spec, ]
te_row <- function(g) {
  post <- pick(paste(g, "vs control, immediate"))
  fu   <- pick(paste(g, "vs control, follow-up"))
  tibble::tibble(Persuader = unname(CLASS_LABELS[g]),
                 Immediate = fmt_cell(post$estimate, post$lower_ci, post$upper_ci),
                 `Follow-up` = fmt_cell(fu$estimate, fu$lower_ci, fu$upper_ci))
}
diff_post <- pick("AI - human, immediate")
diff_fu   <- pick("AI - human, follow-up")
panel_pooled <- dplyr::bind_rows(
  te_row("ai")             |> dplyr::mutate(`Share retained` = share_ci("retained_ai")),
  te_row("ai_constrained") |> dplyr::mutate(`Share retained` = fmt_share(
    ret$shares$share_retained[ret$shares$group == "ai_constrained"])),
  te_row("human")          |> dplyr::mutate(`Share retained` = share_ci("retained_human")),
  tibble::tibble(Persuader = "AI minus all human persuaders",
                 Immediate = fmt_cell(diff_post$estimate, diff_post$lower_ci, diff_post$upper_ci),
                 `Follow-up` = fmt_cell(diff_fu$estimate, diff_fu$lower_ci, diff_fu$upper_ci),
                 `Share retained` = paste0(sprintf("%+.0f", 100 * lmm$estimate[lmm$quantity == "retained_diff"]),
                                           sprintf(" pp [%+.0f, %+.0f]",
                                                   100 * lmm$lower_ci[lmm$quantity == "retained_diff"],
                                                   100 * lmm$upper_ci[lmm$quantity == "retained_diff"])))
) |> dplyr::mutate(panel = "AI vs pooled human persuaders", .before = 1)

tbl <- gt::gt(dplyr::bind_rows(panel_class, panel_pooled), groupname_col = "panel") |>
  style_table(
    title    = gt::md("**Persuasive effects immediately and at follow-up (Studies 1--3).**"),
    subtitle = sprintf(paste0(
      "Effects vs control on the 0--100 attitude scale (pp [95%% CI]) in the %s re-surveyed conversations, ",
      "from the preregistered pooled model (pre-treatment attitude, issue, persuader class and session ",
      "number, with persuader and persuadee random intercepts) fitted to the immediate and to the ",
      "follow-up attitude. Share retained is the follow-up effect divided by the immediate effect, with ",
      "95%% CIs from a persuadee-clustered bootstrap (B = %s); the last row gives the AI minus human ",
      "difference in share retained."),
      format(ret$n, big.mark = ","), format(lmm$n_boot[1], big.mark = ","))
  ) |>
  gt::tab_options(row_group.font.weight = "bold")

save_table(tbl, "tab_followup_effects", TABLES_SI,
           label  = "tab:followup_effects",
           layout = "shrink",
           new    = TRUE)

message("  done.")
