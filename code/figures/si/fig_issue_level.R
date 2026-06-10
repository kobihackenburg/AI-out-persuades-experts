# fig_issue_level.R
# SI Figure: per-issue persuasive effect of AI on each of the 10 policy
# issues. Two panels:
#   (a) AI vs control       -- raw treatment effect on attitudes; backs
#                              the "no negative-effect issues" robustness
#                              check.
#   (b) AI vs pooled humans -- the main-text-cited contrast (manuscript:
#                              "all 10 issues at p < .05, 2.9 to 9.7 pp").
# Issues are ordered identically in both panels (sorted by AI-vs-pooled-
# humans estimate, descending) so the reader can scan the same row across.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/figures/theme.R"))
source(here::here("code/figures/palette.R"))

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

message("\n==== code/figures/si/fig_issue_level.R ====")

# Per-issue figure backs the main-text mechanism-section claim, which is
# narrated chronologically over Studies 1-2. Use the S1+S2-only fit;
# canvasser-inclusion analogue is in SI Section
# si:mechanism-with-canvasser.
issues  <- load_rds("issue_level_s1_s2")
ai_vs_h <- load_rds("issue_level_ai_vs_humans_s1_s2")

ISSUE_LABELS <- c(
  historic_objects     = "Return historic objects to other countries",
  benefit_cap          = "Abolish the two-child benefit cap",
  immigration          = "Allow more immigrants",
  monarchy             = "Keep the monarchy",
  assisted_suicide     = "Legalise physician-assisted suicide",
  social_media_ban     = "Ban social media for under-16s",
  ukraine_peace_deal   = "Back Ukraine peace deal (cede territory)",
  protester_penalties  = "Tougher penalties for road-blocking protesters",
  controversial_speech = "Protect controversial speech at universities",
  pension_age          = "Raise the state pension age"
)

# ---- Panel a: AI vs control --------------------------------------------
panel_a_raw <- issues |>
  dplyr::rowwise() |>
  dplyr::filter(!is.null(fit)) |>
  dplyr::mutate(
    coefs = list(tibble::as_tibble(summary(fit)$coefficients, rownames = "term"))
  ) |>
  tidyr::unnest(coefs) |>
  dplyr::filter(term == "groupai") |>
  dplyr::transmute(
    issue_id  = as.character(issue_id),
    estimate  = Estimate,
    lower_ci  = Estimate - 1.96 * `Std. Error`,
    upper_ci  = Estimate + 1.96 * `Std. Error`
  )

# ---- Panel b: AI vs pooled humans --------------------------------------
panel_b_raw <- ai_vs_h |>
  dplyr::transmute(
    issue_id = as.character(issue_id),
    estimate, lower_ci, upper_ci
  )

# ---- Shared issue ordering ---------------------------------------------
# Sort by panel-b estimate (AI vs humans) descending so the top row in
# each panel is the issue where AI most outpaces the human comparators.
issue_order <- panel_b_raw |>
  dplyr::arrange(estimate) |>   # ascending here -> ggplot puts largest at top
  dplyr::pull(issue_id)

prep_panel <- function(df, kind) {
  df |>
    dplyr::mutate(
      issue_lbl = unname(ifelse(is.na(ISSUE_LABELS[issue_id]),
                                issue_id, ISSUE_LABELS[issue_id])),
      issue_f   = factor(issue_id, levels = issue_order),
      is_sig    = lower_ci > 0 | upper_ci < 0,
      kind      = kind
    )
}

panel_a <- prep_panel(panel_a_raw, "AI vs control")
panel_b <- prep_panel(panel_b_raw, "AI vs pooled humans")

# Pretty y-axis labels keyed on the shared factor ordering. Both panels
# use the same factor so the labels render identically; we set them on
# panel a only and blank them on panel b.
issue_labels_ordered <- ISSUE_LABELS[issue_order]

# Shared x-axis range across both panels so visual widths are comparable.
x_lim    <- c(min(c(panel_a$lower_ci, panel_b$lower_ci), na.rm = TRUE) - 1,
              max(c(panel_a$upper_ci, panel_b$upper_ci), na.rm = TRUE) + 1)
x_breaks <- seq(0, 20, by = 5)

make_panel <- function(df, xlab, show_y_labels) {
  p <- ggplot(df, aes(x = estimate, y = issue_f)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = REF_LINE_GREY, linewidth = 0.3) +
    geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci),
                  width = 0, linewidth = 0.5, colour = HUMAN_GREY,
                  orientation = "y") +
    geom_point(aes(colour = is_sig), size = 2) +
    scale_x_continuous(breaks = x_breaks, limits = x_lim,
                       expand = c(0.01, 0.01)) +
    scale_colour_manual(values = c("TRUE" = AI_RED, "FALSE" = HUMAN_GREY),
                        guide = "none") +
    labs(x = xlab, y = NULL) +
    theme_persuasion4() +
    theme(
      panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3),
      axis.title.x       = element_text(size = FIG_TITLE_PT, face = "bold",
                                        hjust = 0.5,
                                        margin = margin(t = 6)),
      plot.margin        = margin(t = 5, r = 6, b = 3, l = 5)
    )
  if (show_y_labels) {
    p + scale_y_discrete(labels = issue_labels_ordered) +
      theme(axis.text.y = element_text(size = FIG_BODY_PT, hjust = 1,
                                       margin = margin(r = 2)))
  } else {
    p + scale_y_discrete(labels = NULL) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  }
}

p_a <- make_panel(panel_a, "AI vs control (pp)",        show_y_labels = TRUE)
p_b <- make_panel(panel_b, "AI vs pooled humans (pp)",  show_y_labels = FALSE)

combined <- (p_a | p_b) +
  plot_layout(widths = c(1.4, 1)) +
  plot_annotation(tag_levels = "a", theme = tag_theme())

save_figure(combined, "fig_issue_level", FIGURES_SI,
            width = 183, height = 110, units = "mm")
message("  done.")
