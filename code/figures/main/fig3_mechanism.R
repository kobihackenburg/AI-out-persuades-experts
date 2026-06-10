# fig3_mechanism.R
# Main-text Figure 3 (mechanism_figure.{pdf,png}).
#
# The "mechanism" figure for the manuscript section that asks
# "Why does AI out-persuade expert humans?". Two panels, side-by-side, both
# fit strictly on the pooled Studies 1+2 frame (no Study 3 anywhere --
# no canvassers, no S3 AI models):
#
#   a: per-item forest of the effect of constraining AI on partner
#      ratings (selectively suppresses the two informational items:
#      argument strength and learning).
#   b: cross-condition scatter of fact-density vs persuasive impact,
#      pooled across human and AI conditions in Studies 1 and 2. The
#      SI canvasser-inclusion figure in
#      code/figures/si/fig_facts_with_canvasser.R rebuilds the same
#      panel from the canvasser-inclusive S1-S3 fit.
#
# Both panels probe the *throughput / information-delivery* account of
# why AI out-persuades expert humans. The intervention forest (AI vs
# Constrained AI vs Coached Elite Debater vs Elite Debater) that
# establishes the "constraining AI closes the gap" result lives on the
# separate limits figure
# (code/figures/main/fig2_limits.R), together with the per-class
# implied-effect distributions; the mechanism prose refers back to it.
#
# All aesthetics come from code/figures/theme.R. No models are fit.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/figures/theme.R"))

suppressPackageStartupMessages({
  library(patchwork)
})

message("\n==== code/figures/main/fig3_mechanism.R ====")

# =============================================================================
# Load data (both panels strictly S1+S2; no Study 3 inputs)
# =============================================================================
panel_a_df  <- load_rds("mechanism_constraint_effects")
panel_b_df  <- load_rds("mechanism_facts_vs_te")
panel_b_reg <- load_rds("mechanism_facts_regression")

# =============================================================================
# Panel A: per-item effect of constraining AI on partner ratings
# =============================================================================
panel_a_dim_labels <- c(
  rating_learning  = "I learned a lot",
  rating_arguments = "Made strong arguments",
  rating_empathy   = "I felt understood",
  rating_enjoyment = "It was enjoyable",
  rating_anthropo  = "Interacted just like a human",
  rating_deception = "Lied to me / made things up",
  rating_bias      = "Was biased"
)

panel_a_plot <- panel_a_df |>
  dplyr::filter(contrast_kind == "Constrained - AI") |>
  dplyr::arrange(dplyr::desc(estimate)) |>
  dplyr::mutate(
    item  = factor(item, levels = item),
    label = sprintf("%+.1f", estimate)
  )

panel_a_xmin <- floor(min(panel_a_plot$lower_ci) - 2)
panel_a_xmax <- ceiling(max(panel_a_plot$upper_ci) + 2)

panel_a <- ggplot2::ggplot(panel_a_plot,
                           ggplot2::aes(x = estimate, y = item)) +
  ggplot2::geom_segment(
    ggplot2::aes(x = panel_a_xmin, xend = estimate,
                 y = item, yend = item),
    color = GUIDE_GREY, linewidth = 0.3
  ) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                      color = "gray35", linewidth = 0.45) +
  ggplot2::geom_pointrange(
    ggplot2::aes(xmin = lower_ci, xmax = upper_ci),
    color = AI_CONSTRAINED_SALMON,
    shape = study_shapes[["Study 2"]],
    size = 0.4, linewidth = 0.55
  ) +
  ggplot2::geom_label(
    ggplot2::aes(label = label),
    nudge_y = 0.22,
    hjust = 0.5, vjust = 0,
    fontface = "bold",
    color = AI_CONSTRAINED_SALMON,
    fill = "white",
    linewidth = 0,
    label.padding = ggplot2::unit(0.10, "lines"),
    size = FIG_BODY_MM, family = FIG_BASE_FAMILY
  ) +
  ggplot2::scale_y_discrete(labels = panel_a_dim_labels,
                            expand = ggplot2::expansion(add = c(0.3, 0.85))) +
  ggplot2::scale_x_continuous(
    limits = c(panel_a_xmin, panel_a_xmax),
    expand = ggplot2::expansion(mult = c(0.02, 0.02))
  ) +
  ggplot2::labs(x = "Effect of constraining AI (pp)", y = NULL) +
  ggplot2::coord_cartesian(clip = "off") +
  theme_persuasion4() +
  ggplot2::theme(
    axis.text.y  = ggplot2::element_text(size = FIG_BODY_PT, color = "black"),
    axis.title.x = ggplot2::element_text(size = FIG_TITLE_PT, face = "bold",
                                         lineheight = 0.95,
                                         margin = ggplot2::margin(t = 5)),
    plot.margin = ggplot2::margin(t = 16, r = 5, b = 3, l = 5)
  )

# =============================================================================
# Panel B: facts vs persuasive impact scatter (canvassers omitted)
# =============================================================================
color_mapping_b <- c(
  "Human"            = HUMAN_GREY,
  "AI (Constrained)" = AI_CONSTRAINED_SALMON,
  "AI"               = AI_RED
)

r2 <- function(label) {
  v <- panel_b_reg |> dplyr::filter(subset == label) |> dplyr::pull(r_squared)
  if (length(v) == 0L) NA_real_ else v
}
r2_overall <- r2("Overall")
r2_humans  <- r2("Humans only")
r2_ai      <- r2("AI only")

r2_anchor_x <- min(panel_b_df$mean_fact_claims) * 1.02
r2_anchor_y <- 17.8
r2_line_dy  <- 1.4
r2_lbl_overall <- sprintf("'Overall: '*italic(R)^2*' = %.2f'",   r2_overall)
r2_lbl_humans  <- sprintf("'Humans only: '*italic(R)^2*' = %.2f'", r2_humans)
r2_lbl_ai      <- sprintf("'AI only: '*italic(R)^2*' = %.2f'",     r2_ai)

r2_row <- function(y, label) tibble::tibble(x = r2_anchor_x, y = y, label = label)
r2_row_overall <- r2_row(r2_anchor_y,                  r2_lbl_overall)
r2_row_humans  <- r2_row(r2_anchor_y - r2_line_dy,     r2_lbl_humans)
r2_row_ai      <- r2_row(r2_anchor_y - 2 * r2_line_dy, r2_lbl_ai)

r2_label_args <- list(
  inherit.aes   = FALSE,
  parse         = TRUE,
  hjust         = 0, vjust = 1,
  size          = FIG_LEGEND_MM,
  family        = FIG_BASE_FAMILY,
  fill          = "white",
  linewidth     = 0,
  label.padding = ggplot2::unit(0.10, "lines"),
  label.r       = ggplot2::unit(0, "lines"),
  show.legend   = FALSE
)

panel_b <- ggplot2::ggplot(panel_b_df,
                           ggplot2::aes(x = mean_fact_claims, y = estimate)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                      color = "gray70", linewidth = 0.3) +
  ggplot2::geom_smooth(
    method = "lm", formula = y ~ x,
    se = TRUE,
    color = "#333333", linetype = "dashed", linewidth = 0.5,
    fill = "gray70", alpha = 0.18
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = lower_ci, ymax = upper_ci, color = type),
    width = 0, linewidth = 0.45, alpha = 0.7
  ) +
  ggplot2::geom_point(
    ggplot2::aes(color = type, shape = study),
    size = 2.4, stroke = 0.5
  ) +
  ggplot2::scale_color_manual(
    values = color_mapping_b, name = NULL,
    breaks = c("Human", "AI (Constrained)", "AI"),
    guide  = ggplot2::guide_legend(order = 2, ncol = 1)
  ) +
  ggplot2::scale_shape_manual(
    values = study_shapes, name = NULL,
    guide  = ggplot2::guide_legend(order = 1, ncol = 1)
  ) +
  ggplot2::scale_x_log10(
    breaks = c(2, 5, 10, 20, 50),
    expand = ggplot2::expansion(mult = c(0.07, 0.07))
  ) +
  ggplot2::scale_y_continuous(
    limits = c(-1, 18.5),
    breaks = seq(0, 15, 5),
    expand = ggplot2::expansion(mult = c(0.02, 0.02))
  ) +
  do.call(ggplot2::geom_label, c(list(
    data    = r2_row_overall,
    mapping = ggplot2::aes(x = x, y = y, label = label),
    colour  = "#333333", fontface = "bold"
  ), r2_label_args)) +
  do.call(ggplot2::geom_label, c(list(
    data    = r2_row_humans,
    mapping = ggplot2::aes(x = x, y = y, label = label),
    colour  = HUMAN_GREY
  ), r2_label_args)) +
  do.call(ggplot2::geom_label, c(list(
    data    = r2_row_ai,
    mapping = ggplot2::aes(x = x, y = y, label = label),
    colour  = AI_RED
  ), r2_label_args)) +
  ggplot2::labs(
    x = "Facts per conversation (log scale)",
    # Two-line title prevents the rotated text from extending into the
    # top-left corner where the patchwork panel tag ("b") is rendered.
    y = "Estimated\npersuasive impact (pp)"
  ) +
  theme_persuasion4() +
  ggplot2::theme(
    axis.title.y = ggplot2::element_text(size = FIG_TITLE_PT, face = "bold",
                                         lineheight = 0.9,
                                         angle = 90, vjust = 0.5),
    panel.grid.major.x = ggplot2::element_line(colour = "grey92",
                                               linewidth = 0.3),
    panel.grid.major.y = ggplot2::element_line(colour = "grey92",
                                               linewidth = 0.3),
    legend.position        = "inside",
    legend.position.inside = c(0.98, 0.04),
    legend.justification   = c(1, 0),
    legend.direction       = "vertical",
    legend.box             = "vertical",
    legend.spacing.y       = ggplot2::unit(0, "lines"),
    legend.background      = ggplot2::element_blank(),
    legend.box.background  = ggplot2::element_rect(
      fill = scales::alpha("white", 0.92),
      color = "gray80", linewidth = 0.25
    ),
    legend.box.margin = ggplot2::margin(t = 1, r = 3, b = 1, l = 3),
    legend.key.height = ggplot2::unit(0.55, "lines"),
    legend.key.width  = ggplot2::unit(0.60, "lines"),
    legend.text       = ggplot2::element_text(size = FIG_LEGEND_PT - 1L),
    legend.title      = ggplot2::element_text(size = FIG_LEGEND_PT - 1L,
                                              face = "bold"),
    plot.margin = ggplot2::margin(t = 16, r = 5, b = 3, l = 5)
  )

# =============================================================================
# Combine + save
# =============================================================================
# Two side-by-side panels in a single row: partner-ratings forest (A)
# on the left, facts scatter (B) on the right. 50/50 widths.
panel_a_block <- patchwork::wrap_elements(full = panel_a)
panel_b_block <- patchwork::wrap_elements(full = panel_b)

combined <- (panel_a_block | panel_b_block) +
  patchwork::plot_layout(widths = c(1.0, 1.0)) +
  patchwork::plot_annotation(
    tag_levels = "a",
    theme      = ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.tag        = ggplot2::element_text(size = FIG_TAG_PT,
                                              face = "bold",
                                              family = FIG_BASE_FAMILY)
    )
  ) & tag_theme()

# Two-column page; ~85 mm keeps both side-by-side panels readable while
# leaving room on the manuscript page for the separate limits figure
# above and a (short) two-panel caption below.
save_figure(combined, "mechanism_figure", FIGURES_MAIN, height = 85)

message("  done.")
