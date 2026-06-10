# fig_facts_with_canvasser.R
# SI Figure: facts-per-conversation vs persuasive impact scatter
# including Professional Canvassers. Visual analogue of main-text
# Figure 3 Panel B, except canvassers are added back into the panel
# (and the R^2 annotations come from the with-canvasser regression
# fit in 02_mechanism.R as canvasser_facts_regression.rds).

source(here::here("code/analysis/_setup.R"))
source(here::here("code/figures/theme.R"))

message("\n==== code/figures/si/fig_facts_with_canvasser.R ====")

# =============================================================================
# Load data (unfiltered: includes Professional Canvasser row)
# =============================================================================
panel_df  <- load_rds("canvasser_facts_vs_te")
panel_reg <- load_rds("canvasser_facts_regression")

color_mapping <- c(
  "Human"            = HUMAN_GREY,
  "AI (Constrained)" = AI_CONSTRAINED_SALMON,
  "AI"               = AI_RED
)

r2 <- function(label) {
  v <- panel_reg |> dplyr::filter(subset == label) |> dplyr::pull(r_squared)
  if (length(v) == 0L) NA_real_ else v
}
r2_overall <- r2("Overall")
r2_humans  <- r2("Humans only")
r2_ai      <- r2("AI only")

r2_anchor_x <- min(panel_df$mean_fact_claims) * 1.02
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

panel <- ggplot2::ggplot(panel_df,
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
    values = color_mapping, name = NULL,
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
    y = "Estimated persuasive impact (pp)"
  ) +
  theme_persuasion4() +
  ggplot2::theme(
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
    plot.margin = ggplot2::margin(t = 5, r = 5, b = 3, l = 5)
  )

save_figure(panel, "fig_facts_with_canvasser", FIGURES_SI,
            width = 130, height = 95)

message("  done.")
