# fig_accurate_claims_vs_impact.R
# SI Figure: accurate-claims analogue of main-text Fig. 3b. Three panels over
# the same 13 Studies 1-2 conditions, each plotting the arm's persuasive-impact
# estimate against the mean number of (a) all, (b) accurate and (c) inaccurate
# claims per fact-checked conversation (log scale; accurate = veracity > 50).
# Dashed line = OLS on log10(mean claims); R^2 from claim_accuracy_r2.
# Reads RDS written by code/analysis/si_14_claim_accuracy.R.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/figures/theme.R"))

suppressPackageStartupMessages({
  library(patchwork)
})

message("\n==== code/figures/si/fig_accurate_claims_vs_impact.R ====")

panel_df <- load_rds("claim_accuracy_by_condition")
cond_r2  <- load_rds("claim_accuracy_r2")

color_mapping <- c(
  "Human"            = HUMAN_GREY,
  "AI (Constrained)" = AI_CONSTRAINED_SALMON,
  "AI"               = AI_RED
)

r2_label_args <- list(
  inherit.aes   = FALSE,
  parse         = TRUE,
  hjust         = 0, vjust = 1,
  size          = FIG_LEGEND_MM,
  family        = FIG_BASE_FAMILY,
  colour        = "#333333",
  fontface      = "bold",
  fill          = "white",
  linewidth     = 0,
  label.padding = ggplot2::unit(0.10, "lines"),
  label.r       = ggplot2::unit(0, "lines"),
  show.legend   = FALSE
)

claims_panel <- function(predictor, x_title, show_y = FALSE) {
  r2 <- cond_r2$r_squared[cond_r2$predictor == predictor]
  r2_row <- tibble::tibble(
    x = min(panel_df[[predictor]]) * 1.02, y = 17.8,
    label = sprintf("italic(R)^2*' = %.2f'", r2)
  )

  p <- ggplot2::ggplot(panel_df,
                       ggplot2::aes(x = .data[[predictor]], y = estimate)) +
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
      guide  = ggplot2::guide_legend(order = 2, nrow = 1)
    ) +
    ggplot2::scale_shape_manual(
      values = study_shapes, name = NULL,
      guide  = ggplot2::guide_legend(order = 1, nrow = 1)
    ) +
    ggplot2::scale_x_log10(
      breaks = c(0.5, 1, 2, 5, 10, 20, 50),
      labels = function(x) format(x, drop0trailing = TRUE, trim = TRUE),
      expand = ggplot2::expansion(mult = c(0.07, 0.07))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(-1, 18.5),
      breaks = seq(0, 15, 5),
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    do.call(ggplot2::geom_label, c(list(
      data    = r2_row,
      mapping = ggplot2::aes(x = x, y = y, label = label)
    ), r2_label_args)) +
    ggplot2::labs(
      x = x_title,
      y = if (show_y) "Estimated\npersuasive impact (pp)" else NULL
    ) +
    theme_persuasion4() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = FIG_TITLE_PT, face = "bold",
                                           lineheight = 0.95,
                                           margin = ggplot2::margin(t = 5)),
      axis.title.y = ggplot2::element_text(size = FIG_TITLE_PT, face = "bold",
                                           lineheight = 0.9,
                                           angle = 90, vjust = 0.5),
      panel.grid.major.x = ggplot2::element_line(colour = "grey92",
                                                 linewidth = 0.3),
      panel.grid.major.y = ggplot2::element_line(colour = "grey92",
                                                 linewidth = 0.3),
      plot.margin = ggplot2::margin(t = 16, r = 5, b = 3, l = 5)
    )
}

panel_all   <- claims_panel("claims", "Claims per conversation\n(log scale)",
                            show_y = TRUE)
panel_acc   <- claims_panel("accurate", "Accurate claims per\nconversation (log scale)")
panel_inacc <- claims_panel("inaccurate", "Inaccurate claims per\nconversation (log scale)")

combined <- (panel_all | panel_acc | panel_inacc) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(
    tag_levels = "a",
    theme      = ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )
  ) &
  tag_theme() &
  ggplot2::theme(
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.box        = "horizontal",
    legend.margin     = ggplot2::margin(t = 0, b = 0),
    legend.key.height = ggplot2::unit(0.55, "lines"),
    legend.key.width  = ggplot2::unit(0.60, "lines"),
    legend.text       = ggplot2::element_text(size = FIG_LEGEND_PT)
  )

save_figure(combined, "fig_accurate_claims_vs_impact", FIGURES_SI,
            width = 183, height = 75)

message("  done.")
