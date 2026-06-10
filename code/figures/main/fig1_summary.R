# fig1_summary.R
# Main-text Figure 1 (summary_figure.{pdf,png}).
#
# The summary forest referenced at the end of the introduction. One
# horizontal axis with seven x-ticks:
#   Random Layperson, Selected Layperson, Professional Canvasser,
#   Elite Debater, Coached Elite Debater, AI, AI (Constrained).
#
# AI shows three jittered per-study estimates (S1 / S2 / S3) under the
# same x-tick; AI (Constrained) shows a single Study 2 estimate.
# Per-model stencils (small black markers) overlay both AI columns.
#
# Single panel; no implied-distribution density panel (that lives in
# Figure 2 now). All data is loaded from RDS files written by
# code/analysis/01_main_attitudes.R; this script does NOT fit any models.
#
# Aesthetics inherit from code/figures/theme.R. Vectorised per-tick
# colouring + the in-panel legend block follow the same conventions as
# the section forest panels so the summary figure feels visually
# consistent with the figures it previews.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/figures/theme.R"))

suppressPackageStartupMessages({
  library(patchwork)
})

message("\n==== code/figures/main/fig1_summary.R ====")

combined_shape_values <- c(study_shapes, ai_version_shapes)

# =============================================================================
# Load data
# =============================================================================
panel_effects <- load_rds("summary_class_effects")
panel_overlays <- load_rds("summary_ai_overlays")

# =============================================================================
# Display-group ordering
# =============================================================================
group_order_display <- c(
  "Random\nLayperson",     "Selected\nLayperson",
  "Professional\nCanvasser",
  "Elite\nDebater",        "Coached\nElite Debater",
  "AI\n(Constrained)",     "AI"
)

panel_df <- panel_effects |>
  dplyr::mutate(
    study = dplyr::case_when(
      group == "Random Layperson"        ~ "Study 1",
      group == "Elite Layperson"         ~ "Study 1",
      group == "Elite Debater"           ~ "Study 1",
      group == "Coached Elite Debater"   ~ "Study 2",
      group == "Professional Canvasser"  ~ "Study 3",
      group == "AI (Info-prompted) S1"   ~ "Study 1",
      group == "AI (Info-prompted) S2"   ~ "Study 2",
      group == "AI (Info-prompted) S3"   ~ "Study 3",
      group == "AI (Constrained)"        ~ "Study 2",
      TRUE                                ~ NA_character_
    ),
    display_group = dplyr::case_when(
      group == "Random Layperson"        ~ "Random\nLayperson",
      group == "Elite Layperson"         ~ "Selected\nLayperson",
      group == "Elite Debater"           ~ "Elite\nDebater",
      group == "Coached Elite Debater"   ~ "Coached\nElite Debater",
      group == "Professional Canvasser"  ~ "Professional\nCanvasser",
      grepl("^AI \\(Info-prompted\\)", group) ~ "AI",
      group == "AI (Constrained)"        ~ "AI\n(Constrained)",
      TRUE                                ~ NA_character_
    ),
    color_group = dplyr::case_when(
      display_group == "AI"               ~ "AI",
      display_group == "AI\n(Constrained)" ~ "AI (Constrained)",
      TRUE                                 ~ "Human"
    )
  ) |>
  dplyr::filter(!is.na(display_group)) |>
  dplyr::mutate(display_group = factor(display_group,
                                       levels = group_order_display))

# Numeric x for each category; the AI column gets a per-study jitter
# so the three study points sit side-by-side under the same x-tick.
panel_df <- panel_df |>
  dplyr::mutate(
    x_numeric = as.numeric(display_group),
    x_pos = dplyr::case_when(
      display_group == "AI" & study == "Study 1" ~ x_numeric - 0.32,
      display_group == "AI" & study == "Study 2" ~ x_numeric,
      display_group == "AI" & study == "Study 3" ~ x_numeric + 0.32,
      TRUE ~ x_numeric
    )
  )

# AI per-model overlays. Both "(Info) S?" and "(Constrained)" labels.
ai_overlay_df <- panel_overlays |>
  dplyr::mutate(
    is_constrained = grepl("\\(Constrained\\)$", group),
    study = dplyr::case_when(
      is_constrained                      ~ "Study 2",
      grepl(" \\(Info\\) S1$", group)    ~ "Study 1",
      grepl(" \\(Info\\) S2$", group)    ~ "Study 2",
      grepl(" \\(Info\\) S3$", group)    ~ "Study 3",
      TRUE                                ~ NA_character_
    ),
    parent_display = dplyr::if_else(is_constrained,
                                    "AI\n(Constrained)",
                                    "AI"),
    parent_group   = factor(parent_display, levels = group_order_display),
    model_version_label = dplyr::if_else(
      is_constrained,
      sub(" \\(Constrained\\)$", "", group),
      sub(" \\(Info\\) S[123]$", "", group)
    )
  ) |>
  dplyr::filter(model_version_label %in% names(ai_version_shapes)) |>
  dplyr::mutate(
    x_numeric    = as.numeric(parent_group),
    parent_offset = dplyr::case_when(
      is_constrained                ~ 0.00,
      study == "Study 1"            ~ -0.32,
      study == "Study 2"            ~  0.00,
      study == "Study 3"            ~  0.32,
      TRUE                          ~  0.00
    ),
    x_pos = x_numeric + parent_offset + 0.16
  )

# Truncated vertical guides: one short grey segment per x-axis tick,
# stopping at that tick's max estimate so guides don't span the full
# panel height like default grid lines would.
panel_vguides <- panel_df |>
  dplyr::group_by(display_group) |>
  dplyr::summarise(
    x_center = dplyr::first(as.numeric(display_group)),
    y_top    = max(estimate, na.rm = TRUE),
    .groups  = "drop"
  )

ai_labels_present <- intersect(names(ai_version_shapes),
                               unique(ai_overlay_df$model_version_label))

panel_summary <- ggplot2::ggplot() +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                      color = REF_LINE_GREY, linewidth = 0.3) +
  ggplot2::geom_segment(
    data = panel_vguides,
    ggplot2::aes(x = x_center, xend = x_center, y = 0, yend = y_top),
    color = GUIDE_GREY, linewidth = 0.3
  ) +
  ggplot2::geom_pointrange(
    data = panel_df,
    ggplot2::aes(x = x_pos, y = estimate, ymin = lower_ci, ymax = upper_ci,
                 color = color_group, shape = study),
    size = 0.9, linewidth = 0.45
  ) +
  ggplot2::geom_point(
    data = ai_overlay_df,
    ggplot2::aes(x = x_pos, y = estimate, shape = model_version_label,
                 color = dplyr::if_else(is_constrained,
                                        "AI (Constrained)", "AI")),
    size = 1.8, stroke = 0.5
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq_along(group_order_display),
    labels = group_order_display,
    expand = ggplot2::expansion(add = c(0.5, 0.55))
  ) +
  ggplot2::scale_color_manual(
    values = c("Human"            = HUMAN_GREY,
               "AI"               = AI_RED,
               "AI (Constrained)" = AI_CONSTRAINED_SALMON),
    guide  = "none"
  ) +
  ggplot2::scale_shape_manual(values = combined_shape_values, guide = "none") +
  ggplot2::labs(y = "Estimated persuasive impact (pp)", x = "") +
  theme_persuasion4() +
  ggplot2::theme(
    axis.text.x  = axis_text_per_tick(
      colors = c(rep("gray20", length(group_order_display) - 2L),
                 AI_CONSTRAINED_SALMON, AI_RED),
      faces  = c(rep("plain",  length(group_order_display) - 2L),
                 "bold", "bold")
    ),
    axis.title.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(
      color = "gray85", linewidth = 0.25
    ),
    plot.margin  = ggplot2::margin(t = 6, r = 5, b = 3, l = 5)
  )

# In-plot legend block in the lower-right corner of the plot area
# (under the AI columns, where the panel is empty because no human
# class lives this far right and AI's CIs sit above ~12 pp). We
# pre-declare which AI per-model stencils actually appear in the data
# so the legend stays compact.
legend_rows <- c("Study 1", "Study 2", "Study 3", ai_labels_present)
n_rows      <- length(legend_rows)
legend_y_top   <- 8.0
legend_y_step  <- 0.72
legend_x_point <- length(group_order_display) - 0.40
legend_x_text  <- legend_x_point + 0.14
legend_xmin    <- legend_x_point - 0.10
legend_xmax    <- legend_x_text + 1.20
legend_ymax    <- legend_y_top + 0.50
legend_ymin    <- legend_y_top - (n_rows - 1) * legend_y_step - 0.50

panel_summary <- panel_summary +
  ggplot2::annotate("rect",
                    xmin = legend_xmin, xmax = legend_xmax,
                    ymin = legend_ymin, ymax = legend_ymax,
                    fill = scales::alpha("white", 0.92),
                    color = "gray80", linewidth = 0.25)

for (i in seq_along(legend_rows)) {
  lbl <- legend_rows[[i]]
  y   <- legend_y_top - (i - 1) * legend_y_step
  if (lbl %in% names(study_shapes)) {
    sh    <- study_shapes[[lbl]]
    col   <- "black"
    psize <- 2.0
  } else {
    sh    <- ai_version_shapes[[lbl]]
    col   <- AI_RED_DK
    psize <- 1.8
  }
  panel_summary <- panel_summary +
    ggplot2::annotate("point", x = legend_x_point, y = y,
                      shape = sh, size = psize, color = col, stroke = 0.5) +
    ggplot2::annotate("text",  x = legend_x_text,  y = y, label = lbl,
                      hjust = 0, size = FIG_LEGEND_MM,
                      family = FIG_BASE_FAMILY, lineheight = 1.1)
}

panel_summary <- panel_summary + ggplot2::coord_cartesian(clip = "off")

# =============================================================================
# Save
# =============================================================================
# Single panel, sized just shy of the former two-panel height
# (the density panel that lived underneath now sits in Figure 2).
save_figure(panel_summary,
            name   = "summary_figure",
            dir    = FIGURES_MAIN,
            height = 95)

message("  done.")
