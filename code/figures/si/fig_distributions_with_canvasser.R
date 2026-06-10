# fig_distributions_with_canvasser.R
# SI Figure: per-class implied-effect distributions including
# Professional Canvassers. Visual analogue of main-text Figure 2 Panel
# B, except canvassers are added back into the panel. Used to back the
# robustness claim that AI's class-level advantage holds when the most
# real-world human persuader class is included.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/figures/theme.R"))

message("\n==== code/figures/si/fig_distributions_with_canvasser.R ====")

# =============================================================================
# Load data (unfiltered: all five human classes including canvassers)
# =============================================================================
panel_dists         <- load_rds("canvasser_class_distributions")
ai_unconstrained_te <- load_rds("canvasser_ai_reference")

x_range <- seq(-5, 20, length.out = 300)

panel_order <- panel_dists |>
  dplyr::arrange(group_te) |>
  dplyr::pull(display_label)

density_data <- panel_dists |>
  dplyr::filter(!is.na(group_te), !is.na(model_estimated_sd)) |>
  purrr::pmap_dfr(function(display_label, group_te, model_estimated_sd, ...) {
    tibble::tibble(
      x             = x_range,
      density       = dnorm(x_range, mean = group_te, sd = model_estimated_sd),
      display_label = display_label
    )
  }) |>
  dplyr::mutate(display_label = factor(display_label, levels = panel_order))

# Label positions: same logic as the main-text figure, but with an
# explicit position for Professional Canvasser (the new class). Place
# its label below its peak, mirroring the layout of Elite Layperson
# (which sits just below in pp terms).
peaks_df <- panel_dists |>
  dplyr::filter(!is.na(group_te), !is.na(model_estimated_sd)) |>
  dplyr::mutate(
    peak_x = group_te,
    peak_y = 1 / (model_estimated_sd * sqrt(2 * pi)),
    label_x = dplyr::case_when(
      display_label == "Random Layperson"       ~ 2.5,
      display_label == "Elite Layperson"        ~ 5.4,
      display_label == "Professional Canvasser" ~ 13.0,
      display_label == "Elite Debater"          ~ peak_x,
      display_label == "Coached Elite Debater"  ~ 11.5,
      TRUE                                       ~ peak_x
    ),
    label_y = dplyr::case_when(
      display_label == "Random Layperson"       ~ peak_y + 0.030,
      display_label == "Elite Layperson"        ~ 0.36,
      display_label == "Professional Canvasser" ~ 0.20,
      display_label == "Elite Debater"          ~ peak_y + 0.045,
      display_label == "Coached Elite Debater"  ~ peak_y + 0.040,
      TRUE                                       ~ peak_y
    ),
    label_text = dplyr::case_when(
      display_label == "Random Layperson"       ~ "Random\nLayperson",
      display_label == "Elite Layperson"        ~ "Selected\nLayperson",
      display_label == "Professional Canvasser" ~ "Professional\nCanvasser",
      display_label == "Coached Elite Debater"  ~ "Coached\nElite Debater",
      display_label == "Elite Debater"          ~ "Elite Debater",
      TRUE                                       ~ as.character(display_label)
    ),
    needs_leader = !(display_label %in% c("Elite Debater"))
  )

bisector_df <- peaks_df |> dplyr::select(peak_x, peak_y)

# X-axis breaks: same two-stage thinning as in fig2_limits.R so
# crowded class means stay readable.
class_means_for_ticks <- peaks_df |>
  dplyr::arrange(peak_x) |>
  dplyr::pull(peak_x)

break_records <- dplyr::bind_rows(
  tibble::tibble(pos = 0,
                 label = "0",
                 color = "gray20", face = "plain"),
  tibble::tibble(pos = class_means_for_ticks,
                 label = sprintf("%.1f", class_means_for_ticks),
                 color = "gray45", face = "plain"),
  tibble::tibble(pos = ai_unconstrained_te,
                 label = sprintf("%.1f", ai_unconstrained_te),
                 color = AI_RED, face = "bold"),
  tibble::tibble(pos = 15,
                 label = "15",
                 color = "gray20", face = "plain")
) |>
  dplyr::arrange(pos)

n <- nrow(break_records)
keep_label <- rep(TRUE, n)
if (n > 1L) {
  for (i in seq_len(n - 1L)) {
    if (keep_label[i] && abs(break_records$pos[i + 1L] - break_records$pos[i]) < 0.20) {
      if (break_records$color[i] == AI_RED) {
        keep_label[i + 1L] <- FALSE
      } else {
        keep_label[i] <- FALSE
      }
    }
  }
}
cluster_window <- 1.0
is_class_tick  <- break_records$color != AI_RED & break_records$pos > 0 &
                  break_records$pos < 15
class_idx <- which(is_class_tick & keep_label)
if (length(class_idx) >= 3L) {
  positions <- break_records$pos[class_idx]
  start <- 1L
  while (start <= length(class_idx)) {
    end <- start
    while (end < length(class_idx) &&
           (positions[end + 1L] - positions[start]) < cluster_window) {
      end <- end + 1L
    }
    if (end - start >= 2L) {
      interior <- class_idx[(start + 1L):(end - 1L)]
      keep_label[interior] <- FALSE
    }
    start <- end + 1L
  }
}
x_breaks      <- break_records$pos
x_labels      <- ifelse(keep_label, break_records$label, "")
x_tick_colors <- break_records$color
x_tick_faces  <- break_records$face

min_visual_gap <- 0.7
nudges <- rep(0, length(x_breaks))
class_tick_idx <- which(x_tick_colors != AI_RED &
                        x_breaks > 0 & x_breaks < 15 &
                        keep_label)
if (length(class_tick_idx) >= 2L) {
  for (i in seq_len(length(class_tick_idx) - 1L)) {
    lo <- class_tick_idx[i]
    hi <- class_tick_idx[i + 1L]
    gap <- x_breaks[hi] - x_breaks[lo]
    if (gap < min_visual_gap) {
      shift <- (min_visual_gap - gap) / 2
      nudges[lo] <- nudges[lo] - shift
      nudges[hi] <- nudges[hi] + shift
    }
  }
}
x_breaks <- x_breaks + nudges

panel <- ggplot2::ggplot() +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                      color = REF_LINE_GREY, linewidth = 0.3) +
  ggplot2::geom_vline(
    xintercept = ai_unconstrained_te,
    color = AI_RED, linewidth = 0.8, linetype = "dashed", alpha = 0.85
  ) +
  ggplot2::geom_area(
    data = density_data,
    ggplot2::aes(x = x, y = density, group = display_label),
    fill = HUMAN_GREY, alpha = 0.10, color = NA, position = "identity"
  ) +
  ggplot2::geom_line(
    data = density_data,
    ggplot2::aes(x = x, y = density, group = display_label),
    color = HUMAN_GREY, linewidth = 0.35
  ) +
  ggplot2::scale_x_continuous(
    breaks = x_breaks, labels = x_labels, limits = c(-2, 18)
  ) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08)),
                              breaks = NULL) +
  ggplot2::geom_segment(
    data = bisector_df,
    ggplot2::aes(x = peak_x, xend = peak_x, y = 0, yend = peak_y),
    inherit.aes = FALSE,
    linetype = "dashed", color = "gray45", linewidth = 0.3
  ) +
  ggplot2::geom_segment(
    data = peaks_df |> dplyr::filter(needs_leader),
    ggplot2::aes(x = label_x, xend = peak_x, y = label_y, yend = peak_y),
    inherit.aes = FALSE,
    linetype = "dashed", color = "gray45", linewidth = 0.3
  ) +
  ggplot2::geom_label(
    data = peaks_df,
    ggplot2::aes(x = label_x, y = label_y, label = label_text),
    inherit.aes  = FALSE,
    parse        = FALSE,
    fontface     = "plain",
    family       = FIG_BASE_FAMILY,
    hjust = 0.5, vjust = 0.5,
    size         = FIG_BODY_MM, lineheight = 0.85,
    color        = "gray20",
    fill         = scales::alpha("white", 0.92),
    label.size   = 0,
    label.padding = ggplot2::unit(0.22, "lines")
  ) +
  ggplot2::geom_text(
    data = tibble::tibble(
      x     = ai_unconstrained_te + 0.25,
      y     = max(density_data$density, na.rm = TRUE) * 0.65,
      label = "AI"
    ),
    ggplot2::aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0, vjust = 0.5,
    color = AI_RED, fontface = "bold",
    family = FIG_BASE_FAMILY, size = FIG_BODY_MM
  ) +
  ggplot2::labs(x = "Estimated Persuasive Impact (pp)", y = "Density") +
  theme_persuasion4() +
  ggplot2::theme(
    axis.title.y       = ggplot2::element_text(size = FIG_TITLE_PT, face = "bold",
                                               angle = 90, vjust = 0.5,
                                               margin = ggplot2::margin(r = 0)),
    axis.text.x        = axis_text_per_tick(
      colors = x_tick_colors,
      faces  = x_tick_faces
    ),
    axis.text.y        = ggplot2::element_blank(),
    axis.ticks.y       = ggplot2::element_blank(),
    axis.line.x        = ggplot2::element_blank(),
    plot.margin        = ggplot2::margin(t = 18, r = 5, b = 3, l = 5)
  ) +
  ggplot2::coord_cartesian(clip = "off")

save_figure(panel, "fig_distributions_with_canvasser", FIGURES_SI,
            width = 183, height = 80)

message("  done.")
