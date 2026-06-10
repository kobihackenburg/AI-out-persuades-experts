# fig2_limits.R
# Main-text Figure 2 (limits_figure.{pdf,png}).
#
# The "limits" figure for the manuscript section that asks
# "Are there cases where AI can't beat humans?". Two panels, stacked
# top-to-bottom, both fit strictly on the pooled Studies 1+2 frame
# (no Study 3 anywhere -- no canvassers, no S3 AI models):
#
#   a: Cochrane-style three-row forest of within-study contrasts vs AI
#      (Info-prompted): Elite Debater (S1, circle), Coached Elite Debater
#      (S2, triangle), AI (Constrained) (S2, triangle). All three bars
#      are signed AI - comparator (positive = AI advantage). Single AI
#      footer row at x = 0 (pooled S1+S2 in the inset, since AI's
#      words/delay are essentially identical across the two studies).
#      Left-hand text columns show avg. words/msg and response delay (s)
#      for each row. The panel shows that coaching narrows but does not
#      close the gap; the only condition that does is throttling AI's
#      throughput.
#   b: analytical normal-density curves of implied per-persuader
#      effects per class. 4 non-canvasser classes, mu and tau from the
#      S1+S2-only pooled fit (01_main_attitudes.R section 4b). The SI
#      figure code/figures/si/fig_distributions_with_canvasser.R shows
#      the same panel with canvassers added (5 classes; S1-S3 fit).
#      Dashed red vertical = pooled AI estimate (mean of AI S1 + AI S2
#      contrasts).
#
# The partner-ratings and facts-vs-persuasive-impact panels live in
# code/figures/main/fig3_mechanism.R and back the separate mechanism
# section.
#
# All aesthetics come from code/figures/theme.R. No models are fit.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/figures/theme.R"))

suppressPackageStartupMessages({
  library(patchwork)
})

message("\n==== code/figures/main/fig2_limits.R ====")

# =============================================================================
# Load data (both panels strictly S1+S2; no Study 3 inputs)
# =============================================================================
panel_a_contrasts   <- load_rds("limits_contrasts")
panel_a_inset       <- load_rds("limits_inset")
panel_b_dists       <- load_rds("limits_class_distributions")
ai_unconstrained_te <- load_rds("limits_ai_reference")

# =============================================================================
# Panel A: within-study contrasts vs AI (Info-prompted)
# =============================================================================
# Three treatment rows + one header band + one AI footer reference row.
# Row order top->bottom: Elite Debater (S1), Coached Elite Debater (S2),
# AI (Constrained) (S2), AI (reference, pooled S1+S2). AI is the
# within-study reference at x = 0; the contrast-row markers indicate
# the comparator's study (circle = S1, triangle = S2) so every plotted
# bar is a randomised within-study comparison.
panel_a_row_levels <- c("header_title",
                        "Elite Debater",
                        "Coached Elite Debater",
                        "AI (Constrained)",
                        "AI (reference)")

# Sign flip: bars are signed comparator - AI so they extend leftward
# from x = 0 (AI's within-study reference). The negated CI endpoints are
# held in separate columns first so the swap doesn't reference
# already-mutated values mid-transmute.
panel_a_plot <- panel_a_contrasts |>
  dplyr::mutate(
    .lo_new = -upper_ci,
    .hi_new = -lower_ci
  ) |>
  dplyr::transmute(
    row         = factor(display, levels = rev(panel_a_row_levels)),
    estimate    = -estimate,
    lower_ci    = .lo_new,
    upper_ci    = .hi_new,
    study       = study,
    color_group = display
  )

# x range is tuned (together with the panel A patchwork widths below
# and panel B's truncated x range of [-2, 15.5]) so the red AI dashed
# line at x = 0 falls at the same horizontal page position as panel B's
# red AI dashed line at x = 13.9. Panel B's red line sits at fraction
# 9/11 of its truncated plot panel; with panel A's text panel set to
# 50% of figure width and the forest panel to 50%, the same page
# position is reached by placing x = 0 at fraction ~0.78 of the
# forest's x range, i.e. x_min = -9 and x_max = 2.5.
x_min <- -9
x_max <-  2.5

# Y-band boundaries:
#   header_title           = y 5
#   Elite Debater (S1)     = y 4
#   Coached Elite Debater  = y 3
#   AI (Constrained)       = y 2
#   AI (reference)         = y 1
panel_a_forest <- ggplot2::ggplot(panel_a_plot,
                                  ggplot2::aes(x = estimate, y = row,
                                               color = color_group)) +
  ggplot2::geom_hline(yintercept = 4.5, linewidth = 0.45, color = RULE_GREY) +
  ggplot2::geom_hline(yintercept = 1.5, linewidth = 0.45, color = RULE_GREY) +
  ggplot2::annotate("segment", x = 0, xend = 0, y = 1.5, yend = 4.5,
                    linetype = "dashed", color = AI_RED,
                    linewidth = 0.8, alpha = 0.85) +
  ggplot2::annotate("text", x = 0.25, y = 3,
                    label = "AI", hjust = 0, vjust = 0.5,
                    color = AI_RED, fontface = "bold",
                    family = FIG_BASE_FAMILY, size = FIG_BODY_MM) +
  ggplot2::geom_pointrange(
    ggplot2::aes(xmin = lower_ci, xmax = upper_ci, shape = study),
    size = 0.9, linewidth = 0.45,
    show.legend = FALSE
  ) +
  ggplot2::geom_text(
    ggplot2::aes(x = lower_ci, label = sprintf("%+.1f", estimate)),
    hjust = 0, nudge_y = 0.22,
    size = FIG_BODY_MM, family = FIG_BASE_FAMILY,
    fontface = "bold",
    show.legend = FALSE
  ) +
  ggplot2::annotate("text", x = (x_min + x_max) / 2, y = 5,
                    label = "Estimated persuasive impact\nvs. AI (pp)",
                    fontface = "bold", lineheight = 0.9,
                    size = FIG_BODY_MM, family = FIG_BASE_FAMILY,
                    color = "black") +
  ggplot2::scale_color_manual(values = c(
    "Elite Debater"         = HUMAN_GREY,
    "Coached Elite Debater" = HUMAN_GREY,
    "AI (Constrained)"      = AI_CONSTRAINED_SALMON
  )) +
  ggplot2::scale_shape_manual(values = study_shapes) +
  ggplot2::scale_x_continuous(
    limits = c(x_min, x_max),
    expand = ggplot2::expansion(mult = c(0.02, 0.02))
  ) +
  ggplot2::scale_y_discrete(limits = rev(panel_a_row_levels), drop = FALSE) +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::coord_cartesian(clip = "off") +
  theme_persuasion4() +
  ggplot2::theme(
    axis.text.y  = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.text.x  = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    plot.margin  = ggplot2::margin(t = 20, r = 5, b = 3, l = 0)
  )

# -----------------------------------------------------------------------------
# Panel A text columns (Condition / words/msg / delay)
# -----------------------------------------------------------------------------
inset_lookup <- panel_a_inset |>
  dplyr::mutate(
    row = factor(
      dplyr::case_when(
        display == "AI" ~ "AI (reference)",
        TRUE            ~ display
      ),
      levels = rev(panel_a_row_levels)
    ),
    cond_label = dplyr::case_when(
      display == "AI" ~ "AI (reference)",
      TRUE            ~ display
    ),
    words = avg_words_lbl,
    delay = avg_delay_s,
    # Per-row text colour: AI footer in red (matches the dashed AI
    # reference line in the forest panel and the AI dashed line in
    # panel B), Constrained AI in salmon (matches its bar colour);
    # everything else uses the default RULE_GREY for numbers and
    # default black for the condition label.
    label_color = dplyr::case_when(
      display == "AI"               ~ AI_RED,
      display == "AI (Constrained)" ~ AI_CONSTRAINED_SALMON,
      TRUE                          ~ "black"
    ),
    number_color = dplyr::case_when(
      display == "AI"               ~ AI_RED,
      display == "AI (Constrained)" ~ AI_CONSTRAINED_SALMON,
      TRUE                          ~ RULE_GREY
    )
  ) |>
  dplyr::select(row, cond_label, words, delay, label_color, number_color)

panel_a_header <- tibble::tibble(
  row        = factor("header_title", levels = rev(panel_a_row_levels)),
  cond_label = "Condition",
  words      = "Avg. words/msg",
  delay      = "Avg. response\ndelay (s)"
)

panel_a_text <- ggplot2::ggplot() +
  ggplot2::geom_hline(yintercept = 4.5, linewidth = 0.45, color = RULE_GREY) +
  ggplot2::geom_hline(yintercept = 1.5, linewidth = 0.45, color = RULE_GREY) +
  ggplot2::geom_text(data = panel_a_header,
                     ggplot2::aes(x = 0.0, y = row, label = cond_label),
                     hjust = 0, fontface = "bold",
                     size = FIG_BODY_MM, family = FIG_BASE_FAMILY) +
  ggplot2::geom_text(data = panel_a_header,
                     ggplot2::aes(x = 3.0, y = row, label = words),
                     hjust = 0.5, fontface = "bold",
                     size = FIG_BODY_MM, family = FIG_BASE_FAMILY) +
  ggplot2::geom_text(data = panel_a_header,
                     ggplot2::aes(x = 4.6, y = row, label = delay),
                     hjust = 0.5, fontface = "bold", lineheight = 0.85,
                     size = FIG_BODY_MM, family = FIG_BASE_FAMILY) +
  ggplot2::geom_text(data = inset_lookup,
                     ggplot2::aes(x = 0.0, y = row, label = cond_label,
                                  color = label_color),
                     hjust = 0,
                     size = FIG_BODY_MM, family = FIG_BASE_FAMILY) +
  ggplot2::geom_text(data = inset_lookup,
                     ggplot2::aes(x = 3.0, y = row, label = words,
                                  color = number_color),
                     hjust = 0.5,
                     size = FIG_BODY_MM, family = FIG_BASE_FAMILY) +
  ggplot2::geom_text(data = inset_lookup,
                     ggplot2::aes(x = 4.6, y = row, label = delay,
                                  color = number_color),
                     hjust = 0.5,
                     size = FIG_BODY_MM, family = FIG_BASE_FAMILY) +
  ggplot2::scale_color_identity() +
  ggplot2::scale_x_continuous(limits = c(-0.05, 5.4), expand = c(0, 0)) +
  ggplot2::scale_y_discrete(limits = rev(panel_a_row_levels), drop = FALSE) +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::theme_void(base_family = FIG_BASE_FAMILY) +
  ggplot2::theme(
    plot.margin      = ggplot2::margin(t = 20, r = 0, b = 3, l = 5),
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    plot.background  = ggplot2::element_rect(fill = "white", color = NA)
  )

# =============================================================================
# Panel B: per-class implied-effect distributions (no canvassers)
# =============================================================================
x_range <- seq(-5, 20, length.out = 300)

panel_b_order <- panel_b_dists |>
  dplyr::arrange(group_te) |>
  dplyr::pull(display_label)

density_data <- panel_b_dists |>
  dplyr::filter(!is.na(group_te), !is.na(model_estimated_sd)) |>
  purrr::pmap_dfr(function(display_label, group_te, model_estimated_sd, ...) {
    tibble::tibble(
      x             = x_range,
      density       = dnorm(x_range, mean = group_te, sd = model_estimated_sd),
      display_label = display_label
    )
  }) |>
  dplyr::mutate(display_label = factor(display_label, levels = panel_b_order))

# Leader-line offsets: Random / Selected / Coached labels share a
# common direction (|dx|, dy) relative to their peak so the three
# leader lines are parallel in data coords (and therefore on the page).
# Random and Selected sit up-and-left of their peaks; Coached sits
# up-and-right of its peak (mirror). The base direction (LABEL_DX,
# LABEL_DY) is then scaled per class to control each leader's *length*
# without changing its angle. Elite Debater stays centred directly
# above its own peak (no leader).
LABEL_DX <- 1.5
LABEL_DY <- 0.28
LABEL_SCALE_RANDOM   <- 0.50  # 50% shorter than the base length
LABEL_SCALE_SELECTED <- 0.75  # 25% shorter than the base length
LABEL_SCALE_COACHED  <- 0.67  # 33% shorter than the base length

peaks_df <- panel_b_dists |>
  dplyr::filter(!is.na(group_te), !is.na(model_estimated_sd)) |>
  dplyr::mutate(
    peak_x = group_te,
    peak_y = 1 / (model_estimated_sd * sqrt(2 * pi)),
    label_x = dplyr::case_when(
      display_label == "Random Layperson"      ~ peak_x - LABEL_DX * LABEL_SCALE_RANDOM,
      display_label == "Elite Layperson"       ~ peak_x - LABEL_DX * LABEL_SCALE_SELECTED,
      display_label == "Elite Debater"         ~ peak_x,
      display_label == "Coached Elite Debater" ~ peak_x + LABEL_DX * LABEL_SCALE_COACHED,
      TRUE                                      ~ peak_x
    ),
    label_y = dplyr::case_when(
      display_label == "Random Layperson"      ~ peak_y + LABEL_DY * LABEL_SCALE_RANDOM,
      display_label == "Elite Layperson"       ~ peak_y + LABEL_DY * LABEL_SCALE_SELECTED,
      display_label == "Elite Debater"         ~ peak_y + 0.075,
      display_label == "Coached Elite Debater" ~ peak_y + LABEL_DY * LABEL_SCALE_COACHED,
      TRUE                                      ~ peak_y
    ),
    label_text = dplyr::case_when(
      display_label == "Random Layperson"      ~ "Random\nLayperson",
      display_label == "Elite Layperson"       ~ "Selected\nLayperson",
      display_label == "Coached Elite Debater" ~ "Coached\nElite Debater",
      display_label == "Elite Debater"         ~ "Elite Debater",
      TRUE                                      ~ as.character(display_label)
    ),
    needs_leader = !(display_label %in% c("Elite Debater"))
  )

bisector_df <- peaks_df |> dplyr::select(peak_x, peak_y)

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

panel_b <- ggplot2::ggplot() +
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
    breaks = x_breaks, labels = x_labels, limits = c(-2, 15.5)
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

# =============================================================================
# Combine + save
# =============================================================================
# Two stacked panels: Cochrane-style forest (text columns + forest) on
# top, per-class distributions on the bottom. Widths are 50/50; the
# forest's x range and panel B's truncated x range are jointly tuned
# above so the red AI dashed line at x = 0 in panel A lands at the
# same horizontal page position as panel B's red AI dashed line at
# x = 13.9.
panel_a_block <- patchwork::wrap_elements(
  full = panel_a_text + panel_a_forest +
    patchwork::plot_layout(ncol = 2, widths = c(0.5, 0.5))
)
panel_b_block <- patchwork::wrap_elements(full = panel_b)

combined <- panel_a_block /
  panel_b_block +
  patchwork::plot_layout(heights = c(0.55, 0.55)) +
  patchwork::plot_annotation(
    tag_levels = "a",
    theme      = ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.tag        = ggplot2::element_text(size = FIG_TAG_PT,
                                              face = "bold",
                                              family = FIG_BASE_FAMILY)
    )
  ) & tag_theme()

# Two-column page; ~115 mm keeps both panels readable while letting the
# manuscript page also carry the (separate) mechanism figure on the
# facing page or below.
save_figure(combined, "limits_figure", FIGURES_MAIN, height = 115)

message("  done.")
