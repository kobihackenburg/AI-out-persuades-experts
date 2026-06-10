# fig4_donation.R
# Main-text Figure 4 (realworld_figure.{pdf,png}). Study 4 only.
#
# Organised around the 7 preregistered mechanism strategies rather than
# the 14 individual items: the layout groups the items into the strategy
# pairs they were designed to operationalize (canonical labels from the
# action-persuasion paper; preregistered pair structure in
# study_4_preregistration.txt:418-505).
#
# Layout (Nature double-column, 183 x ~100 mm; only two panels):
#
#   +----------------+-----------------------------+
#   | Panel A        | Panel B (7 strategies)      |
#   | (LMM TE        |                             |
#   |  vs Control)   |                             |
#   +----------------+-----------------------------+
#
# Panels:
#   A: vertical forest of donation TE vs Control for Canvasser + AI
#      (preregistered LMM; adjusted for pre-attitude / pre-donation
#      willingness / age / ideology). Bold signed value labels above
#      each point. AI tick label drawn red-bold via axis_text_per_tick().
#      Sized so the plotting rectangle reads roughly square.
#   B: horizontal forest of 7 mechanism strategies. For each row:
#        - bold AI-red pointrange = composite Welch t on the 2-item
#          per-persuadee mean (from realworld_strategies.rds).
#        - 2 faint AI-red open circles offset vertically = the two
#          individual item estimates (from realworld_mechanism.rds);
#          no CIs to keep the row uncluttered.
#        - white-fill geom_label value label above each composite
#          (parity with the Fig 3 panel A pattern).
#      Rows sorted desc by composite estimate (RDS order).
#
# Out of scope here (covered in text + SI):
#   - The extensive/intensive margin decomposition. Cited inline in the
#     manuscript "Real-money giving" section, preserved as
#     realworld.panel_b.{extensive,intensive}.* keys in _numbers.json,
#     and reported in an SI margin table.
#   - Partner-perception items. The full 7-item battery is reported in
#     the manuscript text only (counts exposed via realworld.perception.*
#     keys in _numbers.json) and in an SI per-item table.
#
# All aesthetics come from code/figures/theme.R. No models are fit.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/figures/theme.R"))

suppressPackageStartupMessages({
  library(patchwork)
})

message("\n==== code/figures/main/fig4_donation.R ====")

# =============================================================================
# Load RDS outputs from code/analysis/03_donation.R
# =============================================================================
te_vs_ctrl     <- load_rds("realworld_te_vs_control")
strategies_df  <- load_rds("realworld_strategies")
mechanism_df   <- load_rds("realworld_mechanism")

# =============================================================================
# Display labels
# =============================================================================
panel_a_group_labels <- c(
  canvasser = "Professional\nCanvasser",
  ai        = "AI"
)

# =============================================================================
# Panel A: donation TE vs Control (Canvasser, AI)
# =============================================================================
panel_a_df <- te_vs_ctrl |>
  dplyr::filter(group %in% c("canvasser", "ai")) |>
  dplyr::mutate(
    group_f   = factor(group, levels = c("canvasser", "ai")),
    color_grp = group,
    label     = sprintf("%+.1f", estimate)
  )

panel_a_y_min <- min(c(panel_a_df$lower_ci, 0)) - 1
panel_a_y_max <- max(panel_a_df$upper_ci) + 4

panel_a <- ggplot2::ggplot(panel_a_df,
                           ggplot2::aes(x = group_f, y = estimate,
                                        color = color_grp)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                      color = REF_LINE_GREY, linewidth = 0.3) +
  ggplot2::geom_segment(
    ggplot2::aes(xend = group_f, y = 0, yend = estimate),
    color = GUIDE_GREY, linewidth = 0.3
  ) +
  # Diamond (Study 4 shape) maintains the paper-wide convention from
  # code/figures/theme.R: Study 1 = circle, 2 = triangle, 3 = square,
  # 4 = diamond. Both points (Canvasser, AI) are Study 4.
  ggplot2::geom_pointrange(
    ggplot2::aes(ymin = lower_ci, ymax = upper_ci),
    shape = study_shapes[["Study 4"]],
    size = 0.9, linewidth = 0.45, fatten = 4,
    show.legend = FALSE
  ) +
  ggplot2::geom_text(
    ggplot2::aes(y = upper_ci, label = label),
    nudge_y = (panel_a_y_max - panel_a_y_min) * 0.025,
    hjust = 0.5, vjust = 0,
    fontface = "bold",
    size = FIG_BODY_MM, family = FIG_BASE_FAMILY,
    show.legend = FALSE
  ) +
  ggplot2::scale_x_discrete(labels = panel_a_group_labels,
                            expand = ggplot2::expansion(add = c(0.6, 0.6))) +
  ggplot2::scale_y_continuous(
    limits = c(panel_a_y_min, panel_a_y_max),
    expand = ggplot2::expansion(mult = c(0, 0)),
    # Explicit breaks at 0/10/20 only: the upper CI for AI extends just
    # past 27, so a tick at 30 would visually overrun the highest data
    # point.
    breaks = c(0, 10, 20)
  ) +
  ggplot2::scale_color_manual(values = c(
    canvasser = HUMAN_GREY,
    ai        = AI_RED
  )) +
  # Y-axis title; the caption carries the "(vs. Control, pp of GBP 1
  # bonus)" expansion.
  ggplot2::labs(x = NULL,
                y = "Effect on donation (pp)") +
  ggplot2::coord_cartesian(clip = "off") +
  theme_persuasion4() +
  ggplot2::theme(
    # No aspect.ratio is set: patchwork's layout grid does not fully
    # honour aspect.ratio when composing panels with `|`, so Panel A's
    # plot rect is sized by its column-width share (see the widths ratio
    # at compose time) rather than a fixed aspect.
    # Light horizontal gridlines at the major y-breaks (10, 20) help
    # the reader read off the AI / Canvasser estimates.
    panel.grid.major.y = ggplot2::element_line(color = "grey92",
                                               linewidth = 0.3),
    axis.text.x  = axis_text_per_tick(
      colors = c(HUMAN_GREY, AI_RED),
      faces  = c("plain",    "bold"),
      lineheight = 0.85
    ),
    axis.title.y = ggplot2::element_text(
      size = FIG_TITLE_PT, face = "bold",
      lineheight = 0.95,
      margin = ggplot2::margin(r = 5)
    ),
    # Top margin sized to fit the patchwork "a" tag with minimal extra
    # space (t=4 keeps the tag tight to the panel edge).
    plot.margin  = ggplot2::margin(t = 4, r = 5, b = 3, l = 5)
  )

# =============================================================================
# Panel B x-range
# =============================================================================
# Union of composite CIs + individual item estimates (faint dots).
# Value labels live to the RIGHT of each composite's upper CI (one
# tidy column of bold red numbers, not on top of the points), so we
# reserve roughly 5 data units of headroom past the rightmost data
# element for the labels.
panel_b_lo <- min(c(strategies_df$lower_ci, mechanism_df$estimate, 0),
                  na.rm = TRUE)
panel_b_hi <- max(c(strategies_df$upper_ci, mechanism_df$estimate, 0),
                  na.rm = TRUE)
panel_b_pad <- (panel_b_hi - panel_b_lo) * 0.04
panel_b_x_min   <- panel_b_lo - panel_b_pad
# Anchor for the right-side labels: start them just past the maximum
# data extent.
panel_b_label_x <- panel_b_hi + (panel_b_hi - panel_b_lo) * 0.05
# Right edge: enough room for the widest "+XX.X" label after the
# anchor.
panel_b_x_max   <- panel_b_label_x + (panel_b_hi - panel_b_lo) * 0.22

# =============================================================================
# Panel B: 7 mechanism-strategy composites (with faint per-item dot overlay)
# =============================================================================
# Strategy y-axis ordering: take the RDS order (already sorted desc by
# composite estimate) and reverse it because ggplot factor level 1 sits
# at the BOTTOM of the y-axis.
panel_b_levels <- rev(strategies_df$strategy)
panel_b_df <- strategies_df |>
  dplyr::mutate(
    strategy_f = factor(strategy, levels = panel_b_levels),
    label      = sprintf("%+.1f", estimate)
  )

# Faint-dots overlay. Long-format with one row per (strategy, item),
# plus a y_offset so the two item dots for a given strategy sit just
# above and just below the composite row centre. The strategy -> items
# mapping is read straight off strategies_df (item_a / item_b columns)
# so this script has no dependency on STRATEGY_PAIRS at the analysis layer.
panel_b_item_dots <- strategies_df |>
  dplyr::select(strategy, item_a, item_b) |>
  tidyr::pivot_longer(c(item_a, item_b),
                      names_to  = "slot",
                      values_to = "item") |>
  dplyr::inner_join(mechanism_df |> dplyr::select(item, estimate),
                    by = "item") |>
  dplyr::mutate(
    strategy_f = factor(strategy, levels = panel_b_levels),
    y_offset   = dplyr::if_else(slot == "item_a", +0.18, -0.18)
  )

panel_b <- ggplot2::ggplot(panel_b_df,
                           ggplot2::aes(x = estimate, y = strategy_f)) +
  # Truncated horizontal lead lines from the left edge of the plotting
  # rectangle out to each composite estimate. Same anchor-to-label
  # pattern as Fig 3 panel A (code/figures/main/fig3_mechanism.R):
  # gives the reader a row guide without painting full-panel vertical
  # gridlines.
  ggplot2::geom_segment(
    ggplot2::aes(x = panel_b_x_min, xend = estimate,
                 y = strategy_f, yend = strategy_f),
    color = GUIDE_GREY, linewidth = 0.3
  ) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                      color = REF_LINE_GREY, linewidth = 0.3) +
  # Faint per-item dots first (under the composite). Open diamond
  # (study_shapes_open[["Study 4"]] = 5) keeps the Study 4 visual
  # language consistent: every marker in this panel is a diamond,
  # with the composite filled (Study 4 = 18) and the raw item
  # estimates open.
  ggplot2::geom_point(
    data = panel_b_item_dots,
    ggplot2::aes(x = estimate,
                 y = as.numeric(strategy_f) + y_offset),
    inherit.aes = FALSE,
    color = AI_RED, alpha = 0.55,
    shape = study_shapes_open[["Study 4"]],
    # Sized down ~30% from earlier 1.6 / 0.6 so the per-item dots read
    # clearly as supporting evidence behind the bold composite
    # diamonds (size 0.55, fatten 4) rather than competing with them
    # for visual weight.
    size = 1.1, stroke = 0.45
  ) +
  # Composite pointrange (filled Study 4 diamond). No value label on
  # top: the +XX.X strings live in a right-side column (see geom_text
  # below).
  ggplot2::geom_pointrange(
    ggplot2::aes(xmin = lower_ci, xmax = upper_ci),
    color = AI_RED,
    shape = study_shapes[["Study 4"]],
    size = 0.55, linewidth = 0.5, fatten = 4
  ) +
  # Right-side value column. Same bold AI-red as the points, left-
  # aligned at a fixed x so all 7 labels form a tidy vertical column
  # past the data extent. geom_text (no white box) is fine here since
  # the labels don't overlap any geom.
  ggplot2::geom_text(
    ggplot2::aes(x = panel_b_label_x, label = label),
    hjust = 0, vjust = 0.5,
    fontface = "bold", color = AI_RED,
    size = FIG_BODY_MM, family = FIG_BASE_FAMILY
  ) +
  ggplot2::scale_y_discrete(
    labels = setNames(strategies_df$display, strategies_df$strategy),
    # Tight expansion: no need to reserve vertical room above the
    # topmost row for an above-point label anymore.
    expand = ggplot2::expansion(add = c(0.5, 0.5))
  ) +
  ggplot2::scale_x_continuous(
    limits = c(panel_b_x_min, panel_b_x_max),
    expand = ggplot2::expansion(mult = c(0, 0)),
    breaks = c(0, 5, 10, 15, 20)
  ) +
  # Combined bottom header. We use plot.caption (rather than
  # axis.title.x) because axis titles centre on the plot area
  # (narrower than the title), whereas plot.caption with
  # plot.caption.position = "plot" centres on the full Panel B column.
  # Broken onto two lines so the title sits well within the column
  # even at the equal-width A|B layout.
  ggplot2::labs(x = NULL, y = NULL,
                caption = "AI Effect on Donation Mechanisms\nvs. Canvassers (pp)") +
  ggplot2::coord_cartesian(clip = "off") +
  theme_persuasion4() +
  ggplot2::theme(
    axis.text.y    = ggplot2::element_text(size = FIG_BODY_PT,
                                           color = "black",
                                           lineheight = 0.95),
    axis.text.x    = ggplot2::element_text(size = FIG_BODY_PT,
                                           color = "gray35"),
    plot.caption          = ggplot2::element_text(size = FIG_TITLE_PT,
                                                  face = "bold",
                                                  family = FIG_BASE_FAMILY,
                                                  hjust = 0.5,
                                                  lineheight = 0.95,
                                                  margin = ggplot2::margin(t = 6)),
    plot.caption.position = "plot",
    # Match Panel A's t=4 so the "a" and "b" tags sit on the same
    # baseline and neither panel reads as floating below empty space.
    plot.margin    = ggplot2::margin(t = 4, r = 6, b = 3, l = 5)
  )

# =============================================================================
# Compose + save
# =============================================================================
# Two-panel layout: (A | B). Panel A is composed directly (not via
# wrap_elements()) so its plot rect is sized by the column-width share
# below rather than being normalised to a fixed cell.
#
# Layout dims:
#   * widths c(1.3, 1.0): with patchwork's relative-widths algorithm,
#     a 1:1 split would under-weight Panel A because its column
#     allocation is eroded by Panel B's long y-axis labels
#     ("Information: Impact Efficacy", etc.). The 1.3:1 ratio gives A's
#     plot rect a physical width comparable to B's.
#   * Figure height 72 mm: with both panels' top plot.margin trimmed to
#     t=4 (just enough for the patchwork "a"/"b" tags), the remaining
#     ~65 mm of plot height accommodates Panel B's 7 strategy rows
#     while keeping Panel A's 2-point comparison legible, for an overall
#     183 x 72 mm (~2.5:1) aspect.
combined <- (panel_a | panel_b) +
  patchwork::plot_layout(widths = c(1.3, 1.0)) +
  patchwork::plot_annotation(
    tag_levels = list(c("a", "b")),
    theme      = ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.tag        = ggplot2::element_text(size = FIG_TAG_PT,
                                              face = "bold",
                                              family = FIG_BASE_FAMILY)
    )
  ) & tag_theme()

save_figure(combined, "realworld_figure", FIGURES_MAIN, height = 72)

message("  done.")
