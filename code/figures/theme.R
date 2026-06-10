# theme.R
# =============================================================================
# Single source of truth for every aesthetic spec used in every figure in the
# paper (main + SI). Font sizes, colors, marker shapes, panel margins, save
# dimensions all live here. Figure scripts should never set a font size, a
# color hex, or a panel margin locally; if a tweak is needed across the
# whole paper, edit it here.
#
# Two-column print sizing
# -----------------------
# The manuscript is set in a single-column `article` documentclass. Main
# figures are placed in `figure*` environments and included with
# `\includegraphics[width=\linewidth]`. We save at 183 mm wide -- the
# de-facto two-column print width for Nature / PNAS-style layouts and the
# closest standard match to the rendered \linewidth on this template.
# Saving at the same width as the final print means the 8 pt body text in
# the saved PDF stays 8 pt in the rendered manuscript (no rescaling at
# typesetting time).
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
})

# =============================================================================
# Typography
# =============================================================================
# Sizes are in pt for `theme()` slots and mm for geom_text() / annotate()
# (ggplot uses mm there; the conversion is pt = mm * 2.835).
FIG_TAG_PT    <- 18L
FIG_TITLE_PT  <- 10L
FIG_BODY_PT   <- 8L
FIG_LEGEND_PT <- 7L

FIG_BODY_MM   <- as.numeric(FIG_BODY_PT)   / 2.835
FIG_LEGEND_MM <- as.numeric(FIG_LEGEND_PT) / 2.835

FIG_BASE_FAMILY <- "Helvetica"

# =============================================================================
# Palette
# =============================================================================
HUMAN_GREY            <- "#4A4A4A"
AI_RED                <- "#E41A1C"
AI_RED_DK             <- "#B71C1C"
AI_CONSTRAINED_SALMON <- "#FA8072"
GUIDE_GREY            <- "grey92"   # truncated guide / lead lines
RULE_GREY             <- "gray25"   # booktabs-style rules
REF_LINE_GREY         <- "gray50"   # x=0 dashed reference

# =============================================================================
# Marker shapes
# =============================================================================
# Study shapes used in every panel that shows per-study estimates.
# Convention is paper-wide: Study 1 = circle, Study 2 = triangle,
# Study 3 = square, Study 4 = diamond (the conventional "4th cohort"
# shape in meta-analytic forest plots, visually distinct from the
# first three). Open variants (used e.g. for individual-item dots
# overlaid on composite estimates) are kept parallel: 1/2/0/5.
study_shapes      <- c("Study 1" = 16, "Study 2" = 17, "Study 3" = 15, "Study 4" = 18)
study_shapes_open <- c("Study 1" = 1,  "Study 2" = 2,  "Study 3" = 0,  "Study 4" = 5)

# AI per-model overlay stencils (Fig 1 AI columns). We pre-declare
# all the labels we've ever used so the manual legend stays stable as
# new models are added; only the labels actually present in the data
# end up plotted.
ai_version_shapes <- c(
  "Claude Opus 4.1"     = 2,   # open triangle up
  "Claude Opus 4.6"     = 6,   # open triangle down
  "ChatGPT-4o (latest)" = 0,   # open square
  "GPT-5.4"             = 12,  # square with plus
  "Grok 4.20"           = 8,   # asterisk
  "Gemini 2.5 Pro"      = 4    # cross
)

# =============================================================================
# Theme function
# =============================================================================
# theme_persuasion4(): paper-wide ggplot theme. Built on theme_minimal()
# because every panel in the paper wants white backgrounds, no minor
# gridlines, and no axis lines except where added explicitly (e.g. Fig 1
# draws its own per-category guide segments rather than relying on
# a panel grid).
#
# Per-panel overrides (axis ticks, vectorised per-tick colours, plot.tag
# size for patchwork labels) should be added with `+ theme(...)` after this.
theme_persuasion4 <- function(base_size = FIG_BODY_PT,
                              base_family = FIG_BASE_FAMILY) {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      legend.position    = "none",
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      axis.text          = ggplot2::element_text(size = base_size),
      axis.title.x       = ggplot2::element_text(size = FIG_TITLE_PT,
                                                 face = "bold",
                                                 margin = ggplot2::margin(t = 5)),
      axis.title.y       = ggplot2::element_text(size = FIG_TITLE_PT,
                                                 face = "bold",
                                                 margin = ggplot2::margin(r = 9)),
      panel.background   = ggplot2::element_rect(fill = "white", color = NA),
      plot.background    = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin        = ggplot2::margin(t = 5, r = 5, b = 3, l = 5)
    )
}

# tag_theme(): patchwork plot.tag styling (the bold "a"/"b" panel labels).
# Apply as `combined + plot_annotation(tag_levels = "a", theme = tag_theme())`
# or via `& tag_theme()`.
tag_theme <- function(size = FIG_TAG_PT) {
  ggplot2::theme(
    plot.tag = ggplot2::element_text(size = size, face = "bold",
                                     family = FIG_BASE_FAMILY)
  )
}

# axis_text_per_tick(): build an element_text() with vectorised per-tick
# colour / face / hjust. ggplot2 warns that vectorised element_text input
# is unofficial, but it has rendered correctly since 3.x and is the only
# practical way to colour individual axis tick labels (e.g. the red bold
# AI tick on Fig 1). All inputs must be the same length as the
# number of breaks on the axis.
axis_text_per_tick <- function(colors, faces = "plain", hjusts = NULL,
                               size = FIG_BODY_PT, lineheight = 0.9) {
  args <- list(
    size       = size,
    lineheight = lineheight,
    color      = colors,
    face       = faces
  )
  if (!is.null(hjusts)) args$hjust <- hjusts
  do.call(ggplot2::element_text, args)
}

# =============================================================================
# save_figure(): paper-wide figure writer.
# =============================================================================
# Writes both PDF (manuscript) and PNG (slides / quick previews) at the
# same dimensions. Defaults are pinned to the two-column print width so
# that figure scripts only need to pass a height (most main figures want
# 135 mm; Fig 2's two stacked panels are saved at 115 mm).
#
# Use `units = "mm"` to size in millimetres (Nature/PNAS two-column = 183
# mm); pass `units = "in"` for inches if needed.
#
# We deliberately do NOT pass `device = cairo_pdf`: that requires X11 /
# XQuartz, and the default `grDevices::pdf` device renders cleanly for
# this paper without that dependency.
save_figure <- function(plot, name, dir,
                        width  = 183,
                        height = 135,
                        units  = "mm",
                        dpi    = 300,
                        bg     = "white") {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  pdf_path <- file.path(dir, paste0(name, ".pdf"))
  png_path <- file.path(dir, paste0(name, ".png"))
  ggplot2::ggsave(pdf_path, plot,
                  width = width, height = height, units = units, bg = bg)
  ggplot2::ggsave(png_path, plot,
                  width = width, height = height, units = units, dpi = dpi, bg = bg)
  message(sprintf("  wrote %s", pdf_path))
  message(sprintf("  wrote %s", png_path))
  invisible(list(pdf = pdf_path, png = png_path))
}
