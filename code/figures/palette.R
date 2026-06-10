# palette.R
# Single source of truth for every colour used across every figure. The
# rule is: humans use a gray ramp, AI is red, control is black/grey. Any
# script that needs a colour for a persuader class should read it from
# `PALETTE_CLASS` so cross-figure consistency is automatic.

# ---- per-condition colours ---------------------------------------------
PALETTE_CLASS <- c(
  control                  = "#7F7F7F",   # neutral grey
  random_lay_person        = "#D9D9D9",   # lightest gray (least-prepared human)
  selected_lay_person      = "#B5B5B5",
  professional_canvasser   = "#8C8C8C",
  elite_debater            = "#5A5A5A",
  coached_elite_debater    = "#2E2E2E",   # darkest gray (most-prepared human)
  ai_constrained           = "#F4A6A6",   # pale red (Constrained AI)
  ai                       = "#D02828"    # full red (unconstrained AI)
)

# Marker shapes for study (used in Fig 1 to show which study each estimate
# comes from; per-class colour already conveys class).
PALETTE_STUDY_SHAPE <- c(
  study_1 = 16,    # filled circle
  study_2 = 17,    # filled triangle
  study_3 = 15,    # filled square
  study_4 = 18     # diamond
)

# Per-AI-model overlay colours (Fig 1 AI columns). Lean on the
# providers' brand colours so the figure is self-documenting.
PALETTE_AI_MODEL <- c(
  claude_opus_4_1   = "#D97706",   # Anthropic amber
  claude_opus_4_6   = "#B45309",
  gpt_4o            = "#10A37F",   # OpenAI green
  gpt_5_4           = "#0F855C",
  grok_4_20         = "#1F2937",   # xAI charcoal
  gemini_2_5_pro    = "#1A73E8"    # Google blue
)

# Convenience scale_*_manual constructors so figure scripts don't need to
# repeat the labels / palette mapping.
scale_color_class <- function(drop = FALSE) {
  ggplot2::scale_color_manual(
    name   = "Persuader class",
    values = PALETTE_CLASS,
    labels = PERSUADER_CLASS_LABELS,
    drop   = drop
  )
}

scale_fill_class <- function(drop = FALSE) {
  ggplot2::scale_fill_manual(
    name   = "Persuader class",
    values = PALETTE_CLASS,
    labels = PERSUADER_CLASS_LABELS,
    drop   = drop
  )
}

scale_shape_study <- function() {
  ggplot2::scale_shape_manual(
    name   = "Study",
    values = PALETTE_STUDY_SHAPE
  )
}
