# fig_subgroups.R
# SI Figure: AI-minus-pooled-humans contrast within each level of 14
# persuadee subgroups.
#
# Layout: a single tall horizontal forest plot. Each row is one
# (subgroup, level) cell; rows are nested first by section (Demographics,
# Political identity, Knowledge and attitudes, Psychological) and then by
# subgroup within section. x-axis is the AI advantage in percentage
# points, with a dashed reference at x = 0. Using horizontal rather than
# vertical bars frees up label space and lets all subgroup levels read
# cleanly at full text width.

source(here::here("code/analysis/_setup.R"))
source(here::here("code/figures/theme.R"))
source(here::here("code/figures/palette.R"))

suppressPackageStartupMessages({
  library(ggplot2)
})

message("\n==== code/figures/si/fig_subgroups.R ====")

# Backs the main-text mechanism-section claim; use S1+S2-only fit.
raw <- load_rds("subgroups_ai_vs_humans_s1_s2")

# ---- pretty-print helpers ----------------------------------------------
# Cleaned-data writes the GBP symbol as the Unicode escape "<U+00A3>"
# instead of the literal "£". Decode here so the figure renders correctly
# without depending on the upstream encoding.
decode_unicode <- function(x) {
  gsub("<U\\+00A3>", "\u00a3", x, perl = TRUE)
}

# Subgroup -> section assignment + display label. Order in this list
# determines vertical order within and across sections in the figure.
SUBGROUP_SPEC <- tibble::tribble(
  ~subgroup,               ~section,                  ~display,
  # Demographics
  "gender",                "Demographics",            "Gender",
  "age_bucket",            "Demographics",            "Age",
  "ethnicity",             "Demographics",            "Ethnicity",
  "education",             "Demographics",            "Education",
  "income",                "Demographics",            "Income bracket",
  # Political identity
  "party",                 "Political identity",      "Party affiliation",
  "ideology_text",         "Political identity",      "Ideology (5-level)",
  "mod_ideology",          "Political identity",      "Ideology (binary)",
  # Knowledge &\nattitudes
  "mod_pre_agreement",     "Knowledge &\nattitudes",   "Pre-treatment attitude",
  "mod_issue_knowledge",   "Knowledge &\nattitudes",   "Issue knowledge (binary)",
  "political_knowledge_hi", "Knowledge &\nattitudes",  "Political knowledge (binary)",
  # Psychological
  "mod_dogmatism",         "Psychological",           "Dogmatism (binary)",
  "empathic_trust_high",   "Psychological",           "Empathic trust (binary)",
  "ai_trust_high",         "Psychological",           "AI trust (binary)"
)

# Level orderings for the binary / ordered subgroups that don't sort
# nicely alphabetically. Anything not listed falls back to alphabetical
# order of the raw level strings.
LEVEL_ORDER <- list(
  age_bucket          = c("<30", "30-49", "50+"),
  income              = c("Less than \u00a320,000",
                          "\u00a320,000 - \u00a329,999",
                          "\u00a330,000 - \u00a349,999",
                          "\u00a350,000 - \u00a374,999",
                          "\u00a375,000 - \u00a399,999",
                          "\u00a3100,000 or more"),
  ideology_text       = c("Left", "Centre-left", "Centre", "Centre-right", "Right"),
  mod_ideology        = c("left", "right"),
  mod_pre_agreement   = c("disagree", "agree"),
  mod_issue_knowledge = c("low", "high"),
  mod_dogmatism          = c("low", "high"),
  empathic_trust_high    = c("low", "high"),
  ai_trust_high          = c("FALSE", "TRUE"),
  political_knowledge_hi = c("FALSE", "TRUE")
)

# Level relabelling for display. Binary moderator levels get capitalised
# / rephrased so the y-axis reads as English rather than column codes.
LEVEL_LABELS <- list(
  mod_ideology        = c(left = "Left-leaning", right = "Right-leaning"),
  mod_pre_agreement   = c(disagree = "Initial disagreers",
                          agree    = "Initial agreers"),
  mod_issue_knowledge = c(low = "Lower knowledge", high = "Higher knowledge"),
  mod_dogmatism       = c(low = "Lower dogmatism", high = "Higher dogmatism"),
  empathic_trust_high = c(low = "Lower empathic trust",
                          high = "Higher empathic trust"),
  ai_trust_high       = c("FALSE" = "Lower AI trust",
                          "TRUE"  = "Higher AI trust"),
  political_knowledge_hi = c("FALSE" = "Lower political knowledge",
                             "TRUE"  = "Higher political knowledge")
)

relabel_level <- function(subgroup, level) {
  lbl <- LEVEL_LABELS[[subgroup]]
  if (is.null(lbl) || is.na(lbl[level])) level else unname(lbl[level])
}

# ---- assemble plotting frame -------------------------------------------
panel_df <- raw |>
  dplyr::mutate(
    level = trimws(decode_unicode(level))
  ) |>
  dplyr::inner_join(SUBGROUP_SPEC, by = "subgroup") |>
  dplyr::mutate(
    section_order = match(section, c("Demographics",
                                     "Political identity",
                                     "Knowledge &\nattitudes",
                                     "Psychological")),
    subgroup_order = match(subgroup, SUBGROUP_SPEC$subgroup),
    level_order = purrr::map2_int(subgroup, level, function(sg, lv) {
      ord <- LEVEL_ORDER[[sg]]
      if (is.null(ord)) NA_integer_ else match(lv, ord)
    }),
    level_display = purrr::map2_chr(subgroup, level, relabel_level)
  ) |>
  dplyr::arrange(section_order, subgroup_order,
                 dplyr::coalesce(level_order, 999L), level_display) |>
  dplyr::mutate(
    section   = factor(section, levels = c("Demographics",
                                           "Political identity",
                                           "Knowledge &\nattitudes",
                                           "Psychological")),
    display   = factor(display, levels = unique(display)),
    # Build a stable y-axis factor whose levels are the FULL ordered set,
    # so blank rows aren't inserted between facets.
    row_key   = paste(subgroup, level, sep = "::"),
    row_key   = factor(row_key, levels = rev(unique(row_key))),
    is_sig    = lower_ci > 0 | upper_ci < 0
  )

# Global alternation (no group_by) ensures no two same-shade rows are ever
# adjacent, even across section boundaries.
shade_lookup <- SUBGROUP_SPEC |>
  dplyr::mutate(shade = dplyr::row_number() %% 2 == 1)

n_sections <- length(unique(SUBGROUP_SPEC$section))

panel_df <- panel_df |>
  dplyr::left_join(shade_lookup[, c("subgroup", "shade")], by = "subgroup")

# shade_df: one row per shaded subgroup, used to route geom_rect to the
# correct facet panels via section + display.
shade_df <- panel_df |>
  dplyr::filter(shade) |>
  dplyr::distinct(section, display)

# strip_bg_list: 18 elements (4 sections + 14 subgroups) matching the
# strip-l grob order. Section strips are transparent; subgroup strips
# mirror the shade pattern so the narrow label column also shows the
# alternating fill.
subgroup_strip_fills <- ifelse(shade_lookup$shade, "grey95", "white")

strip_bg_list <- c(
  rep(list(element_rect(fill = NA, colour = NA)), n_sections),
  lapply(subgroup_strip_fills,
         function(f) element_rect(fill = f, colour = NA))
)

strip_text_list <- c(
  rep(list(element_text(size = FIG_BODY_PT, angle = 90, hjust = 0.5,
                        face = "bold",
                        margin = margin(r = 2, l = 2))), n_sections),
  rep(list(element_text(size = FIG_BODY_PT, angle = 0, hjust = 0,
                        margin = margin(r = 3, l = 3))), nrow(SUBGROUP_SPEC))
)

# Diagnostic: print row counts per section to make it easy to spot drops.
sect_counts <- panel_df |>
  dplyr::count(section, name = "n_rows")
message("  section row counts:")
for (i in seq_len(nrow(sect_counts))) {
  message(sprintf("    %-24s %d", sect_counts$section[i], sect_counts$n_rows[i]))
}

# ---- plot --------------------------------------------------------------
x_lim    <- c(min(panel_df$lower_ci, na.rm = TRUE) - 1,
              max(panel_df$upper_ci, na.rm = TRUE) + 1)
x_breaks <- seq(-10, 15, by = 5)

p <- ggplot(panel_df, aes(x = estimate, y = row_key)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = REF_LINE_GREY, linewidth = 0.3) +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci),
                width = 0, linewidth = 0.5, colour = HUMAN_GREY,
                orientation = "y") +
  geom_point(aes(colour = is_sig), size = 1.6) +
  scale_y_discrete(
    labels = setNames(panel_df$level_display, panel_df$row_key)
  ) +
  scale_x_continuous(breaks = x_breaks, limits = x_lim, expand = c(0.01, 0.01)) +
  scale_colour_manual(values = c("TRUE" = AI_RED, "FALSE" = HUMAN_GREY),
                      guide = "none") +
  ggh4x::facet_nested(
    rows = vars(section, display),
    scales = "free_y", space = "free_y", switch = "y",
    nest_line = element_blank(),
    strip = ggh4x::strip_nested(
      background_x = element_blank(),
      background_y = strip_bg_list,
      text_y       = strip_text_list
    )
  ) +
  labs(x = "AI advantage over pooled humans (pp)",
       y = NULL) +
  theme_persuasion4() +
  theme(
    strip.placement      = "outside",
    panel.background     = element_rect(fill = NA, colour = NA),
    panel.grid.major.x   = element_line(colour = "grey92", linewidth = 0.3),
    panel.spacing.y     = unit(2, "pt"),
    axis.text.y         = element_text(size = FIG_BODY_PT, hjust = 1,
                                       margin = margin(r = 2)),
    axis.title.x        = element_text(size = FIG_TITLE_PT, face = "bold",
                                       hjust = 0.5,
                                       margin = margin(t = 6)),
    plot.margin         = margin(t = 5, r = 20, b = 3, l = 1)
  )

# ---- grob post-processing: grey background on axis-text cells --------------
# The y-axis text (Female, Male, Other …) lives in separate gtable cells
# ("axis-l-N-1") that geom_rect cannot reach. We insert a grey rectGrob
# behind the text for every shaded subgroup panel.
#
# Build (and post-process) the gtable on a null device. Constructing this
# gtable forces text-metric measurement; under a non-interactive `Rscript`
# with no device open, that would otherwise spawn a stray `Rplots.pdf` in the
# working directory. The null device absorbs the measurement; save_figure()
# below opens its own PDF/PNG devices for the actual output.
grDevices::pdf(NULL)
g <- ggplotGrob(p)

# ---- grob post-processing: narrow the outer (section) strip sub-column ----
# Each strip-l cell is internally a 2-column gtable:
#   sub-col 1 = section label (Demographics …)  [currently ~3.6 cm]
#   sub-col 2 = subgroup label (Gender …)        [kept unchanged]
# Shrink sub-col 1 to just fit the rotated bold text (~12 pt).
new_sec_subcol_w <- grid::unit(18, "pt")
all_strip_l_all <- grep("^strip-l", g$layout$name)
for (si in all_strip_l_all) {
  sg <- g$grobs[[si]]
  if (inherits(sg, "gtable") && ncol(sg) >= 1) {
    sg$widths[1] <- new_sec_subcol_w
    g$grobs[[si]] <- sg
  }
}
# Update the outer gtable column 6 width to match the new internal total.
strip_col <- unique(g$layout$l[all_strip_l_all])[1]
old_sg <- g$grobs[[all_strip_l_all[1]]]
subcol2_w <- if (inherits(old_sg, "gtable") && ncol(old_sg) >= 2)
               old_sg$widths[2] else grid::unit(0, "pt")
g$widths[strip_col] <- new_sec_subcol_w + subcol2_w

# ---- compute panel right column for use in separator lines ----------------
panel_r_col <- max(g$layout$r[grep("^panel-", g$layout$name)])

# Panels are single-row cells; find their top positions (one per subgroup).
panel_idx    <- grep("^panel-", g$layout$name)
panel_tops   <- g$layout$t[panel_idx]
panel_order  <- order(panel_tops)          # top → bottom = SUBGROUP_SPEC order
panel_tops_sorted <- panel_tops[panel_order]

# axis-l cells share the same row positions as panels.
axis_l_idx   <- grep("^axis-l", g$layout$name)
axis_tops     <- g$layout$t[axis_l_idx]

for (k in seq_along(shade_lookup$shade)) {
  if (!shade_lookup$shade[k]) next
  target_t   <- panel_tops_sorted[k]
  axis_match <- axis_l_idx[axis_tops == target_t]
  if (length(axis_match) == 0) next
  for (ai in axis_match) {
    orig <- g$grobs[[ai]]
    bg   <- grid::rectGrob(gp = grid::gpar(fill = "grey95", col = NA))
    g$grobs[[ai]] <- grid::grobTree(bg, orig)
  }
}

# ---- grob post-processing: section separator lines ------------------------
# Section strips span multiple rows (t < b); subgroup strips are single-row.
# We draw a full-width charcoal line in the spacing gap between consecutive
# section groups.
all_strip_l <- grep("^strip-l", g$layout$name)
sect_strip_idx <- all_strip_l[
  g$layout$t[all_strip_l] < g$layout$b[all_strip_l]
]
sect_layout <- g$layout[sect_strip_idx, ]
sect_layout  <- sect_layout[order(sect_layout$t), ]

sec_strip_l_col <- min(sect_layout$l)   # left edge of section strip column

for (i in seq_len(nrow(sect_layout) - 1L)) {
  # Collapse every spacing row between this section and the next so there
  # is zero white space surrounding the separator line.
  gap_start <- sect_layout$b[i] + 1L
  gap_end   <- sect_layout$t[i + 1L] - 1L
  if (gap_start <= gap_end) {
    for (r in gap_start:gap_end) g$heights[r] <- grid::unit(0, "pt")
  }
  # Place the line at the bottom edge (y = 0) of the last row in section i,
  # so it sits flush between the last grey panel above and the first panel
  # of the next section below.
  sep_line <- grid::segmentsGrob(
    x0 = grid::unit(0, "npc"), y0 = grid::unit(0, "npc"),
    x1 = grid::unit(1, "npc"), y1 = grid::unit(0, "npc"),
    gp = grid::gpar(col = "grey95", lwd = 4)
  )
  g <- gtable::gtable_add_grob(
    g, list(sep_line),
    t = sect_layout$b[i], b = sect_layout$b[i],
    l = sec_strip_l_col, r = panel_r_col,
    z = Inf, name = paste0("section-sep-", i)
  )
}

grDevices::dev.off()  # close the null device opened before grob construction

save_figure(g, "fig_subgroups", FIGURES_SI,
            width = 183, height = 220, units = "mm")
message("  done.")
