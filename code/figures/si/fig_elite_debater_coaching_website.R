# fig_elite_debater_coaching_website.R
# SI Figure: screenshot of the elite-debater coaching website shown to
# debaters in Study 2. This is a STATIC asset (a screenshot), not a plot
# generated from data, so it has no RDS inputs. The canonical copy lives,
# version-controlled, in code/figures/si/assets/; this script copies it into
# output/figures/si/ so it survives `make clean` (which wipes output/) and is
# emitted alongside the data-driven SI figures.

source(here::here("code/analysis/_setup.R"))

message("\n==== code/figures/si/fig_elite_debater_coaching_website.R ====")

asset_name <- "elite_debater_coaching_website.png"
src <- here::here("code/figures/si/assets", asset_name)
dst <- file.path(FIGURES_SI, asset_name)

if (!file.exists(src)) {
  stop(sprintf("missing static asset: %s", src))
}
if (!dir.exists(FIGURES_SI)) {
  dir.create(FIGURES_SI, recursive = TRUE, showWarnings = FALSE)
}

invisible(file.copy(src, dst, overwrite = TRUE))
message(sprintf("  wrote %s", dst))
message("  done.")
