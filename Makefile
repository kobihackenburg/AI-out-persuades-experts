# persuasion-4-v2: analysis pipeline
#
#   data/ -> output/results/ -> output/figures + output/tables
#
# Quick start:
#   make help          show every target
#   make all           run the full pipeline
#   make clean         delete everything under output/
#
# All R scripts source code/analysis/_setup.R, which is responsible for paths and
# library loads, and read the dataset shipped in data/.

SHELL    := /bin/bash
RSCRIPT  ?= Rscript

# ---- discover scripts ---------------------------------------------------
# Stage scripts (01-04) + SI scripts (si_*), discovered and run in sorted
# order, then 99_extract_numbers.R LAST: it reads RDS produced by every other
# analysis script (incl. si_*), so it must run after them. Appending it
# explicitly avoids relying on lexical sort (where "99_" would sort before
# "si_").
EXTRACT_NUMBERS_R := code/analysis/99_extract_numbers.R
ANALYSIS_R       := $(filter-out $(EXTRACT_NUMBERS_R),$(sort $(wildcard code/analysis/[0-9]*.R code/analysis/si_*.R))) $(EXTRACT_NUMBERS_R)
FIGURES_MAIN_R   := $(sort $(wildcard code/figures/main/*.R))
FIGURES_SI_R     := $(sort $(wildcard code/figures/si/*.R))
TABLES_MAIN_R    := $(sort $(wildcard code/tables/main/*.R))
TABLES_SI_R      := $(sort $(wildcard code/tables/si/*.R))

# ---- top-level phony targets -------------------------------------------
# Phony, not file-based: each stage's script knows how to do all studies in
# one call, and the cost of per-rds dependency tracking outweighs the
# benefit when there are only a handful of scripts. Re-runs are explicit.
.PHONY: help all results figures tables clean clean-cache renv-restore

help:
	@printf "persuasion-4-v2 pipeline targets:\n"
	@printf "  make results      %s\n" "run code/analysis/*.R -> output/results/*.rds"
	@printf "  make figures      %s\n" "run code/figures/{main,si}/*.R -> output/figures/{main,si}"
	@printf "  make tables       %s\n" "run code/tables/{main,si}/*.R -> output/tables/{main,si}"
	@printf "  make all          %s\n" "results -> figures + tables (reads data/)"
	@printf "  make clean        %s\n" "remove generated files under output/"
	@printf "  make clean-cache  %s\n" "remove output/results/_data/*.rds (force JSONL re-parse)"
	@printf "  make renv-restore %s\n" "Rscript -e 'renv::restore()'"

all: results figures tables

# ---- R: data -> output/results -----------------------------------------
results:
	@for f in $(ANALYSIS_R); do \
	  echo "==> $$f" ; \
	  $(RSCRIPT) $$f || exit 1 ; \
	done

# ---- R: output/results -> output/figures + output/tables ---------------
figures:
	@for f in $(FIGURES_MAIN_R) $(FIGURES_SI_R); do \
	  echo "==> $$f" ; \
	  $(RSCRIPT) $$f || exit 1 ; \
	done

tables:
	@for f in $(TABLES_MAIN_R) $(TABLES_SI_R); do \
	  echo "==> $$f" ; \
	  $(RSCRIPT) $$f || exit 1 ; \
	done

# ---- housekeeping ------------------------------------------------------
clean:
	@find output -type f ! -name '.gitkeep' -delete

clean-cache:
	@rm -f output/results/_data/*.rds

renv-restore:
	$(RSCRIPT) -e 'renv::restore()'
