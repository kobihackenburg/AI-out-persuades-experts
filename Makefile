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
# Python preprocessing (code/analysis/py/*.py, standard library only) writes
# CSVs to output/results/_py/ that some si_* scripts read, so it runs first.
# It reads message text, which the public release does not carry; there,
# data/derived/ holds its outputs and is copied into place instead.
DERIVED          := data/derived
PYTHON           ?= python3
ANALYSIS_PY      := $(sort $(wildcard code/analysis/py/*.py))
FIGURES_MAIN_R   := $(sort $(wildcard code/figures/main/*.R))
FIGURES_SI_R     := $(sort $(wildcard code/figures/si/*.R))
TABLES_MAIN_R    := $(sort $(wildcard code/tables/main/*.R))
TABLES_SI_R      := $(sort $(wildcard code/tables/si/*.R))

# ---- top-level phony targets -------------------------------------------
# Phony, not file-based: each stage's script knows how to do all studies in
# one call, and the cost of per-rds dependency tracking outweighs the
# benefit when there are only a handful of scripts. Re-runs are explicit.
.PHONY: help all results figures tables reddit clean clean-cache renv-restore

help:
	@printf "persuasion-4-v2 pipeline targets:\n"
	@printf "  make results      %s\n" "run code/analysis/py/*.py (or copy data/derived/) then code/analysis/*.R -> output/results/"
	@printf "  make figures      %s\n" "run code/figures/{main,si}/*.R -> output/figures/{main,si}"
	@printf "  make tables       %s\n" "run code/tables/{main,si}/*.R -> output/tables/{main,si}"
	@printf "  make all          %s\n" "results -> figures + tables (reads data/)"
	@printf "  make reddit       %s\n" "code/reddit/reddit_pangram.py -> output/results/reddit/ (SI only)"
	@printf "  make clean        %s\n" "remove generated files under output/"
	@printf "  make clean-cache  %s\n" "remove output/results/_data/*.rds (force JSONL re-parse)"
	@printf "  make renv-restore %s\n" "Rscript -e 'renv::restore()'"

all: results figures tables

# ---- R: data -> output/results -----------------------------------------
results:
	@if [ -d $(DERIVED) ]; then \
	  echo "==> $(DERIVED)/ -> output/results/_py/ + output/tables/si/ (no message text: Python step skipped)" ; \
	  mkdir -p output/results/_py output/tables/si ; \
	  cp $(DERIVED)/*.csv output/results/_py/ ; \
	  cp $(DERIVED)/*.tex output/tables/si/ ; \
	else \
	  for f in $(ANALYSIS_PY); do \
	    echo "==> $$f" ; \
	    $(PYTHON) $$f || exit 1 ; \
	  done ; \
	fi
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

# ---- Reddit AI-content analysis (SI only; separate data, not in `all`) ----
reddit:
	$(PYTHON) code/reddit/reddit_pangram.py

# ---- housekeeping ------------------------------------------------------
clean:
	@find output -type f ! -name '.gitkeep' -delete

clean-cache:
	@rm -f output/results/_data/*.rds

renv-restore:
	$(RSCRIPT) -e 'renv::restore()'
