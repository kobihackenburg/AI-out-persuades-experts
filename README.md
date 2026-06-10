# AI systems out-persuade expert humans

## Citation

If you use this code or data, please cite:

```bibtex
@article{hackenburg2026outpersuade,
  title   = {AI systems out-persuade expert humans},
  author  = {Hackenburg, Kobi and Wagner, Caroline and Hewitt, Luke and
             Tappin, Ben M. and Saunders, Ed and Kirk, Hannah Rose and
             Margetts, Helen and Summerfield, Christopher},
  year    = {2026},
  journal = {arXiv preprint arXiv:XXXX.XXXXX},  % TODO: add arXiv id once posted
  url     = {https://arxiv.org/abs/XXXX.XXXXX}  % TODO: add arXiv URL once posted
}
```

Reproduction repository for the paper. The pipeline runs end to end from the
de-identified dataset to every figure, table, and in-text number:

```
data/ ──► code/analysis/*.R ──► output/results/*.rds ──► code/{figures,tables}/*.R ──► output/{figures,tables}
```

Every script, data file, and configuration needed to reproduce the published
results lives under this directory; nothing depends on a parent directory at
runtime.

## 1. System requirements

- **Operating system.** Any platform supporting R 4.4.3; developed and tested on macOS (Apple Silicon).
- **Programming language.** R, version 4.4.3.
- **Software dependencies.** All package dependencies are pinned to exact versions in `renv.lock` and restored automatically with `renv` (see the *Installation guide* below). No dependencies need be installed by hand.
- **Non-standard hardware.** None required.

## 2. Installation guide

Install R 4.4.3, then restore the pinned package set from the repository root:

```bash
Rscript -e 'renv::restore()'   # install the exact pinned package set (R 4.4.3)
```

**Typical install time.** Under 10 minutes on a normal desktop computer.

## 3. Demo

The de-identified study data are included in `data/`, so the pipeline itself
serves as the demonstration. A single command runs every analysis, figure, and
table on the full dataset.

```bash
make all                       # data/ -> results -> figures + tables
```

**Expected output.** Fitted models and summary statistics under `output/results/` (including `_numbers.json`, a lookup of every scalar cited in the manuscript), figures under `output/figures/{main,si}`, and tables under `output/tables/{main,si}`.

**Expected run time.** Under 10 minutes on a normal desktop computer.

## 4. Instructions for use

Run individual stages of the pipeline with the targets below; `make help` lists every target.

| Target           | What it does                                                       |
|------------------|--------------------------------------------------------------------|
| `make results`   | Run `code/analysis/*.R` (`99_*` last) → `output/results/*.rds` + `_numbers.json` |
| `make figures`   | Run `code/figures/{main,si}/*.R` → `output/figures/{main,si}`      |
| `make tables`    | Run `code/tables/{main,si}/*.R` → `output/tables/{main,si}`        |
| `make all`       | `results` → `figures` + `tables`                                   |
| `make clean`     | Remove everything under `output/`                                  |

Running `make all` regenerates every quantitative result, figure, and table
reported in the manuscript and SI from the data in `data/`.

## 5. Data and de-identification

The data in `data/` are a de-identified export of the study data. To protect
participant privacy while keeping the analysis fully reproducible, the export
contains **only the fields the analysis pipeline reads** and removes direct and
indirect identifiers:

- **Pseudonymous ids.** Every participant and session identifier is replaced by
  a stable pseudonym. The same person maps to the same pseudonym everywhere, so
  all within- and cross-study linkage the analyses rely on is preserved, but the
  original identifiers are not published.
- **No raw transcripts.** Full message transcripts, system prompts, open-text
  responses, and similar free text are not part of the analysis inputs and are
  not included. Per-conversation summaries the models consume (message/word
  counts, fact-check claim counts and mean veracity, attitudes) are retained.
- **Free-text survey fields** that the pipeline parses are shipped in their
  derived form only (e.g. debating experience as the parsed number of years;
  study-preparation time as the parsed number of hours; nationality normalised
  to canonical country names). Free-text achievement and career descriptions are
  dropped.
- **Standardised demographics** (gender, education, income band, ethnicity,
  party, ideology) are retained as the original low-cardinality category labels,
  as they drive the subgroup analyses.

`code/redact_data.R` is the exact, deterministic script used to produce this
export from the raw study data; it documents precisely which fields are kept,
dropped, or transformed.

The elite-debater achievement coding lives in
`code/analysis/elite_debater/`: the committed `annotations.json` and `audit.csv`
contain the per-criterion booleans the pipeline reads, with evidence quotes and
the raw `input.json` (free-text achievements) withheld for privacy. The numbers
remain reproducible from the cache with
`python3 code/analysis/elite_debater/annotate.py --from-cache`.

## Layout

| Path | Contents |
|------|----------|
| `data/` | De-identified, analysis-ready dataset (`study_{0..4}.jsonl`, fact-check CSVs). The single source for all results. |
| `code/analysis/` | R: data → `output/results/*.rds` and `_numbers.json`. |
| `code/figures/`, `code/tables/` | R: `*.rds` → `output/figures/{main,si}` and `output/tables/{main,si}`. |
| `output/` | Generated artefacts. Figures, tables, and `_numbers.json` are committed for convenience; fitted `*.rds` are gitignored and rebuilt by `make all`. |
| `renv.lock`, `renv/`, `.Rprofile` | Pinned R environment (see *Environment* below). |

The `code/analysis/` and `data/` directories each contain a `README.md` with
further detail; in particular, `data/README.md` describes the dataset structure,
analytical sample definitions, and per-study observation counts.

## How it works

- Analysis fits each model **once** and writes it as an `.rds`. Figures and
  tables only read those `.rds` files, so editing a figure never re-runs a
  model.
- `code/analysis/99_extract_numbers.R` runs last and emits
  `output/results/_numbers.json` — a lookup of every scalar the manuscript
  cites.
- The one step **outside** `make all` is the elite-debater LLM annotation
  (`code/analysis/elite_debater/`); the committed `audit.csv` is read directly
  by the pipeline and is reproducible from cache.

## Environment

The exact package set is pinned in `renv.lock` (R 4.4.3) and restored with
`renv::restore()`. `.Rprofile` and `.renvignore` are deliberate: they add the
renv library to the path manually and stop renv from re-scanning the large
`data/`/`output/` trees on every R startup. See the comments in `.Rprofile`.
