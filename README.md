# AI out-persuades expert humans in text-based dialogue

## Supplementary information

The full Supplementary Information appendix for the paper is included at the
repository root as [`SI_Appendix.pdf`](SI_Appendix.pdf).

## Citation

If you use this code or data, please cite the paper:

```bibtex
@misc{hackenburg2026aisystemsoutpersuadeexpert,
      title={AI out-persuades expert humans in text-based dialogue}, 
      author={Kobi Hackenburg and Caroline Wagner and Luke Hewitt and Ben M. Tappin and Ed Saunders and Hannah Rose Kirk and Helen Margetts and Christopher Summerfield},
      year={2026},
      eprint={2606.16475},
      archivePrefix={arXiv},
      primaryClass={cs.CY},
      url={https://arxiv.org/abs/2606.16475}, 
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

**The conversation transcripts are not available**; see
[*Conversation transcripts*](#conversation-transcripts) below.

## 1. System requirements

- **Operating system.** Any platform supporting R 4.4.3; developed and tested on macOS (Apple Silicon).
- **Programming language.** R, version 4.4.3. Python 3.10 or later (standard library only) is needed only for the Reddit analysis (`make reddit`) and to re-run the optional elite-debater annotation step (`code/analysis/elite_debater/annotate.py --from-cache`). `make all`, which reproduces every other reported result, needs only R.
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

**Expected run time.** About 20 minutes on a normal desktop computer (most of it the follow-up bootstrap, 2,000 resamples, run in parallel).

## 4. Instructions for use

Run individual stages of the pipeline with the targets below; `make help` lists every target.

| Target           | What it does                                                       |
|------------------|--------------------------------------------------------------------|
| `make results`   | Copy `data/derived/` into `output/`, then run `code/analysis/*.R` (`99_*` last) → `output/results/*.rds` + `_numbers.json` |
| `make figures`   | Run `code/figures/{main,si}/*.R` → `output/figures/{main,si}`, plus per-figure source data in `output/source_data/` |
| `make tables`    | Run `code/tables/{main,si}/*.R` → `output/tables/{main,si}`        |
| `make all`       | `results` → `figures` + `tables`                                   |
| `make reddit`    | Run `code/reddit/reddit_pangram.py` → `output/results/reddit/` (SI only; not part of `all`) |
| `make clean`     | Remove everything under `output/`                                  |

Running `make all` regenerates every quantitative result, figure, and table
reported in the manuscript and SI from the data in `data/`.

## 5. Data and de-identification

### Conversation transcripts

The conversation transcripts are not released.
The data were collected at the UK AI Security Institute under a policy that
does not allow transcripts to be shared with third parties, for two reasons. First, the
conversations often contain personal data that is identifiable under the GDPR.
Second, they are dual-use: given many transcripts annotated with persuasion
outcomes (this study has 18,978), it is possible to train cheap but effective
persuasion models ([Hackenburg et al., 2025, *Science*](https://doi.org/10.1126/science.aea3884)),
which malicious actors could misuse. The only transcripts released are the 24
screened examples in `data/example_conversations/`.

Every reported number still reproduces from `data/`. The measures computed
from the transcripts ship as computed, so they can be rerun but not recomputed
from the text: per-conversation message and word counts, fact-checked claim
counts and accuracy (`data/fact_checks/`, `data/derived/si_14_claim_counts.csv`),
reply times and conversation timing (`data/derived/si_12_*`, `si_15_*`, `si_23_*`),
and hedge and booster densities (`data/derived/si_19_conviction_markers.csv`).
The code that computed them is in `code/analysis/py/`, and the fact-checking
prompts are in the SI.

### De-identified export

The data in `data/` are a de-identified export of the study data. To protect
participant privacy while keeping the analysis fully reproducible, the export
contains **only the fields the analysis pipeline reads** and removes direct and
indirect identifiers:

- **Pseudonymous ids.** Every participant and session identifier is replaced by
  a stable pseudonym. The same person maps to the same pseudonym everywhere, so
  all within- and cross-study linkage the analyses rely on is preserved, but the
  original identifiers are not published.
- **No transcripts or free text.** Message transcripts (see above), system
  prompts, open-text responses and similar free text are not included.
- **Free-text survey fields** that the pipeline parses are shipped in their
  derived form only (e.g. debating experience as the parsed number of years;
  study-preparation time as the parsed number of hours; nationality normalised
  to canonical country names). Free-text achievement and career descriptions are
  dropped.
- **Standardised demographics** (gender, education, income band, ethnicity,
  party, ideology) are retained as the original low-cardinality category labels,
  as they drive the subgroup analyses.
- **Follow-up survey.** `data/final_followup_data/` holds the follow-up
  responses (attitude items, donation amount, timestamps) keyed to the same
  conversation pseudonyms. Prolific ids, browser details and payment records
  are not included.
- **Text-derived inputs.** The outputs of `code/analysis/py/*.py`, which read
  the transcripts, ship in `data/derived/` (keys pseudonymised); `make results`
  copies them into place instead of running that step.
- **Example conversations.** The 24 transcripts described in the SI ("Example
  conversations") are released in full in `data/example_conversations/`, as
  screened and drawn by `code/analysis/py/si_12_example_conversations.py`, with
  conversation ids pseudonymised. Their sampling and screening are described
  in the SI.
- **Reddit.** `data/reddit/` holds post ids and Pangram scores for the SI
  analysis of r/PoliticalDiscussion, but no post text, which Reddit's terms do
  not allow us to redistribute (see `data/reddit/README.md`).

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
| `data/` | De-identified, analysis-ready dataset (`study_{0..4}.jsonl`, fact-check CSVs, follow-up survey, text-derived inputs, example conversations, Reddit scores). The single source for all results. |
| `code/analysis/` | R: data → `output/results/*.rds` and `_numbers.json`. |
| `code/figures/`, `code/tables/` | R: `*.rds` → `output/figures/{main,si}` and `output/tables/{main,si}`. |
| `code/analysis/py/`, `code/reddit/` | Python: the text-derived inputs (run in the private repository; outputs in `data/derived/`) and the Reddit analysis. |
| `output/` | Generated artefacts. Figures, tables, figure source data (`source_data/`), and `_numbers.json` are committed for convenience; fitted `*.rds` are gitignored and rebuilt by `make all`. |
| `renv.lock`, `renv/`, `.Rprofile` | Pinned R environment (see *Environment* below). |
| `SI_Appendix.pdf` | Supplementary Information appendix (typeset PDF) for the paper. |

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

## License

The code in this repository is released under the MIT License (`LICENSE`). The
de-identified data under `data/` are released under the Creative Commons
Attribution 4.0 International License (CC BY 4.0; `data/LICENSE`). If you use
either, please cite the paper and the archived repository (see *Citation* above
and `CITATION.cff`).
