# code/analysis/

R scripts that read `data/*.jsonl` and write fitted models +
summary tables to `output/results/*.rds`. Each script owns one slice of
the manuscript; figure + table code (under `code/figures/` and
`code/tables/`) consumes the RDS, never the data directly.

## Pipeline

```
data/study_X.jsonl  ──►  code/analysis/*.R  ──►  output/results/*.rds
                                                ──►  output/results/_numbers.json
```

Run order (enforced by the Makefile): the stage scripts `01`–`04` and the
SI scripts `si_01`–`si_11` run first (in sorted order), then
`99_extract_numbers.R` runs **last** because it reads RDS produced by
every other analysis script.

## Scripts

| Script | Loads | Writes (output/results/) | Manuscript home |
|---|---|---|---|
| `_setup.R` | — (sourced first) | — | shared paths, libraries, constants, `save_rds`/`load_rds` |
| `_load_data.R` | `data/*.jsonl` | `_data/study_X.rds` (cache) | shared data loader |
| `_lmm_helpers.R` | — (sourced) | — | shared LMM fit + contrast helpers |
| `01_main_attitudes.R` | `study_{1,2,3}` | `pooled_s1_s3_lmm`, `pooled_s1_s2_lmm`, `study{1,2,3}_prereg_lmm`, `per_persuader_re`, `summary_class_effects`, `summary_ai_overlays`, `limits_class_distributions`, `limits_ai_reference`, `limits_per_persuader_re`, `mechanism_te_inputs`, `canvasser_*` (distributions, AI ref, TE inputs), `robustness_*` | Fig 1, Fig 2b inputs, SI robustness + distributions, body text |
| `02_mechanism.R` | `study_{1,2,3}` | `limits_contrasts`, `limits_inset`, `mechanism_constraint_effects`, `mechanism_facts_vs_te`, `mechanism_facts_regression`, `canvasser_facts_vs_te`, `canvasser_facts_regression`, `mechanism_vs_coached_{contrasts,inset}` (Coached-reference contrasts) | Fig 2a forest, Fig 3a–b |
| `03_donation.R` | `study_4` | `study4_donation_lmm`, `realworld_te_vs_control`, `realworld_ai_vs_canvasser`, `realworld_strategies`, `realworld_margins`, `realworld_perception`, `realworld_mechanism` | Fig 4 |
| `04_bonuses.R` | `summary_class_effects`, `robustness_main_spec`, `per_persuader_re` | `bonuses`, `bonuses_per_persuader` | body-text bonus figures (via `_numbers.json`) |
| `si_01_issue_level.R` | `study_{1,2,3}` | `issue_level_ai_vs_humans_s1_s2`, `issue_level_ai_vs_humans` | per-issue figure + canvasser-inclusion robustness |
| `si_02_subgroups.R` | `study_{1,2,3}` | `subgroups_ai_vs_humans{,_s1_s2}`, `moderator_effects{,_s1_s2}` | SI subgroups figure + moderator tables |
| `si_03_attrition.R` | `study_{1,2,3,4}` | `attrition` | SI attrition section |
| `si_04_partner_ratings.R` | `study_{1,2,3}` | `partner_ratings_s123` | SI partner-ratings table |
| `si_05_attrition_sensitivity.R` | `study_{1,2,3}` | `lee_bounds` | SI Lee-bounds sensitivity |
| `si_06_fact_density_within.R` | `study_{1,2,3}` | `fact_density_within` | SI within-condition fact-density check |
| `si_07_ai_gap_net_of_facts.R` | `study_{1,2,3}` | `si_ai_gap_net_of_facts` | SI AI-gap-net-of-facts |
| `si_08_coaching_behaviour_shift.R` | `study_{1,2}` | `coaching_behaviour_shift` | SI coaching-shift, Discussion parenthetical |
| `si_09_fact_accuracy.R` | `study_{1,2,3}`, `canvasser_facts_vs_te` | `fact_accuracy_by_study`, `fact_accuracy_by_condition` | SI fact-accuracy tables |
| `si_10_raw_per_persuader.R` | `study_{1,2}`, `limits_ai_reference` | `si_raw_per_persuader` | SI raw per-persuader BLUP table |
| `si_11_repeat_participation.R` | `study_{0,1,2,3,4}` | `repeat_descriptives`, `repeat_overlap`, `repeat_issue_check`, `repeat_first_session`, `repeat_first_session_ai_minus`, `repeat_carryover`, `repeat_summary` | SI repeated-participation tables + body-text robustness |
| `99_extract_numbers.R` | every `output/results/*.rds` + `elite_debater/audit.csv` | `_numbers.json`, per-cohort audit CSVs | every inline number in `manuscript.tex` / `SI.tex` |

`elite_debater/` holds the one out-of-pipeline step (LLM annotation of
the Study 1 elite debaters); see `code/analysis/elite_debater/README.md`.

## RDS naming

RDS object names are **consumer-aligned and content-based**, so the file
name says which figure/section it backs:

| Prefix | Backs | Example |
|---|---|---|
| `summary_*` | Figure 1 (summary forest) | `summary_class_effects` |
| `limits_*` | Figure 2 (limits: forest + per-class distributions) | `limits_contrasts`, `limits_class_distributions` |
| `mechanism_*` | Figure 3 (mechanism: constraint effects + facts scatter) | `mechanism_constraint_effects`, `mechanism_facts_vs_te` |
| `realworld_*` | Figure 4 (donation / real-world action) | `realworld_te_vs_control` |
| `canvasser_*` | SI canvasser-inclusion (S1–S3) robustness figures | `canvasser_facts_vs_te` |

Model objects keep descriptive names (`pooled_s1_s3_lmm`,
`study4_donation_lmm`, `robustness_*`, `issue_level_*`,
`subgroups_ai_vs_humans*`, `moderator_effects*`, etc.).

## Conventions

- Every script starts with `source(here::here("code/analysis/_setup.R"))` and (if it touches data) `source(here::here("code/analysis/_load_data.R"))`. No script defines its own libraries or paths.
- The only output destination is `output/results/<name>.rds`, via `save_rds()` in `_setup.R`. Analysis scripts never write to `output/figures/` or `output/tables/`.
- Per-study LMMs, the pooled S1–S3 model (Fig 1 summary, canvasser-inclusion robustness, per-class contrast tables), and the pooled S1+S2-only model (main-text limits + mechanism sections, Figs 2–3) all live in `01_main_attitudes.R` so their specifications stay in lockstep. The limits + mechanism sections are narrated chronologically over Studies 1–2 before Study 3 is introduced, so they use the S1+S2-only fit; the canvasser-inclusion S1–S3 refit lives in the SI.
- `_data/` is a cache, not a deliverable; safe to delete (rebuilt on next `load_study()`).

## Data flow / analytical sample

`data/` is the single canonical dataset (see
`data/README.md` for the full data-flow discussion). Each row is
one randomized persuadee session, with per-row flags
`successfully_matched`, `attrited`, and the raw `pre_attitude` /
`post_attitude` (NA on missingness). Three CONSORT-style sample layers
flow out of these flags:

| Layer | Definition | Used by |
|---|---|---|
| Assigned | every data row | `si_03_attrition.R` (pre-treatment dropout denominator) |
| Matched | `successfully_matched == TRUE` | `si_03_attrition.R` (post-treatment attrition denominator) |
| Complete case | matched **and** `!attrited` **and** non-NA `pre/post_attitude` | every outcome script (`01`–`04`, `si_01`, `si_02`, `si_06`–`si_11`, `99`) |

`_setup.R::complete_case_filter()` is the single canonical implementation
of the matched → complete-case step. Outcome scripts call it once at the
top and print an audit line:

```
[<label>] N assigned = N0 -> matched = N1 (-D dropouts)
                          -> CC = N2 (-A attrited, -P pre NA, -Q post NA)
```

`si_03_attrition.R` deliberately bypasses `complete_case_filter()` and
filters to `successfully_matched` directly, because attrition is its
outcome variable. `_load_data.R` returns the raw layer-0 tibble so each
script makes its own sample choice explicitly.

## Running

```bash
make results          # all analysis scripts in dependency order (99 last)
```

Or run a single script directly, e.g. `Rscript code/analysis/01_main_attitudes.R`
(note: `99_extract_numbers.R` requires every other analysis script to have
run first, since it reads their RDS).
