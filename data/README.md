# data/

The de-identified, analysis-ready dataset for the project: cleaned JSONL files
for the five analysed study slices, plus the per-claim fact-check CSVs. Every
result, figure, table, and inline number in the manuscript and SI is derived
from the files in this folder.

This is a privacy-preserving export: it contains only the fields the analysis
pipeline reads, with participant/session identifiers pseudonymised and free
text removed or reduced to its parsed form. See the *Data and de-identification*
section of the top-level `README.md` and `code/redact_data.R` for the exact
transformation.

## Files

| File | Description |
|---|---|
| `study_{0,1,2,3,4}.jsonl` | Cleaned conversations for each study (one row per persuadee-side conversation), restricted to analysis fields. |
| `fact_checks/study_{1..4}/<role>_MAIN_fact_checks.csv` | Per-claim fact-check output (one row per extracted claim), reduced to the pseudonymous `chat_session_id` and the 0-100 `veracity_score` read by `code/analysis/si_09_fact_accuracy.R`. Per-session claim counts and mean veracity are already embedded in the `study_X.jsonl` rows. |

Each `study_X.jsonl` row is one randomized persuadee session. Subsetting to the
right analytical sample is done at analysis time using explicit per-row flags,
never by forking the dataset into multiple files.

## Analytical sample (CONSORT-style)

Three nested sample layers are computed from per-row Boolean flags:

| Layer | Definition | Used by |
|---|---|---|
| **Assigned** | every row | `si_03_attrition.R` (pre-treatment dropout denominator) |
| **Matched** | `successfully_matched == TRUE` | `si_03_attrition.R` (post-treatment attrition denominator) |
| **Complete case** | matched **and** `!attrited` **and** non-NA `pre_attitude` / `post_attitude` | every outcome analysis (`01`-`04`, `si_01`, `si_02`, `si_06`-`si_11`, `99`) |

`code/analysis/_setup.R::complete_case_filter()` is the single canonical
implementation of the matched -> complete-case step. Every outcome script calls
it and prints a per-call audit line:

```
[<label>] N assigned = N0 -> matched = N1 (-D dropouts)
                          -> CC = N2 (-A attrited, -P pre NA, -Q post NA)
```

### Key flags

- `successfully_matched` — the participant cleared pre-treatment / matchmaking
  and was exposed to the assigned condition (human arms: the persuader-side
  chat resolved; AI / control arms: the persuadee reached the post-treatment
  phase).
- `attrited` — `TRUE` iff the participant did not provide a valid
  post-treatment attitude.

### Why one dataset, not many

Different questions need different denominators: pre-treatment dropout rates
need every assigned row, post-treatment attrition needs the matched subset, and
the outcome models need the complete-case subset. A pre-filtered
"analysis-only" dataset would make dropout and attrition rates uncomputable.
Keeping one canonical dataset plus typed filters is the standard CONSORT /
ICH-E9 single-source-of-truth pattern.

## Per-study conversation counts

| Study | Conversations |
|---|---:|
| `study_0` (selection tournament) | 9,475 |
| `study_1` | 11,755 |
| `study_2` | 4,955 |
| `study_3` | 2,930 |
| `study_4` | 3,803 |
