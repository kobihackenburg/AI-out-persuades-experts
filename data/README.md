# data/

The de-identified, analysis-ready dataset for the project: cleaned JSONL files
for the five analysed study slices, the per-claim fact-check CSVs, the
follow-up survey, the inputs derived from message text, the example
conversations and the Reddit scores. Every result, figure, table, and inline
number in the manuscript and SI is derived from the files in this folder.

**The conversation transcripts are not included** (personal data, and dual-use risk; see *Conversation transcripts* in
the top-level `README.md`). Measures computed from them ship as computed.

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
| `final_followup_data/followup_blocks.csv` | One row per follow-up block (a persuadee re-surveyed about one issue): renumbered `block_id`, the pseudonymous `row_id` of the conversation (single-exposure blocks only; it matches `row_id` in `study_X.jsonl`), `exposure_pattern`, `skipped` and the timestamps used for the time since the conversation. Read by `code/analysis/si_13_followup.R`. |
| `final_followup_data/followup_items.csv` | The block's responses, long format (`block_id`, `item_id`, `value_raw`): the three attitude items (Studies 1-3) or the donation amount (Study 4). |
| `derived/*.csv`, `derived/tab_example_conversations.tex` | Outputs of `code/analysis/py/*.py`, which read message text (claim counts, reply and conversation timing, conviction markers, strategy reports, the example-conversation draw). Keys (`row_id`, `persuadee_session`) are pseudonymised. `make results` copies them to `output/results/_py/` and `output/tables/si/`. |
| `example_conversations/` | The 24 example transcripts from the SI (`transcripts/*.txt`) and their `manifest.csv` (study, persuader, issue and statement, attitudes before and after, donation outcomes for Study 4, pseudonymous `row_id`). |
| `reddit/` | Pangram scores for 1,000 r/PoliticalDiscussion posts, without post text; see `reddit/README.md`. |

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

## Follow-up survey

In September 2026 all persuadees from Studies 1-4 were invited to a follow-up
survey (see the manuscript's Methods). `si_13_followup.R` analyses blocks with
`exposure_pattern == "single"` (the persuadee discussed that issue in exactly
one conversation) that were not skipped; blocks for issues discussed more than
once carry no `row_id`, since they cannot be tied to one conversation.
