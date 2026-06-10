# Elite-debater achievement annotation

This is the one out-of-pipeline analysis step. It codes the 56 Study 1 Elite
Debaters' free-text `debating-achievements` responses against a fixed rubric
using an LLM, producing the cohort descriptors (`debater.*` keys) the
manuscript cites in the Elite Debater paragraph and Methods.

It lives **outside `make all`** because a live re-run calls the Anthropic API
(needs a key). `make all` never calls the API: `99_extract_numbers.R` simply
reads the committed `audit.csv` here.

## Files

| File | Role |
| --- | --- |
| `annotate.py` | The annotation script (live API or `--from-cache`). |
| `rubric.md` | The verbatim coding rubric, embedded into the LLM prompt. |
| `input.json` | One entry per debater (`persuader_id`, raw years + achievements text). Derived from the Study 1 persuader pre-survey; the canonical input. |
| `annotations.json` | Canonical LLM output: per-criterion TRUE/FALSE + evidence. **Source of truth** for the cohort claims. |
| `audit.csv` | Flat CSV derived from `annotations.json`; read by `code/analysis/99_extract_numbers.R`. |

In the public de-identified release, `input.json` is withheld and the
`evidence` fields in `annotations.json` are blanked (both contain raw
free-text); the per-criterion TRUE/FALSE codings and `audit.csv` are unchanged,
so `99_extract_numbers.R` and `--from-cache` still reproduce every cohort claim.

## Reproducing

No API access (rebuild `audit.csv` from the committed annotations):

```bash
python3 code/analysis/elite_debater/annotate.py --from-cache
```

Live re-annotation (only needed if the rubric changes):

```bash
export ANTHROPIC_API_KEY=sk-...
python3 code/analysis/elite_debater/annotate.py
```

The model is pinned in `annotate.py` (`temperature=0`) for reproducibility.
Any rubric change must be followed by a live re-run so `annotations.json` and
`audit.csv` stay consistent with `rubric.md`.
