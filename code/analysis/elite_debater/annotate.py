#!/usr/bin/env python3
"""LLM-driven annotation of Study 1 elite-debater achievements.

All inputs and outputs live beside this script in `code/analysis/elite_debater/`
(committed to the repo, not in the deletable `output/` cache).

Reads `code/analysis/elite_debater/input.json` (one entry per debater with raw
`debating_achievements` and `debating_years`) and produces:

  * `code/analysis/elite_debater/annotations.json` -- per-debater LLM output with
    each rubric criterion as a TRUE/FALSE plus a one-line justification quoting
    the relevant fragment of the response.
  * `code/analysis/elite_debater/audit.csv` -- flat CSV (one row per debater)
    consumed by `code/analysis/99_extract_numbers.R` to emit the `debater.*` keys in
    `_numbers.json`.

The rubric lives in `code/analysis/elite_debater/rubric.md` and is embedded into
the LLM system prompt below so the script is self-contained.

This step is intentionally OUTSIDE `make all`: a live re-run needs an API key,
and `make all` simply reads the committed `audit.csv`.

Reproducibility
---------------

Two ways to (re)run this script:

  1. Live LLM annotation (default):
       export ANTHROPIC_API_KEY=sk-...
       python3 code/analysis/elite_debater/annotate.py

     Calls the Anthropic API per debater with `temperature=0` and writes both
     the annotations JSON and the audit CSV.

  2. Cache-only (no API call needed):
       python3 code/analysis/elite_debater/annotate.py --from-cache

     Reads the committed annotations JSON and rebuilds the audit CSV. Use this
     if you are reproducing the manuscript numbers without API access.

The committed annotations JSON is the canonical source of truth for the
manuscript's elite-debater cohort claims. Any change to the rubric requires a
re-run of the live LLM annotation.
"""

from __future__ import annotations

import argparse
import csv
import json
import os
import sys
from pathlib import Path
from typing import Any

# All inputs/outputs live beside this script (code/analysis/elite_debater/), so the
# committed annotations are version-controlled rather than buried in the
# deletable output/results/_data cache.
HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
INPUT_PATH = HERE / "input.json"
ANN_PATH = HERE / "annotations.json"
CSV_PATH = HERE / "audit.csv"
RUBRIC_PATH = HERE / "rubric.md"

CRITERIA = [
    "is_world_champion",
    "is_continental_champion",
    "is_continental_champion_any_division",
    "reached_continental_semis_or_better",
    "reached_major_semis_or_better",
    "is_best_speaker_at_major",
    "is_continental_finalist",
]

MODEL = "claude-opus-4-5-20250929"  # frozen for reproducibility

PROMPT_TEMPLATE = """\
You are coding a single elite competitive debater's self-reported achievements
against a fixed rubric. Apply the rubric strictly: the bar is "explicit
self-report"; do not infer titles the debater did not claim.

==== RUBRIC (verbatim) ====
{rubric}
==== END RUBRIC ====

==== DEBATER FREE-TEXT RESPONSE ====
persuader_id: {persuader_id}
debating-years (raw): {years_raw!r}
debating-achievements (raw):
{achievements_raw}
==== END RESPONSE ====

Return ONLY a single JSON object (no preamble, no code fences, no commentary)
with these exact keys:

{{
  "persuader_id": "{persuader_id}",
  "is_world_champion": <true|false>,
  "is_world_champion_evidence": "<<=160 char justification quoting the response>",
  "is_continental_champion": <true|false>,
  "is_continental_champion_evidence": "<<=160 char justification>",
  "is_continental_champion_any_division": <true|false>,
  "is_continental_champion_any_division_evidence": "<<=160 char justification>",
  "reached_continental_semis_or_better": <true|false>,
  "reached_continental_semis_or_better_evidence": "<<=160 char justification>",
  "reached_major_semis_or_better": <true|false>,
  "reached_major_semis_or_better_evidence": "<<=160 char justification>",
  "is_best_speaker_at_major": <true|false>,
  "is_best_speaker_at_major_evidence": "<<=160 char justification>",
  "is_continental_finalist": <true|false>,
  "is_continental_finalist_evidence": "<<=160 char justification>"
}}
"""


def load_rubric() -> str:
    if not RUBRIC_PATH.exists():
        sys.exit(f"Rubric not found: {RUBRIC_PATH}")
    return RUBRIC_PATH.read_text(encoding="utf-8")


def load_input() -> list[dict[str, Any]]:
    if not INPUT_PATH.exists():
        sys.exit(f"Input not found: {INPUT_PATH}")
    payload = json.loads(INPUT_PATH.read_text(encoding="utf-8"))
    return payload["debaters"]


def annotate_one_via_api(client, rubric: str, row: dict[str, Any]) -> dict[str, Any]:
    """Single Anthropic API call returning the parsed JSON annotation."""
    prompt = PROMPT_TEMPLATE.format(
        rubric=rubric,
        persuader_id=row["persuader_id"],
        years_raw=row.get("years_raw", ""),
        achievements_raw=row.get("achievements_raw", ""),
    )
    msg = client.messages.create(
        model=MODEL,
        max_tokens=1500,
        temperature=0,
        messages=[{"role": "user", "content": prompt}],
    )
    text = "".join(b.text for b in msg.content if hasattr(b, "text")).strip()
    if text.startswith("```"):
        text = text.strip("`").lstrip("json").strip()
    return json.loads(text)


def run_live_annotation(debaters: list[dict[str, Any]]) -> list[dict[str, Any]]:
    try:
        import anthropic
    except ImportError:
        sys.exit(
            "anthropic package not installed. Either run with --from-cache or "
            "install it: pip install anthropic"
        )
    if "ANTHROPIC_API_KEY" not in os.environ:
        sys.exit(
            "ANTHROPIC_API_KEY not set. Either export it or run with "
            "--from-cache to reuse committed annotations."
        )
    rubric = load_rubric()
    client = anthropic.Anthropic()
    annotations = []
    for i, row in enumerate(debaters, 1):
        print(f"  [{i:2d}/{len(debaters)}] annotating {row['persuader_id'][:10]}...", file=sys.stderr)
        ann = annotate_one_via_api(client, rubric, row)
        annotations.append(ann)
    return annotations


def load_cached() -> list[dict[str, Any]]:
    if not ANN_PATH.exists():
        sys.exit(
            f"Cached annotations not found at {ANN_PATH}. Run without "
            "--from-cache (and with ANTHROPIC_API_KEY set) to generate them."
        )
    return json.loads(ANN_PATH.read_text(encoding="utf-8"))["annotations"]


def write_annotations(annotations: list[dict[str, Any]]) -> None:
    payload = {
        "model": MODEL,
        "rubric_path": str(RUBRIC_PATH.relative_to(ROOT)),
        "n_debaters": len(annotations),
        "criteria": CRITERIA,
        "annotations": annotations,
    }
    ANN_PATH.write_text(json.dumps(payload, indent=2, ensure_ascii=False), encoding="utf-8")
    print(f"  wrote {len(annotations)} annotations to {ANN_PATH}", file=sys.stderr)


def write_audit_csv(annotations: list[dict[str, Any]]) -> None:
    fieldnames = ["persuader_id"]
    for c in CRITERIA:
        fieldnames.append(c)
        fieldnames.append(f"{c}_evidence")
    with CSV_PATH.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        for ann in sorted(annotations, key=lambda r: r["persuader_id"]):
            row = {"persuader_id": ann["persuader_id"]}
            for c in CRITERIA:
                row[c] = "TRUE" if bool(ann.get(c)) else "FALSE"
                row[f"{c}_evidence"] = ann.get(f"{c}_evidence", "")
            writer.writerow(row)
    print(f"  wrote audit CSV ({len(annotations)} rows) to {CSV_PATH}", file=sys.stderr)


def main() -> None:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--from-cache", action="store_true",
                   help="Skip API call; read committed annotations and rebuild CSV.")
    args = p.parse_args()

    if args.from_cache:
        annotations = load_cached()
    else:
        debaters = load_input()
        annotations = run_live_annotation(debaters)
        write_annotations(annotations)

    write_audit_csv(annotations)


if __name__ == "__main__":
    main()
