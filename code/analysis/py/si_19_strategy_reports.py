#!/usr/bin/env python3
"""si_19_strategy_reports.py

Human persuaders' post-conversation strategy self-reports (nine 0-100 items,
SI "Persuader post-conversation assessment"), Studies 1-3, one row per
conversation with at least one rated item.

Usage:
  python3 code/analysis/py/si_19_strategy_reports.py
Writes output/results/_py/si_19_strategy_reports.csv
  -> code/analysis/si_19_persuader_strategies.R
"""

from __future__ import annotations

import csv
import json
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
DATA = ROOT / "data"
RESULTS_PY = ROOT / "output/results/_py"

STRATEGIES = ("information", "storytelling", "deep_canvassing", "moral_reframing",
              "norms", "debate", "mega", "deception", "none")


def main() -> None:
    RESULTS_PY.mkdir(parents=True, exist_ok=True)
    rows = []
    for study in ("study_1", "study_2", "study_3"):
        with open(DATA / f"{study}.jsonl") as f:
            for line in f:
                r = json.loads(line)
                post = ((r.get("persuader") or {}).get("persuader_post_conversation") or {})
                vals = {s: post.get(f"strategy_{s}") for s in STRATEGIES}
                if all(v is None for v in vals.values()):
                    continue
                rows.append({"row_id": r["row_id"], **{f"strat_{s}": vals[s] for s in STRATEGIES}})
    with open(RESULTS_PY / "si_19_strategy_reports.csv", "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)
    print(f"{len(rows)} conversations with strategy reports")


if __name__ == "__main__":
    main()
