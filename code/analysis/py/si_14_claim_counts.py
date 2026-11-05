#!/usr/bin/env python3
"""si_14_claim_counts.py

Per-conversation counts of accurate and inaccurate persuader claims, Studies
1-2, fact-checked conversations only (fact_check_summary.checked). A claim is
accurate if its veracity_score exceeds 50 (as in si_09_fact_accuracy.R);
claims with no veracity score are dropped.

Usage:
  python3 code/analysis/py/si_14_claim_counts.py
Writes output/results/_py/si_14_claim_counts.csv
  -> code/analysis/si_14_claim_accuracy.R
"""

from __future__ import annotations

import csv
import json
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
DATA = ROOT / "data"
RESULTS_PY = ROOT / "output/results/_py"
ACCURATE_THRESHOLD = 50


def main() -> None:
    RESULTS_PY.mkdir(parents=True, exist_ok=True)
    rows = []
    for study in ("study_1", "study_2"):
        with open(DATA / f"{study}.jsonl") as f:
            for line in f:
                r = json.loads(line)
                if not (r.get("fact_check_summary") or {}).get("checked"):
                    continue
                scores = [c["veracity_score"]
                          for m in r["conversation"]["messages"] if m["speaker"] == "persuader"
                          for c in ((m.get("facts") or {}).get("claims") or [])
                          if c.get("veracity_score") is not None]
                n_acc = sum(s > ACCURATE_THRESHOLD for s in scores)
                rows.append({"row_id": r["row_id"], "n_claims": len(scores),
                             "n_accurate": n_acc, "n_inaccurate": len(scores) - n_acc})
    with open(RESULTS_PY / "si_14_claim_counts.csv", "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)
    print(f"{len(rows)} fact-checked conversations; "
          f"{sum(r['n_claims'] for r in rows)} scored claims")


if __name__ == "__main__":
    main()
