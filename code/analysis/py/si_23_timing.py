#!/usr/bin/env python3
"""si_23_timing.py

Per-session timestamps for the matchmaking and persuader-order analyses
(code/analysis/si_23_matching_order.R). One row per Studies 1-3 persuadee
session (matched or not) with
  session_started_at   persuadee session start (before the pre-survey)
  conv_started_at      conversation start (after the pre-survey and, for
                       human conditions, the matchmaking queue)
  conv_ended_at        conversation end
  start_gap_min        conv_started_at - session_started_at, in minutes;
                       pre-survey time plus, for human conditions, the wait
                       for a partner
Joined to the R loader on row_id.

Usage:
  python3 code/analysis/py/si_23_timing.py
Writes output/results/_py/si_23_timing.csv.
"""

from __future__ import annotations

import csv
import json
from datetime import datetime
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
DATA = ROOT / "data"
OUT = ROOT / "output/results/_py"

STUDIES = ["study_1", "study_2", "study_3"]


def parse_ts(x: str | None) -> datetime | None:
    if not x:
        return None
    x = x.replace("T", " ").replace("Z", "")
    for fmt in ("%Y-%m-%d %H:%M:%S.%f", "%Y-%m-%d %H:%M:%S"):
        try:
            return datetime.strptime(x[:26], fmt)
        except ValueError:
            continue
    return None


def main() -> None:
    OUT.mkdir(parents=True, exist_ok=True)
    out_path = OUT / "si_23_timing.csv"
    fields = ["row_id", "session_started_at", "conv_started_at", "conv_ended_at", "start_gap_min"]
    n = 0
    with out_path.open("w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=fields)
        w.writeheader()
        for study in STUDIES:
            with (DATA / f"{study}.jsonl").open() as f:
                for line in f:
                    r = json.loads(line)
                    conv = r.get("conversation") or {}
                    s0 = parse_ts(r["persuadee"].get("session_started_at"))
                    c0 = parse_ts(conv.get("started_at"))
                    c1 = parse_ts(conv.get("ended_at"))
                    gap = (c0 - s0).total_seconds() / 60 if (s0 and c0) else None
                    w.writerow({
                        "row_id": r["row_id"],
                        "session_started_at": s0.isoformat() if s0 else "",
                        "conv_started_at": c0.isoformat() if c0 else "",
                        "conv_ended_at": c1.isoformat() if c1 else "",
                        "start_gap_min": f"{gap:.3f}" if gap is not None else "",
                    })
                    n += 1
    print(f"wrote {n} rows -> {out_path}")


if __name__ == "__main__":
    main()
