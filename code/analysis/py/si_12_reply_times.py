#!/usr/bin/env python3
"""si_12_reply_times.py

Human persuader reply times and the share of each conversation spent
awaiting the persuader's reply (SI Table "Conversation descriptive
statistics").

From per-message timestamps, each conversation's duration is split into:
  persuader time  gaps ending in a persuader message (reading + writing the
                  reply; the opening message is timed from started_at)
  persuadee time  gaps ending in a persuadee message
  tail            last message -> end (remainder of duration_seconds)
Sample and duration match the table: matched, non-attrited, >= 1 persuader
message, duration_seconds > 0.
reply_gap_s is the message-level mean over persuader replies, excluding the
opening message (pre-written, sent at start). persuader_share is mean
persuader time over mean duration.
AI messages are timestamped server-side, so only human persuader classes are
reported.

Output (consumer after `->`):
  output/results/_py/si_12_reply_times.csv -> si_12_conversation_descriptives.R
"""

from __future__ import annotations

import csv
import json
import statistics
from datetime import datetime
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
DATA = ROOT / "data"
RESULTS_PY = ROOT / "output/results/_py"

ARMS = [
    ("study_1", "random_lay_person"),
    ("study_1", "elite_lay_person"),
    ("study_1", "elite_debater"),
    ("study_2", "coached_elite_debater"),
    ("study_3", "canvasser"),
    ("study_4", "canvasser"),
]


def ts(s: str) -> float:
    return datetime.fromisoformat(s.replace("Z", "+00:00")).timestamp()


def complete(r: dict) -> bool:
    p, c = r["persuadee"], r["conversation"]
    return (bool(p.get("successfully_matched")) and not p.get("attrited")
            and (c["message_stats"].get("n_persuader_messages") or 0) > 0
            and (c.get("duration_seconds") or 0) > 0)


def split(r: dict) -> dict | None:
    c = r["conversation"]
    msgs = sorted((m for m in c["messages"] if m.get("created_at")), key=lambda m: m["created_at"])
    if not msgs or not c.get("started_at"):
        return None
    start = ts(c["started_at"])
    prev, er, ee, replies = start, 0.0, 0.0, []
    for i, m in enumerate(msgs):
        t = ts(m["created_at"])
        gap = max(t - prev, 0.0)
        if m["speaker"] == "persuader":
            er += gap
            if i > 0:
                replies.append(gap)
        else:
            ee += gap
        prev = t
    dur = c["duration_seconds"]
    return {
        "duration_min": dur / 60,
        "persuader_min": er / 60,
        "persuadee_min": ee / 60,
        "tail_min": max(dur - (prev - start), 0.0) / 60,
        "n_persuader_msgs": sum(m["speaker"] == "persuader" for m in msgs),
        "n_persuadee_msgs": sum(m["speaker"] == "persuadee" for m in msgs),
        "replies": replies,
    }


def main() -> None:
    RESULTS_PY.mkdir(parents=True, exist_ok=True)
    rows = []
    for study, cls in ARMS:
        convs = []
        with open(DATA / f"{study}.jsonl") as f:
            for line in f:
                r = json.loads(line)
                if r["treatment"]["persuader_class"] == cls and complete(r):
                    x = split(r)
                    if x:
                        convs.append(x)
        row = {"study": study, "persuader_class": cls, "n_conv": len(convs)}
        for k in convs[0]:
            if k != "replies":
                row[k] = statistics.mean(x[k] for x in convs)
        row["reply_gap_s"] = statistics.mean(g for x in convs for g in x["replies"])
        row["persuader_share"] = row["persuader_min"] / row["duration_min"]
        rows.append(row)
        print(f"{study} {cls}: n={row['n_conv']}  reply {row['reply_gap_s']:.1f} s  "
              f"awaiting {row['persuader_share']:.3f}")

    out = RESULTS_PY / "si_12_reply_times.csv"
    with open(out, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)
    print(f"wrote {out}")


if __name__ == "__main__":
    main()
