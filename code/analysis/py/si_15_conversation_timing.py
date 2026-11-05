#!/usr/bin/env python3
"""si_15_conversation_timing.py

Conversation-level message length and timing for Studies 1-2, read by
code/analysis/si_15_length_timing.R (SI sections on message length, reply
time and persuasive impact, and on waiting time under the constraint).

For each conversation with at least one persuadee reply to a persuader
message, records:
  mean_persuader_msg_words  mean words per persuader message
  median_reply_s            median seconds from a persuader message to the
                            persuadee's reply (reading + thinking + writing)
  median_reply_words        median words in the persuadee's replies
  n_replies
  median_wait_s             median seconds from a persuadee message to the
                            persuader's next message (exact only for human
                            persuaders, ~0 for AI by logging)
  median_cycle_s            median seconds from a persuadee message to the
                            persuadee's next message (total time per
                            exchange; for Constrained AI = drawn delay +
                            reading + reply)

AI `created_at` is logged when the persuadee's previous message arrives, so in
AI conversations the reply time also includes AI generation time (and, for
Constrained AI, the imposed delay). Human persuader timestamps are exact.

Usage:
  python3 code/analysis/py/si_15_conversation_timing.py
Writes output/results/_py/si_15_conversation_timing.csv.
"""

from __future__ import annotations

import csv
import json
import statistics
from datetime import datetime
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
DATA = ROOT / "data"
OUT = ROOT / "output/results/_py"
STUDIES = ["study_1", "study_2"]


def ts(s: str) -> float:
    return datetime.fromisoformat(s.replace("Z", "+00:00")).timestamp()


def main() -> None:
    OUT.mkdir(parents=True, exist_ok=True)
    rows = []
    for study in STUDIES:
        with open(DATA / f"{study}.jsonl") as f:
            for line in f:
                r = json.loads(line)
                msgs = (r.get("conversation") or {}).get("messages") or []
                if not msgs:
                    continue
                msgs = sorted(msgs, key=lambda m: ts(m["created_at"]))
                persuader_words = [int(m["n_words"] or 0) for m in msgs if m["speaker"] == "persuader"]
                replies = [
                    (ts(m["created_at"]) - ts(prev["created_at"]), int(m["n_words"] or 0))
                    for prev, m in zip(msgs, msgs[1:])
                    if prev["speaker"] == "persuader" and m["speaker"] == "persuadee"
                ]
                if not persuader_words or not replies:
                    continue
                waits = [
                    ts(m["created_at"]) - ts(prev["created_at"])
                    for prev, m in zip(msgs, msgs[1:])
                    if prev["speaker"] == "persuadee" and m["speaker"] == "persuader"
                ]
                persuadee_ts = [ts(m["created_at"]) for m in msgs if m["speaker"] == "persuadee"]
                cycles = [b - a for a, b in zip(persuadee_ts, persuadee_ts[1:])]
                rows.append({
                    "persuadee_session": r["persuadee"]["session_id"],
                    "study": study,
                    "mean_persuader_msg_words": statistics.mean(persuader_words),
                    "median_reply_s": statistics.median(x[0] for x in replies),
                    "median_reply_words": statistics.median(x[1] for x in replies),
                    "n_replies": len(replies),
                    "median_wait_s": statistics.median(waits) if waits else "",
                    "median_cycle_s": statistics.median(cycles) if cycles else "",
                })

    path = OUT / "si_15_conversation_timing.csv"
    with open(path, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)
    print(f"  wrote {path} ({len(rows)} conversations)")


if __name__ == "__main__":
    main()
