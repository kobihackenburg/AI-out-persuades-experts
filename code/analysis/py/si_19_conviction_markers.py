#!/usr/bin/env python3
"""si_19_conviction_markers.py

Density per 100 persuader words of the three metadiscourse markers of
conviction used by Schoenegger et al. (2026): hedges (Hyland, 2005, via
Vazquez & Giner, 2008), boosters and maximizers (Alshaar, 2017), per
persuader conversation in Studies 1-2. Word lists are copied verbatim from
their Appendix E. Matching is case-insensitive and whole-word; a multi-word
phrase counts as one marker.

Usage:
  python3 code/analysis/py/si_19_conviction_markers.py
Writes output/results/_py/si_19_conviction_markers.csv
  -> code/analysis/si_19_conviction_markers.R
"""

from __future__ import annotations

import csv
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
DATA = ROOT / "data"
RESULTS_PY = ROOT / "output/results/_py"

HEDGES = [
    "about", "almost", "apparent", "apparently", "appear", "appeared", "appears", "approximately", "argue",
    "argued", "argues", "around", "assume", "assumed", "broadly", "certain amount", "certain extent",
    "certain level", "claim", "claimed", "claims", "could", "couldn't", "doubt", "doubtful", "essentially",
    "estimate", "estimated", "fairly", "feels", "felt", "frequently", "from my perspective", "from our perspective",
    "from this perspective", "generally", "guess", "indicate", "indicated", "indicates", "in general",
    "in most cases", "in most instances", "in my opinion", "in my view", "in this view", "in our view", "largely",
    "likely", "mainly", "may", "maybe", "might", "mostly", "often", "on the whole", "ought", "perhaps",
    "plausibly", "possible", "possibly", "postulate", "postulated", "postulates", "presumably", "probable",
    "probably", "quite", "rather", "relatively", "roughly", "seems", "should", "sometimes", "somewhat",
    "suggest", "suggested", "suggests", "suppose", "supposed", "supposes", "suspect", "suspects", "tend to",
    "tended to", "tends to", "to my knowledge", "typical", "typically", "uncertain", "uncertainly", "unclear",
    "unclearly", "unlikely", "usually", "would", "wouldn't",
]
BOOSTERS = ["deeply", "heavily", "highly", "increasingly", "largely", "much", "really", "right",
            "significantly", "so", "strongly", "too", "very", "well", "widely"]
MAXIMIZERS = ["absolutely", "completely", "entirely", "extremely", "fully", "perfectly", "totally", "ultimately"]


def pattern(words: list[str]) -> re.Pattern:
    alts = sorted((re.escape(w).replace(r"\ ", r"\s+") for w in words), key=len, reverse=True)
    return re.compile(r"(?<![\w'])(?:" + "|".join(alts) + r")(?![\w'])", re.I)


PATTERNS = {"hedges": pattern(HEDGES), "boosters": pattern(BOOSTERS), "maximizers": pattern(MAXIMIZERS)}
WORD_RE = re.compile(r"[A-Za-z0-9']+")


def main() -> None:
    RESULTS_PY.mkdir(parents=True, exist_ok=True)
    rows = []
    for study in ("study_1", "study_2"):
        with open(DATA / f"{study}.jsonl") as f:
            for line in f:
                r = json.loads(line)
                if r["treatment"]["persuader_class"] in ("control", "none"):
                    continue
                text = "\n".join((m.get("content") or "") for m in r["conversation"]["messages"]
                                 if m["speaker"] == "persuader")
                n_words = len(WORD_RE.findall(text))
                if n_words == 0:
                    continue
                row = {"row_id": r["row_id"], "persuader_words": n_words}
                for k, rx in PATTERNS.items():
                    row[f"{k}_per100"] = 100 * len(rx.findall(text)) / n_words
                rows.append(row)
    with open(RESULTS_PY / "si_19_conviction_markers.csv", "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)
    print(f"{len(rows)} conversations scored")


if __name__ == "__main__":
    main()
