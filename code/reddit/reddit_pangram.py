#!/usr/bin/env python3
"""Reproduce the r/PoliticalDiscussion Pangram figures from the committed files, after checking them end to end.

Backs the SI section on AI-generated content in r/PoliticalDiscussion (cited in the Introduction). Data and
provenance: `data/reddit/` (see its README). The post text there must not be redistributed (Reddit's terms); the
public release carries post ids and Pangram scores only.

In a random sample of 1,000 r/PoliticalDiscussion posts from the past twelve months, Pangram estimates that 36% are
at least 80% AI-generated. This script derives that and the sensitivity figures from the two data files in
`data/reddit/` (the seed-42 sample of 1,000 posts and their Pangram scores) and writes `results.json` and
`results.md` to `output/results/reddit/`. It needs no network access and
no third-party packages.

Before computing anything it verifies the files, and aborts if any check fails:
  1. the sample file holds 1,000 posts with unique ids, all from the subreddit, all inside the twelve-month window
     its run record describes, all with scoreable text, drawn with seed 42 from the 1,197 posts with text among the
     10,348 scraped (the counts in the run record);
  2. re-cleaning the committed text changes nothing (except the documented shortening of underscore rules);
  3. every sampled post has one Pangram result, for exactly the text the pipeline sends, with internally consistent
     scores (fractions sum to one; `fraction_ai` is the character-weighted share of AI windows);
  4. the headline numbers equal the published ones.

With `--posts PATH` pointing at the 12-month scrape the sample was drawn from (kept in the p4_pangram repository as
`data/politicaldiscussion_posts_12mo_clean.jsonl`, not shipped here), it also re-draws the seed-42 sample with
`pangram_pipeline.sample_rows` and checks that it is exactly the committed sample, and reports why the other posts
had no text.

The cleaning, eligibility, sampling and text rules are imported from `pangram_pipeline.py`, so there is one
definition of each.

The public release ships `<stem>.scores.jsonl` (the Pangram results with the text removed from every window, written
by `--export-public DIR` once every check has passed) and the two run records. When the sample file is absent and that
file is present, the script runs the checks that need no text (1 without the text conditions, and 3 without the
comparison to the sent text) and computes the same numbers.

Usage: python3 code/reddit/reddit_pangram.py [--data DIR] [--out DIR] [--posts PATH] [--export-public DIR]
"""

from __future__ import annotations

import argparse
import json
import math
import re
import statistics
import sys
from collections import Counter, defaultdict
from datetime import datetime, timedelta, timezone
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from pangram_pipeline import (  # noqa: E402
    WORDS_PER_UNIT,
    clean_row,
    item_text,
    read_rows,
    row_skip_reason,
    sample_rows,
)

ROOT = Path(__file__).resolve().parents[2]
DATA_DIR = ROOT / "data" / "reddit"
OUT_DIR = ROOT / "output" / "results" / "reddit"
SUBREDDIT = "PoliticalDiscussion"
SAMPLE_SIZE = 1000
SEED = 42
SAMPLE_STEM = f"politicaldiscussion_posts_12mo_clean_sample{SAMPLE_SIZE}_seed{SEED}"
# The public release: the Pangram results with the text removed from every window.
PUBLIC_SCORES = f"{SAMPLE_STEM}.scores.jsonl"
AI_THRESHOLD = 0.8
THRESHOLDS = (0.5, 0.8, 0.9, 1.0)
SHORT_TEXT_WORDS = 50
PANGRAM_VERSION = "4.0"

# The numbers as published in the referee responses. The script fails if the committed files no longer give them.
PUBLISHED = {"n_sampled": 1000, "months": 12, "ai_threshold_pct": 80, "share_ai_pct_rounded": 36}
# Known from the audit of the committed files; a change means the data changed.
EXPECTED = {"n_posts": 10348, "n_with_text": 1197, "n_ai": 364}


class CheckFailed(Exception):
    """A verification step failed; the message says which and why."""


def check(condition: bool, message: str) -> None:
    if not condition:
        raise CheckFailed(message)


def read_json(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def parse_iso(value: str) -> datetime:
    return datetime.fromisoformat(value.replace("Z", "+00:00")).astimezone(timezone.utc)


def month_of(created_utc: int) -> str:
    return datetime.fromtimestamp(created_utc, tz=timezone.utc).strftime("%Y-%m")


# --------------------------------------------------------------------------------------------------------------
# Checks
# --------------------------------------------------------------------------------------------------------------


def check_window(meta: dict) -> dict:
    """The scrape window in the run record is twelve whole months; returns its description."""
    since, until = parse_iso(meta["since"]), parse_iso(meta["until"])
    months = (until.year - since.year) * 12 + until.month - since.month
    check(months == PUBLISHED["months"] and since.day == until.day, f"window is not {PUBLISHED['months']} whole months")
    check(until - since in (timedelta(days=365), timedelta(days=366)), "window is not one year")
    check(parse_iso(meta["finished_at"]) >= until, "the scrape finished before the window closed")
    return {
        "since": since.isoformat(),
        "until_exclusive": until.isoformat(),
        "months": months,
        "first_post_at": meta["first_item_at"],
        "last_post_at": meta["last_item_at"],
        "scraped_at": meta["finished_at"],
        "source": meta["source"],
    }


def check_run_record(rows: list[dict], meta: dict) -> dict:
    """The rows (sampled posts, or their scores) are what the run record says; returns the scrape's counts."""
    check(meta["status"] == "complete", f"scrape status is {meta['status']!r}, not complete")
    check(meta["content_type"] == "posts", f"file holds {meta['content_type']!r}, not posts")
    check(meta["subreddit"] == SUBREDDIT, f"file is for r/{meta['subreddit']}, not r/{SUBREDDIT}")
    sampling = meta["sampling"]
    check(sampling["seed"] == SEED, f"sample was drawn with seed {sampling['seed']}, not {SEED}")
    check(sampling["items_sampled"] == SAMPLE_SIZE == len(rows), f"sample has {len(rows)} rows, not {SAMPLE_SIZE}")
    check(
        sampling["items_in_source"] == meta["items_written"] == meta["cleaning"]["items"], "run record counts disagree"
    )
    check(
        sampling["items_in_source"] == EXPECTED["n_posts"], f"drawn from {sampling['items_in_source']} posts, not 10348"
    )
    check(
        sampling["items_eligible"] == EXPECTED["n_with_text"], f"{sampling['items_eligible']} eligible posts, not 1197"
    )

    ids = [row["id"] for row in rows]
    check(len(set(ids)) == len(ids), "duplicate post ids in the sample")
    check(ids == sorted(ids), "sample is not in id order, so not in the scrape's time order")
    check(all(row["subreddit"] == SUBREDDIT for row in rows), "a post is from another subreddit")
    lo, hi = parse_iso(meta["since"]).timestamp(), parse_iso(meta["until"]).timestamp()
    check(all(lo <= row["created_utc"] < hi for row in rows), "a post was created outside the scrape window")
    return {
        "n_in_window": sampling["items_in_source"],
        "n_with_text": sampling["items_eligible"],
        "n_without_text": sampling["items_in_source"] - sampling["items_eligible"],
    }


def check_sample(sample: list[dict], meta: dict) -> dict:
    """The sample file is what its run record says; returns the counts of the scrape it was drawn from."""
    counts = check_run_record(sample, meta)
    check(all("title" in row and "parent_id" not in row for row in sample), "a row is not a post")
    check(all(row_skip_reason(row) is None for row in sample), "a sampled post has no scoreable text")
    check(
        all(row.get("author") in (None, "AutoModerator", "[deleted]") for row in sample),
        "a username survived redaction",
    )
    return counts


def check_redraw(posts_path: Path, sample: list[dict], meta: dict) -> dict:
    """Re-draw the seed-42 sample from the 12-month file and confirm it is the committed sample."""
    rows = read_rows(posts_path)
    check(
        len(rows) == meta["sampling"]["items_in_source"], f"{posts_path} has {len(rows)} rows, not the source's count"
    )
    reasons = Counter(row_skip_reason(row) for row in rows)
    check(reasons[None] == meta["sampling"]["items_eligible"], "eligible count differs from the sample's run record")
    redrawn = sample_rows(rows, SAMPLE_SIZE, SEED)
    check(
        [row["id"] for row in redrawn] == [row["id"] for row in sample],
        "seed-42 draw does not give the committed sample",
    )
    by_id = {row["id"]: row for row in rows}
    for row in sample:
        source = dict(by_id[row["id"]])
        if source.get("author") not in ("AutoModerator", "[deleted]"):
            source["author"] = None  # the shipped sample has usernames removed
        check(source == row, f"sampled post {row['id']} differs from its row in the 12-month file")
    return {
        "posts_file": str(posts_path),
        "n_removed_on_submission": reasons["removed_or_deleted"],
        "n_moderator": reasons["moderator"],
        "n_empty_after_cleaning": reasons["empty"],
        "n_with_text": reasons[None],
        "redraw_identical": True,
    }


def check_recleaning(sample: list[dict]) -> dict:
    """Cleaning the committed text again changes nothing except runs of underscores (horizontal rules)."""
    changed = [row for row in sample if clean_row(row) != row]
    for row in changed:
        again = clean_row(row)
        for key in ("title", "body"):
            check(
                re.sub("_+", "_", row[key]) == re.sub("_+", "_", again[key]),
                f"re-cleaning post {row['id']} changes its {key} beyond underscore runs",
            )
    return {"rows_changed": len(changed), "ids": [row["id"] for row in changed], "only_underscore_runs_differ": True}


def check_scores(scores: list[dict], scores_meta: dict) -> None:
    """One complete, internally consistent Pangram result per sampled post (needs no post text)."""
    check(len(scores) == SAMPLE_SIZE, f"{len(scores)} score rows, not {SAMPLE_SIZE}")
    check(len({score["id"] for score in scores}) == len(scores), "duplicate ids in the scores")
    check(scores_meta["status"] == "complete", "scoring run did not complete")
    sessions = scores_meta["counts"].get("scored", 0) + scores_meta["already_done"]
    check(sessions == SAMPLE_SIZE, "scoring sessions do not add up to the sample")
    check(not scores_meta["pending_tasks"] and not scores_meta["pending_bulks"], "scoring left pending work")
    for score in scores:
        post = score["id"]
        check(score["status"] == "scored" and score["error"] is None, f"post {post} is not scored")
        check(score["kind"] == "post" and score["post_id"] == post, f"post {post} scored as something else")
        check(score["pangram_version"] == PANGRAM_VERSION, f"post {post}: Pangram {score['pangram_version']}")
        windows = score["windows"]
        check(bool(windows), f"post {post} has no windows")
        total = score["fraction_ai"] + score["fraction_ai_assisted"] + score["fraction_human"]
        check(abs(total - 1) < 1e-6, f"post {post}: fractions sum to {total}")
        chars = sum(w["end_index"] - w["start_index"] for w in windows)
        ai_chars = sum(w["end_index"] - w["start_index"] for w in windows if w["label"] == "AI-Generated")
        check(abs(ai_chars / chars - score["fraction_ai"]) < 1e-3, f"post {post}: fraction_ai is not the AI text share")
        segments = score["num_ai_segments"] + score["num_ai_assisted_segments"] + score["num_human_segments"]
        check(segments == len(windows), f"post {post}: segment counts do not match its windows")


def check_scores_match_sample(sample: list[dict], scores: list[dict]) -> None:
    """Each Pangram result is for exactly the text the pipeline sends for its sampled post."""
    by_id = {score["id"]: score for score in scores}
    check(set(by_id) == {row["id"] for row in sample}, "score ids differ from the sample ids")
    squash = re.compile(r"\s+")
    for row in sample:
        score = by_id[row["id"]]
        text = item_text(row, "post")
        check(score["created_utc"] == row["created_utc"], f"post {row['id']} has another timestamp in the scores")
        check(score["word_count"] == len(text.split()), f"post {row['id']}: scored word count differs from its text")
        check(
            squash.sub(" ", "".join(w["text"] for w in score["windows"])).strip() == squash.sub(" ", text).strip(),
            f"post {row['id']}: Pangram's windows do not add up to the text that would be sent",
        )


def export_public(scores: list[dict], data_dir: Path, out_dir: Path) -> None:
    """Write the text-free scores and both run records for the public release."""
    out_dir.mkdir(parents=True, exist_ok=True)
    with (out_dir / PUBLIC_SCORES).open("w", encoding="utf-8") as fh:
        for score in scores:
            row = {**score, "windows": [{k: v for k, v in w.items() if k != "text"} for w in score["windows"]]}
            fh.write(json.dumps(row, ensure_ascii=False) + "\n")
    for name in (f"{SAMPLE_STEM}.meta.json", f"{SAMPLE_STEM}.pangram.meta.json"):
        (out_dir / name).write_bytes((data_dir / name).read_bytes())


# --------------------------------------------------------------------------------------------------------------
# Numbers
# --------------------------------------------------------------------------------------------------------------


def wilson_interval(successes: int, n: int, z: float = 1.959964) -> tuple[float, float]:
    """Wilson score 95% confidence interval for a binomial share."""
    p = successes / n
    denominator = 1 + z * z / n
    centre = (p + z * z / (2 * n)) / denominator
    half = z * math.sqrt(p * (1 - p) / n + z * z / (4 * n * n)) / denominator
    return centre - half, centre + half


def share_block(hits: int, n: int) -> dict:
    low, high = wilson_interval(hits, n)
    return {"n": hits, "share": hits / n, "ci95_wilson": [low, high]}


def nearest_rank(values: list[int], q: float) -> int:
    ordered = sorted(values)
    return ordered[min(len(ordered) - 1, max(0, math.ceil(q * len(ordered)) - 1))]


def statements(
    counts: dict, window: dict, n: int, n_ai: int, pct: int, low: float, high: float, scores: list[dict]
) -> dict:
    """The published sentences and an SI paragraph, with every number filled in from the computed results."""
    since_date = parse_iso(window["since"]).date()
    until_date = parse_iso(window["until_exclusive"]).date() - timedelta(days=1)  # the window's last day
    until_text = f"{until_date.day} {until_date:%B %Y}"
    thr = PUBLISHED["ai_threshold_pct"]
    share = lambda t: 100 * sum(s["fraction_ai"] >= t for s in scores) / n  # noqa: E731
    return {
        "editor_reply_E1": (
            f"In a new analysis of r/PoliticalDiscussion, we also find that {pct}% of a random sample of {n:,} posts "
            f"from the past twelve months are at least {thr}% AI-generated."
        ),
        "referee_reply_R3_2": (
            f"Of the {counts['n_in_window']:,} posts submitted to r/PoliticalDiscussion in the twelve months to "
            f"{until_text}, {counts['n_with_text']:,} were not removed on submission and had text available. In a "
            f"random sample of {n:,} of these, Pangram estimates that {pct}% (95% CI {100 * low:.0f} to "
            f"{100 * high:.0f}%) were at least {thr}% AI-generated (SI Appendix, Section X)."
        ),
        "si_paragraph": (
            f"We collected every post submitted to r/PoliticalDiscussion between {since_date} and {until_date} from "
            f"the Arctic Shift archive of Reddit ({counts['n_in_window']:,} posts). The archive captures each post "
            f"within seconds of submission; {counts['n_without_text']:,} posts had no text available, in almost every "
            f"case because they had already been removed by then, almost certainly by the subreddit's automated "
            f"moderation. That left {counts['n_with_text']:,} posts. We drew a random sample of {n:,} of them (seed "
            f"{SEED}) and sent each post's title and body to Pangram (version {PANGRAM_VERSION}), which labels each "
            f"segment of a text as human-written, AI-assisted or AI-generated and reports the share of the text "
            f"labelled AI-generated. {n_ai} of the {n:,} posts ({pct}%; 95% Wilson CI {100 * low:.1f} to "
            f"{100 * high:.1f}%) were at least {thr}% AI-generated, which is also exactly the set of posts Pangram's "
            f"overall verdict calls AI. The share is {share(0.8):.1f}% with the threshold at {thr}%, {share(0.5):.1f}% "
            f"at 50% and {share(1.0):.1f}% for posts labelled AI-generated in full. Because the sample covers only "
            f"posts that passed the subreddit's submission filter, it describes the posts readers could see, not "
            f"every attempted submission."
        ),
    }


def compute(
    scores: list[dict],
    metas: dict,
    counts: dict,
    window: dict,
    recleaning: dict | None,
    redraw: dict | None,
    source_files: dict,
) -> dict:
    ai = {score["id"] for score in scores if score["fraction_ai"] >= AI_THRESHOLD}
    n = len(scores)
    check(len(ai) == EXPECTED["n_ai"], f"{len(ai)} posts at or above {AI_THRESHOLD}, expected {EXPECTED['n_ai']}")
    pct = round(100 * len(ai) / n)
    check(pct == PUBLISHED["share_ai_pct_rounded"], f"share rounds to {pct}%, not {PUBLISHED['share_ai_pct_rounded']}%")
    check(n == PUBLISHED["n_sampled"], f"{n} scored posts, not {PUBLISHED['n_sampled']}")
    check(round(100 * AI_THRESHOLD) == PUBLISHED["ai_threshold_pct"], "threshold differs from the published one")
    verdict_ai = {score["id"] for score in scores if score["prediction_short"] == "AI"}
    low, high = wilson_interval(len(ai), n)

    by_month: dict[str, list[bool]] = defaultdict(list)
    for score in scores:
        by_month[month_of(score["created_utc"])].append(score["id"] in ai)
    words = [score["word_count"] for score in scores]

    return {
        "generated_at": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "source_files": source_files,
        "window": window,
        "posts": {**counts, "share_with_text": counts["n_with_text"] / counts["n_in_window"]},
        "sample": {
            "n": n,
            "seed": SEED,
            "share_of_posts_with_text": n / counts["n_with_text"],
            "n_scored": sum(score["status"] == "scored" for score in scores),
            "pangram_version": PANGRAM_VERSION,
            "last_scoring_session_finished_at": metas["scores"]["finished_at"],
            "scoring_sessions": {
                "first": metas["scores"]["already_done"],
                "second": metas["scores"]["counts"]["scored"],
            },
            "billable_units": sum(max(1, math.ceil(score["word_count"] / WORDS_PER_UNIT)) for score in scores),
        },
        "headline": {
            "ai_threshold": AI_THRESHOLD,
            "n_ai": len(ai),
            "share_ai": len(ai) / n,
            "share_ai_pct_rounded": pct,
            "ci95_wilson": [low, high],
            "n_exactly_at_threshold": sum(score["fraction_ai"] == AI_THRESHOLD for score in scores),
            "equals_pangram_ai_verdict": ai == verdict_ai,
        },
        "by_threshold": {
            f"fraction_ai>={t}": share_block(sum(score["fraction_ai"] >= t for score in scores), n) for t in THRESHOLDS
        },
        "ai_or_assisted>=0.8": share_block(
            sum(score["fraction_ai"] + score["fraction_ai_assisted"] >= AI_THRESHOLD for score in scores), n
        ),
        "pangram_verdicts": {
            "prediction_short": dict(Counter(score["prediction_short"] for score in scores).most_common()),
            "headline": dict(Counter(score["headline"] for score in scores).most_common()),
        },
        "by_month": [
            {"month": month, "n": len(flags), "n_ai": sum(flags), "share_ai": sum(flags) / len(flags)}
            for month, flags in sorted(by_month.items())
        ],
        "text_length_words": {
            "min": min(words),
            "q25": nearest_rank(words, 0.25),
            "median": int(statistics.median(words)),
            "q75": nearest_rank(words, 0.75),
            "max": max(words),
            f"n_under_{SHORT_TEXT_WORDS}": sum(w < SHORT_TEXT_WORDS for w in words),
            "n_over_1000": sum(w > 1000 for w in words),
        },
        "recleaning": recleaning,
        "redraw_from_12mo_file": redraw,
        "published": PUBLISHED,
        "statements": statements(counts, window, n, len(ai), pct, low, high, scores),
    }


# --------------------------------------------------------------------------------------------------------------
# Report
# --------------------------------------------------------------------------------------------------------------


def pct(value: float, digits: int = 1) -> str:
    return f"{100 * value:.{digits}f}%"


def render_markdown(results: dict, checks: list[str]) -> str:
    h, p, s, w = results["headline"], results["posts"], results["sample"], results["window"]
    low, high = h["ci95_wilson"]
    lines = [
        "# r/PoliticalDiscussion posts scored with Pangram",
        "",
        f"Generated by `code/reddit/reddit_pangram.py` on {results['generated_at']} from "
        f"{', '.join(f'`{v}`' for v in results['source_files'].values())}. All checks passed.",
        "",
        "## The published numbers",
        "",
        "| Number | Value | Source |",
        "|---|---|---|",
        f"| Time window | {w['months']} months, {w['since'][:10]} to {w['until_exclusive'][:10]} (exclusive), "
        f"scraped {w['scraped_at'][:10]} | run record |",
        f"| Posts sampled | {s['n']:,} (seed {s['seed']}) of {p['n_with_text']:,} posts with text, "
        f"out of {p['n_in_window']:,} scraped | run record |",
        f"| AI threshold | `fraction_ai` >= {h['ai_threshold']} "
        f"(at least {round(100 * h['ai_threshold'])}% of the text AI-generated) | `reddit_pangram.py` |",
        f"| Share of posts | {h['n_ai']} / {s['n']:,} = {pct(h['share_ai'])}, rounded **{h['share_ai_pct_rounded']}%**;"
        f" 95% Wilson CI {pct(low)} to {pct(high)} | Pangram scores |",
        "",
        "The threshold set is identical to the posts Pangram's own verdict labels `AI`: "
        f"{h['equals_pangram_ai_verdict']}. Posts exactly at the threshold: {h['n_exactly_at_threshold']}.",
        "",
        "## Denominator",
        "",
        "| Posts in the window | Without text available | With text | Sampled |",
        "|---|---|---|---|",
        f"| {p['n_in_window']:,} | {p['n_without_text']:,} ({pct(p['n_without_text'] / p['n_in_window'])}) | "
        f"{p['n_with_text']:,} ({pct(p['share_with_text'])}) | "
        f"{s['n']:,} ({pct(s['share_of_posts_with_text'])} of those with text) |",
        "",
    ]
    r = results["redraw_from_12mo_file"]
    if r:
        lines += [
            f"Re-drawn from `{r['posts_file']}`: identical to the committed sample. Of the posts without text, "
            f"{r['n_removed_on_submission']:,} were `[removed]` when the archive captured them, "
            f"{r['n_moderator']} were moderator posts and {r['n_empty_after_cleaning']} had no text left after "
            "cleaning.",
            "",
        ]
    lines += ["## Sensitivity to the threshold", "", "| Rule | Posts | Share | 95% CI |", "|---|---|---|---|"]
    for rule, block in {**results["by_threshold"], "ai_or_assisted>=0.8": results["ai_or_assisted>=0.8"]}.items():
        lo, hi = block["ci95_wilson"]
        lines.append(f"| `{rule}` | {block['n']} | {pct(block['share'])} | {pct(lo)} to {pct(hi)} |")
    lines += ["", "## Pangram's own verdicts", "", "| `prediction_short` | Posts |", "|---|---|"]
    lines += [f"| {k} | {v} |" for k, v in results["pangram_verdicts"]["prediction_short"].items()]
    lines += ["", "| `headline` | Posts |", "|---|---|"]
    lines += [f"| {k} | {v} |" for k, v in results["pangram_verdicts"]["headline"].items()]
    lines += ["", "## By month of posting", "", "| Month | Posts | AI (>= 80%) | Share |", "|---|---|---|---|"]
    lines += [f"| {m['month']} | {m['n']} | {m['n_ai']} | {pct(m['share_ai'])} |" for m in results["by_month"]]
    t = results["text_length_words"]
    lines += [
        "",
        "## Text length (words, title and body)",
        "",
        f"Min {t['min']}, lower quartile {t['q25']}, median {t['median']}, upper quartile {t['q75']}, "
        f"max {t['max']:,}. {t[f'n_under_{SHORT_TEXT_WORDS}']} posts are under {SHORT_TEXT_WORDS} words; "
        f"{t['n_over_1000']} exceed 1,000 words. Scoring billed {s['billable_units']:,} units (one per started "
        f"1,000 words) in two sessions ({s['scoring_sessions']['first']} then {s['scoring_sessions']['second']} "
        f"posts), Pangram version {s['pangram_version']}.",
        "",
        "## Checks performed",
        "",
    ]
    lines += [f"- {c}" for c in checks]
    c = results["recleaning"]
    if c:
        lines.append(
            f"- Re-cleaning changed {c['rows_changed']} of the sampled posts, all only in runs of underscores: "
            f"{', '.join(c['ids'])}."
        )
    lines += ["", "## Suggested wording", ""]
    for key, text in results["statements"].items():
        lines += [f"**{key.replace('_', ' ')}**", "", f"> {text}", ""]
    return "\n".join(lines).rstrip() + "\n"


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument(
        "--data", type=Path, default=DATA_DIR, help="Folder with the two data files (default: data/reddit/)"
    )
    parser.add_argument(
        "--out",
        type=Path,
        default=OUT_DIR,
        help="Where to write results.json and results.md (default: output/results/reddit/)",
    )
    parser.add_argument(
        "--posts", type=Path, help="The 12-month scrape the sample was drawn from; re-draws the seed-42 sample from it"
    )
    parser.add_argument(
        "--export-public",
        type=Path,
        metavar="DIR",
        help="After all checks pass, write the scores without post text and both run records to DIR",
    )
    args = parser.parse_args(argv)

    sample_path = args.data / f"{SAMPLE_STEM}.jsonl"
    scores_path = args.data / f"{SAMPLE_STEM}.pangram.jsonl"
    text_free = not sample_path.exists() and (args.data / PUBLIC_SCORES).exists()
    if text_free:
        scores_path = args.data / PUBLIC_SCORES
        if args.posts or args.export_public:
            parser.error("--posts and --export-public need the sample with its text")
    for path in (scores_path, args.posts) if text_free else (sample_path, scores_path, args.posts):
        if path is not None and not path.exists():
            parser.error(f"Missing data file: {path}")
    metas = {
        "sample": read_json(args.data / f"{SAMPLE_STEM}.meta.json"),
        "scores": read_json(args.data / f"{SAMPLE_STEM}.pangram.meta.json"),
    }
    scores = read_rows(scores_path)
    source_files = {"scores": f"data/reddit/{scores_path.name}"}

    checks: list[str] = []
    try:
        window = check_window(metas["sample"])
        checks.append(
            f"Window: {window['months']} whole months, {window['since'][:10]} to {window['until_exclusive'][:10]}."
        )
        redraw = recleaning = None
        if text_free:
            counts = check_run_record(scores, metas["sample"])
            check(all("text" not in w for s in scores for w in s["windows"]), "the text-free scores hold text")
            checks.append(
                f"Sample: {SAMPLE_SIZE:,} scored posts, unique ids, all r/{SUBREDDIT}, all inside the window; "
                f"drawn with seed {SEED} from the {counts['n_with_text']:,} posts with text among "
                f"{counts['n_in_window']:,} scraped. (Post text is not distributed, so the checks that the scores "
                "are for exactly the sampled text, and the re-cleaning check, were run before release.)"
            )
        else:
            sample = read_rows(sample_path)
            source_files = {"sample": f"data/reddit/{sample_path.name}", **source_files}
            counts = check_sample(sample, metas["sample"])
            checks.append(
                f"Sample: {SAMPLE_SIZE:,} posts, unique ids, all r/{SUBREDDIT}, all inside the window, all with text; "
                f"drawn with seed {SEED} from the {counts['n_with_text']:,} posts with text among "
                f"{counts['n_in_window']:,} scraped."
            )
            if args.posts:
                redraw = check_redraw(args.posts, sample, metas["sample"])
                checks.append(
                    f"Re-draw: seed-{SEED} sample re-drawn from {args.posts} is identical to the committed sample."
                )
            recleaning = check_recleaning(sample)
            checks.append("Cleaning: re-cleaning the committed text is a no-op apart from underscore rules.")
            check_scores_match_sample(sample, scores)
        check_scores(scores, metas["scores"])
        checks.append(
            f"Scores: one Pangram {PANGRAM_VERSION} result per sampled post"
            + ("" if text_free else ", for exactly its title and body")
            + "; fractions consistent."
        )
        results = compute(scores, metas, counts, window, recleaning, redraw, source_files)
        checks.append(
            f"Headline: {results['headline']['n_ai']} / {SAMPLE_SIZE:,} = "
            f"{results['headline']['share_ai_pct_rounded']}% at the {AI_THRESHOLD} threshold, as published."
        )
    except CheckFailed as exc:
        print(f"CHECK FAILED: {exc}", file=sys.stderr)
        return 1
    results["checks_passed"] = checks

    args.out.mkdir(parents=True, exist_ok=True)
    (args.out / "results.json").write_text(json.dumps(results, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
    (args.out / "results.md").write_text(render_markdown(results, checks), encoding="utf-8")
    h = results["headline"]
    low, high = (100 * v for v in h["ci95_wilson"])
    if args.export_public:
        export_public(scores, args.data, args.export_public)
        print(f"Text-free scores and run records written to {args.export_public}.")
    print(
        f"{h['n_ai']} of {SAMPLE_SIZE:,} sampled posts ({100 * h['share_ai']:.1f}%, rounded "
        f"{h['share_ai_pct_rounded']}%) are at least {round(100 * AI_THRESHOLD)}% AI-generated; "
        f"95% CI {low:.1f} to {high:.1f}%. Results written to {args.out / 'results.json'} and results.md."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
