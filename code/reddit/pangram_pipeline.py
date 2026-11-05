#!/usr/bin/env python3
"""Clean, sample and Pangram-score scraped Reddit posts: the pipeline behind the r/PoliticalDiscussion figures.

This is a self-contained copy of the `reddit_scraper.detection` package of the p4_pangram repository (the
functions below are lifted verbatim from its `clean.py`, `prepare.py`, `sample.py`, `pangram.py`,
`http_client.py`, `runner.py` and `writers.py`), reduced to the code path that produced the committed scores:

  clean   Strip Reddit markup and quoted text so Pangram scores only what the author wrote.
          Writes `<stem>_clean.jsonl` and copies the scrape's `.meta.json` next to it with a `cleaning` block.
  sample  Draw a seeded random sample of the items worth scoring (not removed, deleted, moderator-written or empty).
          Writes `<stem>_sample<N>_seed<S>.jsonl` plus `.meta.json` with a `sampling` block. The draw is made from
          an id-sorted list, so the same seed picks the same rows however the file is ordered.
  score   Send each item (a post's title and body, or a comment's body) to Pangram's single-text `/task` endpoint
          and save one result row per item to `<stem>.pangram.jsonl` (resumable: rerunning sends only items not yet
          scored), with the run record in `<stem>.pangram.meta.json`. The API key is read from the PANGRAM_API_KEY
          environment variable or `--key-file`. Pangram's Bulk API, which the original package also supports, is
          not included: the committed scores were produced with the single-text mode because bulk jobs stalled.

Re-scoring is NOT a no-op: it costs about one billable unit per item (one unit per started 1,000 words; about
1,018 units for the committed sample) and Pangram updates its model, so results can differ from the committed
`.pangram.jsonl`, which is the source of truth for the reported numbers. `analyse.py` reproduces the numbers from
the committed files without any network access.

Usage:
  python3 pangram_pipeline.py clean  data/politicaldiscussion_posts_12mo.jsonl
  python3 pangram_pipeline.py sample data/politicaldiscussion_posts_12mo_clean.jsonl -n 1000 --seed 42
  python3 pangram_pipeline.py score  data/politicaldiscussion_posts_12mo_clean_sample1000_seed42.jsonl --dry-run
  export PANGRAM_API_KEY=...
  python3 pangram_pipeline.py score  data/politicaldiscussion_posts_12mo_clean_sample1000_seed42.jsonl

Requires Python 3.10+. The `requests` package is needed only for `score` (not for `--dry-run`).
"""

from __future__ import annotations

import argparse
import html
import json
import logging
import math
import os
import random
import re
import sys
import time
from collections import Counter
from collections.abc import Iterator
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Literal

__version__ = "0.1.0"  # version of the p4_pangram package this was lifted from

log = logging.getLogger("pangram_pipeline")

# --------------------------------------------------------------------------------------------------------------
# Files: `.jsonl` data and the `.meta.json` run record written next to it (from reddit_scraper/writers.py)
# --------------------------------------------------------------------------------------------------------------


def manifest_path(data_path: Path) -> Path:
    """The `.meta.json` describing a data file; for a `_partN` file without its own, the one for the whole output."""
    data_path = Path(data_path)
    own = data_path.with_suffix(".meta.json")
    match = re.fullmatch(r"(.+)_part\d+", data_path.stem)
    if own.exists() or not match:
        return own
    return data_path.with_name(f"{match.group(1)}.meta.json")


def write_manifest(output: Path, manifest: dict) -> Path:
    """Write `manifest` as `<output>.meta.json` next to the data file and return its path."""
    path = Path(output).with_suffix(".meta.json")
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(manifest, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
    return path


def read_manifest(data_path: Path) -> dict:
    """The run record of a data file, or an empty dict if it has none."""
    path = manifest_path(data_path)
    return json.loads(path.read_text(encoding="utf-8")) if path.exists() else {}


def read_rows(input_path: Path) -> list[dict]:
    with Path(input_path).open(encoding="utf-8") as file:
        return [json.loads(line) for line in file if line.strip()]


class JsonlWriter:
    """Writes one JSON object per line and flushes each write, so partial runs leave a valid file."""

    def __init__(self, path: Path, append: bool = False) -> None:
        self.path = Path(path)
        self.append = append

    def __enter__(self) -> JsonlWriter:
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self._file = self.path.open("a" if self.append else "w", encoding="utf-8")
        return self

    def write(self, record: dict) -> None:
        self._file.write(json.dumps(record, ensure_ascii=False) + "\n")
        self._file.flush()

    def __exit__(self, *exc_info) -> None:
        self._file.close()


def _now() -> str:
    return datetime.now(timezone.utc).isoformat(timespec="seconds")


# --------------------------------------------------------------------------------------------------------------
# Which items are worth scoring, and what text is scored (from reddit_scraper/detection/prepare.py)
# --------------------------------------------------------------------------------------------------------------

Kind = Literal["comment", "post"]

REMOVED_BODIES = frozenset({"[removed]", "[deleted]"})
MODERATOR_ROLES = frozenset({"moderator", "admin"})
MODERATOR_AUTHORS = frozenset({"AutoModerator"})
WORDS_PER_UNIT = 1000


@dataclass(frozen=True)
class DetectionItem:
    id: str
    kind: Kind
    text: str
    row: dict

    @property
    def word_count(self) -> int:
        return len(self.text.split())

    @property
    def units(self) -> int:
        return billable_units(self.text)


def kind_from_meta(input_path: Path) -> Kind | None:
    """The item kind recorded in the scraper's `.meta.json`, if that file exists."""
    content_type = read_manifest(input_path).get("content_type")
    if content_type == "comments":
        return "comment"
    return "post" if content_type in ("posts", "text_posts") else None


def kind_from_row(row: dict) -> Kind:
    if "parent_id" in row:
        return "comment"
    if "title" in row:
        return "post"
    raise ValueError(f"Row {row.get('id')!r} is neither a comment (parent_id) nor a post (title)")


def item_text(row: dict, kind: Kind) -> str:
    """What Pangram scores: a post's title and body together, a comment's body."""
    body = row.get("body") or ""
    return f"{row.get('title') or ''}\n\n{body}" if kind == "post" else body


def load_items(input_path: Path) -> list[DetectionItem]:
    """Read a scraped JSONL file into items, telling comments and posts apart."""
    kind = kind_from_meta(input_path)
    items = []
    for row in read_rows(input_path):
        row_kind = kind or kind_from_row(row)
        items.append(DetectionItem(id=row["id"], kind=row_kind, text=item_text(row, row_kind), row=row))
    return items


def row_skip_reason(row: dict) -> str | None:
    """Why a scraped comment or post row has no text worth scoring, or None if it has."""
    body = (row.get("body") or "").strip()
    if body in REMOVED_BODIES:
        return "removed_or_deleted"
    if row.get("distinguished") in MODERATOR_ROLES or row.get("author") in MODERATOR_AUTHORS:
        return "moderator"
    if not body:
        return "empty"
    return None


def skip_reason(item: DetectionItem) -> str | None:
    return row_skip_reason(item.row)


def billable_units(text: str) -> int:
    """Pangram bills one unit per started 1,000-word block, with a minimum of one."""
    return max(1, math.ceil(len(text.split()) / WORDS_PER_UNIT))


# --------------------------------------------------------------------------------------------------------------
# clean (from reddit_scraper/detection/clean.py)
# --------------------------------------------------------------------------------------------------------------

ZERO_WIDTH = re.compile("[\u200b\u200c\u200d\u2060\ufeff]")
QUOTE_LINE = re.compile(r"^[ \t]*>.*$\n?", re.MULTILINE)
# The url may contain one level of parentheses, as Wikipedia links often do.
MARKDOWN_LINK = re.compile(r"\[([^\]]*)\]\((?:[^()\s]|\([^()\s]*\))*\)")
EMPHASIS = re.compile(r"(\*\*\*|\*\*|__)(.+?)\1")
TRAILING_SPACE = re.compile(r"[ \t]+$", re.MULTILINE)
EXTRA_BLANK_LINES = re.compile(r"\n{3,}")


def clean_text(text: str, drop_quotes: bool = True) -> str:
    """`text` without HTML entities, zero-width characters, quoted lines, link urls or bold markers."""
    text = ZERO_WIDTH.sub("", html.unescape(text))
    if drop_quotes:
        text = QUOTE_LINE.sub("", text)
    text = EMPHASIS.sub(r"\2", MARKDOWN_LINK.sub(r"\1", text))
    return EXTRA_BLANK_LINES.sub("\n\n", TRAILING_SPACE.sub("", text)).strip()


def clean_row(row: dict) -> dict:
    """A copy of a scraped row with its title and body cleaned; removal placeholders are kept as they are."""
    cleaned = dict(row)
    body = row.get("body") or ""
    if body.strip() not in REMOVED_BODIES:
        cleaned["body"] = clean_text(body)
    if row.get("title"):
        cleaned["title"] = clean_text(row["title"], drop_quotes=False)
    return cleaned


def clean_output(input_path: Path) -> Path:
    input_path = Path(input_path)
    return input_path.with_name(f"{input_path.stem}_clean.jsonl")


def clean_file(input_path: Path, output_path: Path | None = None) -> dict:
    """Write a cleaned copy of a scraped `.jsonl` file and its `.meta.json`; return the cleaning counts.

    The source run record is copied into the new `.meta.json`, so the item type is still known.
    """
    input_path = Path(input_path)
    output_path = Path(output_path) if output_path else clean_output(input_path)
    counts: Counter[str] = Counter()
    with input_path.open(encoding="utf-8") as source, JsonlWriter(output_path) as writer:
        for line in source:
            if not line.strip():
                continue
            row = json.loads(line)
            cleaned = clean_row(row)
            counts["items"] += 1
            if cleaned != row:
                counts["changed"] += 1
                if (row.get("body") or "").strip() and not cleaned["body"]:
                    counts["emptied"] += 1
            writer.write(cleaned)

    cleaning = {
        "cleaned_from": str(input_path),
        "items": counts["items"],
        "items_changed": counts["changed"],
        "items_emptied": counts["emptied"],
        "cleaned_at": _now(),
        "cleaner_version": __version__,
    }
    write_manifest(output_path, {**read_manifest(input_path), "cleaning": cleaning})
    return {"output": str(output_path), **cleaning}


# --------------------------------------------------------------------------------------------------------------
# sample (from reddit_scraper/detection/sample.py)
# --------------------------------------------------------------------------------------------------------------

DEFAULT_SEED = 42


def sample_rows(rows: list[dict], size: int, seed: int = DEFAULT_SEED) -> list[dict]:
    """`size` rows drawn without replacement from those worth scoring, kept in their original order.

    Rows are drawn from an id-sorted list, so the same seed picks the same rows however the file is ordered.
    """
    eligible = [row for row in rows if row_skip_reason(row) is None]
    if size > len(eligible):
        raise ValueError(f"Cannot sample {size} items: only {len(eligible)} are worth scoring")
    chosen = {row["id"] for row in random.Random(seed).sample(sorted(eligible, key=lambda row: row["id"]), size)}
    return [row for row in rows if row["id"] in chosen]


def sample_output(input_path: Path, size: int, seed: int) -> Path:
    input_path = Path(input_path)
    return input_path.with_name(f"{input_path.stem}_sample{size}_seed{seed}.jsonl")


def sample_file(input_path: Path, size: int, seed: int = DEFAULT_SEED, output_path: Path | None = None) -> dict:
    """Write a random sample of `input_path` and its `.meta.json`; return the sampling record."""
    input_path = Path(input_path)
    output_path = Path(output_path) if output_path else sample_output(input_path, size, seed)
    rows = read_rows(input_path)
    sample = sample_rows(rows, size, seed)
    with JsonlWriter(output_path) as writer:
        for row in sample:
            writer.write(row)

    sampling = {
        "sampled_from": str(input_path),
        "items_in_source": len(rows),
        "items_eligible": sum(row_skip_reason(row) is None for row in rows),
        "items_sampled": len(sample),
        "seed": seed,
        "sampled_at": _now(),
        "sampler_version": __version__,
    }
    write_manifest(output_path, {**read_manifest(input_path), "sampling": sampling})
    return {"output": str(output_path), **sampling}


# --------------------------------------------------------------------------------------------------------------
# Pangram client: single-text tasks (from reddit_scraper/detection/pangram.py and reddit_scraper/http_client.py)
# --------------------------------------------------------------------------------------------------------------

BASE_URL = "https://text.external-api.pangram.com"
TERMINAL_STAGES = frozenset({"STAGE_SUCCESS", "STAGE_FAILED"})
RETRY_STATUSES = frozenset({429, 500, 502, 503, 504})  # 422 means invalid input here, not a timeout
MAX_BACKOFF = 120.0
# Only these are safe to retry: retrying a POST could repeat a paid Pangram submission whose reply was lost.
SAFE_METHODS = frozenset({"GET", "HEAD"})
UNPROCESSED_STATUSES = frozenset({429})
USER_AGENT = "p4_pangram-reddit-scraper/0.1 (research scraper)"
ENV_VAR = "PANGRAM_API_KEY"


class HttpError(RuntimeError):
    def __init__(self, message: str, status: int | None = None) -> None:
        super().__init__(message)
        self.status = status


class AuthError(HttpError):
    """The server rejected the credentials (401)."""


class PaymentError(HttpError):
    """The account has run out of credits (402)."""


class TaskTimeoutError(RuntimeError):
    pass


class HttpClient:
    """Rate-limited JSON client. The only place in this file that touches the network.

    Requests are spaced at least `min_interval` seconds apart plus a random extra of up to `jitter` seconds.
    """

    def __init__(
        self,
        min_interval: float = 1.0,
        jitter: float = 0.2,
        max_retries: int = 6,
        timeout: float = 60.0,
        sleep=time.sleep,
        retry_statuses: frozenset[int] = RETRY_STATUSES,
    ) -> None:
        import requests  # imported here so clean, sample and --dry-run need no third-party packages

        self.min_interval = min_interval
        self.jitter = jitter
        self.max_retries = max_retries
        self.timeout = timeout
        self.retry_statuses = retry_statuses
        self.session = requests.Session()
        self.session.headers["User-Agent"] = USER_AGENT
        self._requests = requests
        self._sleep = sleep
        self._last_request = 0.0

    def request_json(
        self,
        method: str,
        url: str,
        params: dict[str, Any] | None = None,
        json: Any = None,
        headers: dict[str, str] | None = None,
    ) -> Any:
        """Send a request and return its JSON body.

        GET requests are retried on network errors and `retry_statuses`. Other methods are retried only
        when the request certainly was not processed (connect timeout, HTTP 429), so they are never repeated.
        """
        requests = self._requests
        safe = method.upper() in SAFE_METHODS
        for attempt in range(self.max_retries + 1):
            self._throttle()
            try:
                response = self.session.request(
                    method, url, params=params, json=json, headers=headers, timeout=self.timeout
                )
            except requests.RequestException as exc:
                if not safe and not isinstance(exc, requests.ConnectTimeout):
                    raise HttpError(
                        f"{method} {url} failed: {exc}. Not retried, because the server may have processed it."
                    ) from exc
                if attempt == self.max_retries:
                    raise HttpError(f"Request to {url} failed: {exc}") from exc
                self._backoff(attempt, reason=str(exc))
                continue

            retryable = response.status_code in self.retry_statuses and (
                safe or response.status_code in UNPROCESSED_STATUSES
            )
            if retryable and attempt < self.max_retries:
                self._backoff(attempt, reason=f"HTTP {response.status_code}", response=response)
                continue
            if response.status_code == 401:
                raise AuthError(f"Credentials for {url} were rejected (HTTP 401).", 401)
            if response.status_code == 402:
                raise PaymentError(f"Insufficient credits for {url} (HTTP 402).", 402)
            if not response.ok:
                raise HttpError(
                    f"HTTP {response.status_code} from {response.url}: {response.text[:200]}", response.status_code
                )
            if "json" not in response.headers.get("Content-Type", ""):
                raise HttpError(f"Expected JSON from {url} but got something else (ended up at {response.url}).")
            return response.json()

        raise HttpError(f"Giving up on {url} after {self.max_retries} retries")

    def _throttle(self) -> None:
        interval = self.min_interval + random.uniform(0, self.jitter)
        wait = interval - (time.monotonic() - self._last_request)
        if wait > 0:
            self._sleep(wait)
        self._last_request = time.monotonic()

    def _backoff(self, attempt: int, reason: str, response=None) -> None:
        delay = min(2.0 ** (attempt + 1), MAX_BACKOFF) + random.uniform(0, 1)
        if response is not None and response.status_code == 429:
            server_wait = _float_header(response, "Retry-After") or _float_header(response, "X-RateLimit-Reset")
            if server_wait:
                delay = max(delay, server_wait + 1)
        log.warning("Retrying in %.0fs (%s)", delay, reason)
        self._sleep(delay)


def _float_header(response, name: str) -> float | None:
    value = response.headers.get(name)
    try:
        return float(value) if value is not None else None
    except ValueError:
        return None


class PangramClient:
    """Pangram's single-text API: submit one text as a task, poll until it is done."""

    def __init__(self, api_key: str, http=None, base_url: str = BASE_URL, sleep=time.sleep) -> None:
        self._api_key = api_key
        self.http = http or HttpClient(sleep=sleep)
        self.base_url = base_url.rstrip("/")
        self._sleep = sleep

    def submit_task(self, text: str) -> str:
        """Queue one text and return its task id."""
        return self._request("POST", "/task", json={"text": text})["task_id"]

    def wait_tasks(
        self, task_ids: list[str], poll_interval: float = 5.0, timeout: float = 3600.0
    ) -> Iterator[tuple[str, dict[str, Any]]]:
        """Poll single-text tasks and yield `(task_id, result)` as each reaches a terminal stage."""
        waiting = list(task_ids)
        waited = 0.0
        while True:
            for task_id in list(waiting):
                result = self._request("GET", f"/task/{task_id}")
                if result.get("stage") in TERMINAL_STAGES:
                    waiting.remove(task_id)
                    yield task_id, result
            if not waiting:
                return
            if waited >= timeout:
                raise TaskTimeoutError(f"{len(waiting)} tasks still unfinished after {timeout:.0f}s")
            self._sleep(poll_interval)
            waited += poll_interval

    def _request(self, method: str, path: str, **kwargs: Any) -> Any:
        return self.http.request_json(method, self.base_url + path, headers={"x-api-key": self._api_key}, **kwargs)


def get_api_key(key_file: Path | None = None) -> str:
    """The Pangram API key from `key_file` if given, else from the PANGRAM_API_KEY environment variable."""
    if key_file is not None:
        key = Path(key_file).read_text(encoding="utf-8").strip()
        if not key:
            raise RuntimeError(f"Key file {key_file} is empty.")
        return key
    key = os.environ.get(ENV_VAR, "").strip()
    if not key:
        raise RuntimeError(f"No Pangram API key: set {ENV_VAR} or pass --key-file.")
    return key


# --------------------------------------------------------------------------------------------------------------
# score (from reddit_scraper/detection/runner.py, single-text mode)
# --------------------------------------------------------------------------------------------------------------

# Output field name: Pangram result field name.
RESULT_FIELDS = {
    "pangram_version": "version",
    "prediction_short": "prediction_short",
    "headline": "headline",
    "fraction_ai": "fraction_ai",
    "fraction_ai_assisted": "fraction_ai_assisted",
    "fraction_human": "fraction_human",
    "num_ai_segments": "num_ai_segments",
    "num_ai_assisted_segments": "num_ai_assisted_segments",
    "num_human_segments": "num_human_segments",
    "windows": "windows",
}
DONE_STATUSES = frozenset({"scored", "skipped"})
TASK_WAVE = 20


def score_output(input_path: Path) -> Path:
    input_path = Path(input_path)
    return input_path.with_name(f"{input_path.stem}.pangram.jsonl")


def read_done_ids(output_path: Path) -> set[str]:
    """Ids whose latest row in an existing output is scored or skipped; failed items are retried."""
    latest: dict[str, str] = {}
    path = Path(output_path)
    if path.exists():
        for row in read_rows(path):
            latest[row["id"]] = row["status"]
    return {item_id for item_id, status in latest.items() if status in DONE_STATUSES}


def read_pending_tasks(output_path: Path) -> list[dict]:
    """Single-text tasks a previous run submitted but did not finish collecting."""
    return read_manifest(output_path).get("pending_tasks", [])


@dataclass
class DetectionPlan:
    to_send: list[DetectionItem] = field(default_factory=list)
    skipped: list[tuple[DetectionItem, str]] = field(default_factory=list)
    already_done: int = 0

    @property
    def units(self) -> int:
        return sum(item.units for item in self.to_send)

    def summary(self) -> dict:
        return {
            "to_send": len(self.to_send),
            "estimated_units": self.units,
            "already_done": self.already_done,
            "skip_reasons": dict(Counter(reason for _, reason in self.skipped)),
        }


def plan_detection(items: list[DetectionItem], done_ids: set[str]) -> DetectionPlan:
    plan = DetectionPlan()
    for item in items:
        if item.id in done_ids:
            plan.already_done += 1
        elif reason := skip_reason(item):
            plan.skipped.append((item, reason))
        else:
            plan.to_send.append(item)
    return plan


def record(item: DetectionItem, status: str, **fields) -> dict:
    """One output row: context to join back to the data, the outcome, and Pangram's scores."""
    row = item.row
    return {
        "id": item.id,
        "kind": item.kind,
        "subreddit": row.get("subreddit"),
        "post_id": row.get("post_id") if item.kind == "comment" else item.id,
        "created_utc": row.get("created_utc"),
        "word_count": item.word_count,
        "status": status,
        "skip_reason": None,
        "error": None,
        **dict.fromkeys(RESULT_FIELDS),
        "bulk_id": None,
        "task_id": None,
        **fields,
    }


class _Run:
    """State of one scoring run; writes each outcome as soon as it is known."""

    def __init__(self, writer: JsonlWriter, items: list[DetectionItem], done: set[str]) -> None:
        self.writer = writer
        self.by_id = {item.id: item for item in items}
        self.done = done
        self.counts: Counter[str] = Counter()

    def write(self, item: DetectionItem, status: str, **fields) -> None:
        if item.id in self.done:
            return
        self.writer.write(record(item, status, **fields))
        self.counts[status] += 1
        if status in DONE_STATUSES:
            self.done.add(item.id)

    def collect_tasks(
        self, client: PangramClient, tasks: list[dict], poll_interval: float, wait_timeout: float
    ) -> None:
        by_task = {task["task_id"]: self.by_id[task["id"]] for task in tasks if task["id"] in self.by_id}
        for task_id, result in client.wait_tasks(list(by_task), poll_interval=poll_interval, timeout=wait_timeout):
            self._write_result(by_task[task_id], task_id, result)

    def _write_result(self, item: DetectionItem, task_id: str, result: dict) -> None:
        trace = {"bulk_id": None, "task_id": task_id}
        if result.get("stage") == "STAGE_SUCCESS" and result:
            self.write(item, "scored", **trace, **{out: result.get(key) for out, key in RESULT_FIELDS.items()})
        else:
            error = result.get("error") or result.get("headline") or result.get("stage") or "unknown"
            self.write(item, "failed", error=error, **trace)


def run_detection(
    input_path: Path,
    client: PangramClient,
    output_path: Path | None = None,
    poll_interval: float = 5.0,
    wait_timeout: float = 3600.0,
    force: bool = False,
    task_wave: int = TASK_WAVE,
) -> dict:
    """Score every eligible item of a scraped JSONL file and return the run record.

    Results go to `<input stem>.pangram.jsonl` (appended to, so reruns only send what is missing) and the run
    record to `<input stem>.pangram.meta.json`, which is written whatever the outcome. Each text is sent as its
    own task, in waves of `task_wave`.
    """
    input_path = Path(input_path)
    output_path = Path(output_path) if output_path else score_output(input_path)
    items = load_items(input_path)
    done = set() if force else read_done_ids(output_path)
    pending_tasks = [] if force else read_pending_tasks(output_path)
    started_at = _now()
    units_submitted = 0
    already_done = len(done & {item.id for item in items})
    skip_reasons: Counter[str] = Counter()
    status, error = "error", None
    run = None

    def manifest(current_status: str) -> dict:
        return {
            "status": current_status,
            "error": error,
            "input": str(input_path),
            "counts": dict(run.counts) if run else {},
            "skip_reasons": dict(skip_reasons),
            "already_done": already_done,
            "units_submitted": units_submitted,
            "bulk_ids": [],
            "pending_bulks": [],
            "pending_tasks": pending_tasks,
            "batch_units": None,
            "started_at": started_at,
            "finished_at": _now() if current_status != "running" else None,
            "scraper_version": __version__,
        }

    log.info("Scoring %d items from %s with Pangram", len(items), input_path)
    try:
        with JsonlWriter(output_path, append=not force) as writer:
            run = _Run(writer, items, done)
            if pending_tasks:
                log.info("Collecting %d tasks left over from a previous run", len(pending_tasks))
                run.collect_tasks(client, pending_tasks, poll_interval, wait_timeout)
                pending_tasks.clear()

            plan = plan_detection(items, done)
            for item, reason in plan.skipped:
                run.write(item, "skipped", skip_reason=reason)
                skip_reasons[reason] += 1

            for start in range(0, len(plan.to_send), task_wave):
                for item in plan.to_send[start : start + task_wave]:
                    pending_tasks.append({"id": item.id, "task_id": client.submit_task(item.text)})
                    units_submitted += item.units
                    write_manifest(output_path, manifest("running"))
                run.collect_tasks(client, pending_tasks, poll_interval, wait_timeout)
                pending_tasks.clear()
                finished = min(start + task_wave, len(plan.to_send))
                log.info("Tasks: %d/%d finished (%s)", finished, len(plan.to_send), dict(run.counts))
        status = "complete"
    except KeyboardInterrupt:
        status = "interrupted"
    except Exception as exc:
        error = str(exc)
        raise
    finally:
        result = manifest(status)
        path = write_manifest(output_path, result)
        log.info("%s: %s written to %s (run details in %s)", status, result["counts"], output_path, path)
    return result


# --------------------------------------------------------------------------------------------------------------
# Command line
# --------------------------------------------------------------------------------------------------------------


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="pangram_pipeline.py", description="Clean, sample and Pangram-score scraped Reddit posts or comments."
    )
    parser.add_argument("--verbose", "-v", action="store_true", help="Show debug logging")
    commands = parser.add_subparsers(dest="command", required=True)

    clean = commands.add_parser("clean", help="Copy a scraped .jsonl file without quoted text or formatting")
    clean.add_argument("input", type=Path, help="A .jsonl file written by the scraper")
    clean.add_argument("--output", "-o", type=Path, help="Output path (default: <stem>_clean.jsonl)")

    sample = commands.add_parser("sample", help="Copy a seeded random sample of the items worth scoring")
    sample.add_argument("input", type=Path, help="A .jsonl file written by the scraper or `clean`")
    sample.add_argument("--size", "-n", type=int, required=True, help="Number of items to sample")
    sample.add_argument("--seed", type=int, default=DEFAULT_SEED, help=f"Random seed (default: {DEFAULT_SEED})")
    sample.add_argument("--output", "-o", type=Path, help="Output path (default: <stem>_sample<size>_seed<seed>.jsonl)")

    score = commands.add_parser("score", help="Score a .jsonl file with Pangram, one text per task")
    score.add_argument("input", type=Path, help="A .jsonl file written by the scraper, `clean` or `sample`")
    score.add_argument("--output", "-o", type=Path, help="Results path (default: <stem>.pangram.jsonl)")
    score.add_argument("--key-file", type=Path, help=f"File holding the API key (default: the {ENV_VAR} variable)")
    score.add_argument(
        "--poll-interval", type=float, default=5.0, help="Seconds between task status checks (default: 5)"
    )
    score.add_argument(
        "--task-wave", type=int, default=TASK_WAVE, help=f"Tasks in flight at once (default: {TASK_WAVE})"
    )
    score.add_argument("--dry-run", action="store_true", help="Show what would be sent and its cost; no API calls")
    score.add_argument("--force", action="store_true", help="Ignore existing results and score everything again")
    return parser


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(asctime)s %(levelname)s %(message)s",
        datefmt="%H:%M:%S",
    )
    if not args.input.exists():
        parser.error(f"Input file not found: {args.input}")

    if args.command == "clean":
        result = clean_file(args.input, args.output)
        log.info(
            "%s: %d items, %d changed, %d left with no text -> %s",
            args.input,
            result["items"],
            result["items_changed"],
            result["items_emptied"],
            result["output"],
        )
        return 0

    if args.command == "sample":
        if args.size < 1:
            parser.error("--size must be at least 1")
        try:
            result = sample_file(args.input, args.size, args.seed, args.output)
        except ValueError as exc:
            parser.error(str(exc))
        log.info(
            "%s: sampled %d of %d items worth scoring (seed %d) -> %s",
            args.input,
            result["items_sampled"],
            result["items_eligible"],
            result["seed"],
            result["output"],
        )
        return 0

    output = args.output or score_output(args.input)
    if args.dry_run:
        done = set() if args.force else read_done_ids(output)
        print(json.dumps(plan_detection(load_items(args.input), done).summary(), indent=2))
        return 0
    if args.task_wave < 1:
        parser.error("--task-wave must be at least 1")
    try:
        client = PangramClient(api_key=get_api_key(args.key_file))
        result = run_detection(
            args.input,
            client,
            output_path=output,
            poll_interval=args.poll_interval,
            force=args.force,
            task_wave=args.task_wave,
        )
    except AuthError:
        log.error("Pangram rejected the API key (HTTP 401).")
        return 1
    except PaymentError:
        log.error("The Pangram account has insufficient credits (HTTP 402); rerun later to resume.")
        return 1
    except (HttpError, TaskTimeoutError, RuntimeError) as exc:
        log.error("%s", exc)
        return 1
    return 0 if result["status"] == "complete" else 1


if __name__ == "__main__":
    sys.exit(main())
