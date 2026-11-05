#!/usr/bin/env python3
"""si_12_example_conversations.py

Example conversations: the released transcripts and the issue-matched set of
transcripts printed in the SI ("Example conversations").

Selection is outcome-blind:
  1. Eligibility uses only engagement and length, never attitude change or
     donation (complete case; persuadee sent >= MIN_PE_MSGS messages; total
     words under a printable cap; issue not in EXCLUDE_ISSUES).
  2. Within each stratum, eligible conversations are shuffled with a fixed seed.
  3. Draws are screened in that order for personal information and sensitive
     content. A rejected draw is replaced by the next one and logged in
     REJECTED with its reason, so the procedure is fully auditable.

Strata:
  Study 2: one AI, one Constrained AI and one Coached Elite Debater
           conversation per eligible issue (issue-matched triplets).
  Study 4: N_DONATION AI and N_DONATION Professional Canvasser conversations.

The SI prints the Study 2 triplet with the fewest words in total.

Outputs:
  output/example_conversations/
      transcripts/*.txt, manifest.csv          released in the project repository
  output/tables/si/tab_example_conversations.tex
                                               -> SI.tex (\\input)
  output/results/_py/si_12_example_draw_log.csv  every draw and its status
  output/results/_py/si_12_example_flags.txt     automated PII/sensitivity hits
                                                 to check by hand
"""

from __future__ import annotations

import csv
import html
import json
import random
import re
import shutil
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
DATA = ROOT / "data"
SUPP = ROOT / "output/example_conversations"
RESULTS_PY = ROOT / "output/results/_py"
TABLES_SI = ROOT / "output/tables/si"

SEED = 20261007
MIN_PE_MSGS = 4
N_DONATION = 3

# Issues whose `issue_stance` text is the opposite pole from the side the
# persuader argued, so a "statement argued for" header would be misleading.
EXCLUDE_ISSUES = {"immigration", "monarchy", "controversial_speech", "benefit_cap"}

WORD_CAP = {  # total conversation words, to keep examples printable
    ("study_2", "ai"): 4500,
    ("study_2", "ai_constrained"): 2600,
    ("study_2", "coached_elite_debater"): 2600,
    ("study_4", "ai"): 3800,
    ("study_4", "canvasser"): 2000,
}

# Draws rejected at screening: row_id -> reason category.
REJECTED: dict[str, str] = {
    "anon_0062441": "persuadee PII (names a friend; occupation and age)",
    "anon_0082590": "persuader PII (family details)",
    "anon_0083296": "politically sensitive (current conflicts)",
    "anon_0080852": "unverified allegations against a named organization",
    "anon_0082808": "politically sensitive (party politics, named politicians); persuadee PII",
}

# Literal replacements for personal details a regex can't catch.
REDACTIONS: list[tuple[str, str]] = []

LABEL = {
    "coached_elite_debater": "Coached Elite Debater",
    "canvasser": "Professional Canvasser",
}

# ---------------------------------------------------------------------------
# Text cleaning.
# ---------------------------------------------------------------------------
EMAIL_RE = re.compile(r"[\w.+-]+@[\w-]+\.[\w.-]+")
URL_RE = re.compile(r"https?://\S+|www\.\S+", re.I)
HANDLE_RE = re.compile(r"(?<![\w.])@[A-Za-z0-9_]{3,}")
PHONE_RE = re.compile(r"(?<!\d)(?:\+?44\s?|0)7\d{2,3}[\s-]?\d{3}[\s-]?\d{3,4}(?!\d)")
LONGNUM_RE = re.compile(r"(?<!\d)\d{11,}(?!\d)")
BOLD_RE = re.compile(r"\*\*(.+?)\*\*|__(.+?)__", re.S)
HEADER_RE = re.compile(r"(?m)^\s{0,3}#{1,6}\s*")
RULE_RE = re.compile(r"(?m)^\s*([-*_])\1{2,}\s*$")
ITALIC_RE = re.compile(r"(?<![\w*])\*(?=\S)([^*\n]+?)(?<=\S)\*(?![\w*])")

# Automated screening: phrases that often precede personal details, plus a
# short list of terms that warrant a read for sensitivity. Hits are flagged
# for manual review, not removed.
PII_FLAG_RE = re.compile(
    r"\b(my name is|i'?m called|call me|i live in|i'?m from|i work (?:at|for|as)|"
    r"my (?:son|daughter|husband|wife|partner|mum|mom|dad|father|mother|brother|"
    r"sister|grandad|grandma|nan|boss|employer|school|uni|university|town|village|city))\b",
    re.I,
)
SENSITIVE_FLAG_RE = re.compile(
    r"\b(suicid\w*|self[- ]harm|abuse\w*|disab\w*|cancer|terminal|diagnos\w*|"
    r"religio\w*|muslim\w*|jew\w*|christian\w*|racis\w*|nazi\w*|rape\w*|kill\w*)\b",
    re.I,
)


def render_markdown(s: str) -> str:
    s = BOLD_RE.sub(lambda m: m.group(1) or m.group(2), s)
    s = re.sub(r"(?m)^(\s*)[*+]\s+", r"\1- ", s)
    s = ITALIC_RE.sub(r"\1", s)
    s = HEADER_RE.sub("", s)
    return RULE_RE.sub("", s)


def clean_text(s: str | None) -> str:
    if not s:
        return ""
    s = html.unescape(s).replace("\u00a0", " ")
    s = s.replace("\\r\\n", "\n").replace("\\n", "\n").replace("\\r", "\n").replace("\\t", " ")
    s = s.replace('\\"', '"').replace("\\'", "'").replace("\\\\", "\\")
    s = render_markdown(s)
    s = EMAIL_RE.sub("[email removed]", s)
    s = URL_RE.sub("[link removed]", s)
    s = PHONE_RE.sub("[number removed]", s)
    s = LONGNUM_RE.sub("[number removed]", s)
    s = HANDLE_RE.sub("[handle removed]", s)
    for needle, repl in REDACTIONS:
        s = s.replace(needle, repl)
    s = re.sub(r"[ \t]+\n", "\n", s)
    return re.sub(r"\n{3,}", "\n\n", s).strip()


def wrap(text: str, width: int = 74) -> list[str]:
    out, cur = [], ""
    for w in text.split(" "):
        while len(w) > width:
            if cur:
                out.append(cur)
                cur = ""
            out.append(w[:width])
            w = w[width:]
        if not cur:
            cur = w
        elif len(cur) + 1 + len(w) <= width:
            cur += " " + w
        else:
            out.append(cur)
            cur = w
    if cur:
        out.append(cur)
    return out or [""]


# ---------------------------------------------------------------------------
# Loading and eligibility.
# ---------------------------------------------------------------------------
def iter_rows(study: str):
    with open(DATA / f"{study}.jsonl") as f:
        for line in f:
            yield json.loads(line)


def is_constrained(r: dict) -> bool:
    return "Constrained" in (r["treatment"].get("assigned_group_label") or "")


def arm(r: dict) -> str:
    cls = r["treatment"]["persuader_class"]
    return "ai_constrained" if cls == "ai" and is_constrained(r) else cls


def total_words(r: dict) -> int:
    ms = r["conversation"]["message_stats"]
    return (ms.get("persuader_n_words_total") or 0) + (ms.get("persuadee_n_words_total") or 0)


def eligible(study: str, cls: str, r: dict) -> bool:
    t, p = r["treatment"], r["persuadee"]
    if arm(r) != cls or t.get("issue_id") in EXCLUDE_ISSUES:
        return False
    if not p.get("successfully_matched") or p.get("attrited"):
        return False
    pre = (p.get("pre_treatment") or {}).get("pre_attitude_average")
    post = (p.get("post_treatment") or {}).get("post_attitude_average")
    if pre is None or post is None:
        return False
    ms = r["conversation"]["message_stats"]
    return (ms.get("n_persuadee_messages") or 0) >= MIN_PE_MSGS and total_words(r) <= WORD_CAP[(study, cls)]


def outcome(study: str, r: dict) -> float | None:
    if study == "study_4":
        return r["outcomes"].get("donation_amount")
    return r["outcomes"].get("primary_attitude_change_signed")


def shuffled(rows: list[dict], key: str) -> list[dict]:
    rows = sorted(rows, key=lambda r: r["row_id"])
    random.Random(f"{SEED}:{key}").shuffle(rows)
    return rows


def draw(rows: list[dict], n: int, log: list[dict], stratum: str) -> list[dict]:
    picked = []
    for rank, r in enumerate(rows, 1):
        if len(picked) >= n:
            break
        reason = REJECTED.get(r["row_id"])
        log.append({"stratum": stratum, "draw_rank": rank, "row_id": r["row_id"],
                    "status": f"rejected: {reason}" if reason else "selected"})
        if not reason:
            picked.append(r)
    return picked


# ---------------------------------------------------------------------------
# Plain-text transcripts (released in the project repository).
# ---------------------------------------------------------------------------
def model_name(r: dict) -> str:
    t = r["treatment"]
    return (t.get("assigned_group_label") or t.get("ai_model") or "").replace(" (Constrained)", "")


def persuader_name(r: dict) -> str:
    cls = arm(r)
    if cls == "ai":
        return f"AI ({model_name(r)})"
    if cls == "ai_constrained":
        return f"Constrained AI ({model_name(r)})"
    return LABEL[cls]


def speaker(r: dict) -> str:
    return {"ai": "AI", "ai_constrained": "Constrained AI"}.get(arm(r), LABEL.get(arm(r), ""))


def attitudes(r: dict) -> tuple[float, float]:
    p = r["persuadee"]
    return p["pre_treatment"]["pre_attitude_average"], p["post_treatment"]["post_attitude_average"]


def format_conversation(study: str, r: dict) -> str:
    t, p = r["treatment"], r["persuadee"]
    L = ["=" * 78, f"{persuader_name(r)}  |  {study.replace('_', ' ').title()}  |  {r['row_id']}", "=" * 78, ""]
    L.append(f"Statement argued for: {clean_text(t.get('issue_stance'))}")
    if study == "study_4":
        will = p["pre_treatment"].get("pre_donation_willingness")
        L.append(f"Willingness to donate beforehand: {round(will, 1) if will is not None else 'n/a'} / 100")
        L.append(f"Share of the GBP 1 bonus donated afterwards: {outcome(study, r):.0f}%")
    else:
        pre, post = attitudes(r)
        L.append(f"Agreement before -> after (0-100): {pre:.1f} -> {post:.1f}  (shift {outcome(study, r):+.1f})")
    L += ["", "-" * 78, ""]
    for m in r["conversation"]["messages"]:
        content = clean_text(m.get("content"))
        if not content:
            continue
        L.append("PERSUADEE:" if m["speaker"] == "persuadee" else f"{persuader_name(r).upper()}:")
        for para in content.split("\n"):
            L += ["  " + ln for ln in wrap(para.strip())] if para.strip() else [""]
        L.append("")
    return "\n".join(L).rstrip() + "\n"


def manifest_row(study: str, r: dict, name: str) -> dict:
    t, p = r["treatment"], r["persuadee"]
    row = {"file": name, "study": study, "persuader_class": arm(r), "persuader": persuader_name(r),
           "issue": t["issue_id"], "statement": clean_text(t.get("issue_stance")),
           "pre_attitude": "", "post_attitude": "", "attitude_change": "",
           "pre_donation_willingness": "", "donation_pct": "", "row_id": r["row_id"]}
    if study == "study_4":
        will = p["pre_treatment"].get("pre_donation_willingness")
        row["pre_donation_willingness"] = round(will, 1) if will is not None else ""
        row["donation_pct"] = round(outcome(study, r), 1)
    else:
        pre, post = attitudes(r)
        row.update(pre_attitude=round(pre, 1), post_attitude=round(post, 1),
                   attitude_change=round(outcome(study, r), 1))
    return row


def flag_lines(name: str, text: str) -> list[str]:
    hits = []
    for i, ln in enumerate(text.split("\n"), 1):
        for kind, rx in (("PII", PII_FLAG_RE), ("SENSITIVE", SENSITIVE_FLAG_RE)):
            if rx.search(ln):
                hits.append(f"{name}:{i}: [{kind}] {ln.strip()}")
    return hits


# ---------------------------------------------------------------------------
# LaTeX for the printed transcripts.
# ---------------------------------------------------------------------------
TEX_CHARS = {
    "\\": r"\textbackslash{}", "{": r"\{", "}": r"\}", "$": r"\$", "&": r"\&",
    "#": r"\#", "_": r"\_", "%": r"\%", "^": r"\textasciicircum{}",
    "~": r"\textasciitilde{}", "<": r"\textless{}", ">": r"\textgreater{}",
    "|": r"\textbar{}", "\u2018": "`", "\u2019": "'", "\u201c": "``", "\u201d": "''",
    "\u2014": "---", "\u2013": "--", "\u2212": "-", "\u2026": r"\ldots{}",
    "\u00a3": r"\pounds{}", "\u20ac": r"\texteuro{}", "\u00d7": r"$\times$",
    "\u2192": r"$\rightarrow$", "\u2248": r"$\approx$", "\u2265": r"$\geq$",
    "\u2264": r"$\leq$", "\u00b0": r"\textdegree{}", "\u2009": r"\,",
    "\u200b": "", "\u2022": r"\textbullet{}",
}


def tex_escape(s: str) -> str:
    out = []
    for i, ch in enumerate(s):
        prev = s[i - 1] if i else " "
        opening = prev.isspace() or prev in "([{"
        if ch == '"':
            out.append("``" if opening else "''")
        elif ch == "'" and opening:
            out.append("`")
        elif ch in TEX_CHARS:
            out.append(TEX_CHARS[ch])
        elif ord(ch) > 0xFF:
            raise ValueError(f"no LaTeX mapping for {ch!r} (U+{ord(ch):04X}) in: {s[:80]}")
        else:
            out.append(ch)
    return "".join(out)


def tex_conversation(r: dict) -> list[str]:
    pre, post = attitudes(r)
    change = outcome("study_2", r)
    L = [f"\\subsubsection*{{{tex_escape(persuader_name(r))}}}", "",
         f"Persuadee attitude (0--100) before $\\rightarrow$ after: {pre:.1f} $\\rightarrow$ {post:.1f} "
         f"(change ${change:+.1f}$)", "",
         "\\begin{description}[leftmargin=1.5em, labelindent=0pt, itemsep=0.4\\baselineskip]"]
    for m in r["conversation"]["messages"]:
        content = clean_text(m.get("content"))
        if not content:
            continue
        who = "Persuadee" if m["speaker"] == "persuadee" else speaker(r)
        paras = [tex_escape(p.strip()) for p in content.split("\n") if p.strip()]
        L.append(f"\\item[{who}:] " + "\n\n".join(paras))
    L += ["\\end{description}", ""]
    return L


def write_tex(triplet: list[dict], path: Path) -> None:
    L = ["% Generated by code/analysis/py/si_12_example_conversations.py; do not edit.", ""]
    for r in triplet:
        L += tex_conversation(r)
    path.write_text("\n".join(L))


# ---------------------------------------------------------------------------
def main() -> None:
    if SUPP.exists():
        shutil.rmtree(SUPP)
    (SUPP / "transcripts").mkdir(parents=True)
    RESULTS_PY.mkdir(parents=True, exist_ok=True)
    TABLES_SI.mkdir(parents=True, exist_ok=True)

    pools: dict[tuple[str, str], list[dict]] = {}
    for study, cls in WORD_CAP:
        pools[(study, cls)] = [r for r in iter_rows(study) if eligible(study, cls, r)]

    log: list[dict] = []
    selected: list[tuple[str, dict]] = []
    triplets: dict[str, list[dict]] = {}

    study_2_arms = ("ai", "ai_constrained", "coached_elite_debater")
    issues = sorted(set.intersection(*({r["treatment"]["issue_id"] for r in pools[("study_2", a)]}
                                       for a in study_2_arms)))
    for issue in issues:
        for cls in study_2_arms:
            rows = [r for r in pools[("study_2", cls)] if r["treatment"]["issue_id"] == issue]
            for r in draw(shuffled(rows, f"study_2:{cls}:{issue}"), 1, log, f"study_2:{cls}:{issue}"):
                selected.append(("study_2", r))
                triplets.setdefault(issue, []).append(r)
    for cls in ("ai", "canvasser"):
        for r in draw(shuffled(pools[("study_4", cls)], f"study_4:{cls}"), N_DONATION, log, f"study_4:{cls}"):
            selected.append(("study_4", r))

    manifest, flags = [], []
    for study, r in selected:
        who = {"ai": "ai", "ai_constrained": "aiconstrained"}.get(arm(r), "human")
        name = f"{study}_{who}_{r['treatment']['issue_id']}_{r['row_id'].split(':')[1][-6:]}.txt"
        text = format_conversation(study, r)
        (SUPP / "transcripts" / name).write_text(text)
        flags += flag_lines(name, text)
        manifest.append(manifest_row(study, r, name))

    with open(SUPP / "manifest.csv", "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(manifest[0]))
        w.writeheader()
        w.writerows(manifest)
    with open(RESULTS_PY / "si_12_example_draw_log.csv", "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(log[0]))
        w.writeheader()
        w.writerows(log)
    (RESULTS_PY / "si_12_example_flags.txt").write_text("\n".join(flags) + "\n")

    printed = min(issues, key=lambda i: sum(total_words(r) for r in triplets[i]))
    write_tex(triplets[printed], TABLES_SI / "tab_example_conversations.tex")

    print(f"issues (Study 2 triplets): {issues}")
    print(f"{len(selected)} transcripts written to {SUPP}; {len(flags)} lines flagged for manual review")
    print(f"printed in the SI: {printed} "
          f"({sum(total_words(r) for r in triplets[printed])} words)")


if __name__ == "__main__":
    main()
