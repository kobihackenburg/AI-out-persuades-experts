# Elite Debater Achievements — Coding Rubric

**Purpose.** The 56 elite debaters in Study 1 self-reported their debating
achievements as free text in response to the PRE-survey item
`debating-achievements`:

> *"Please describe your most impressive debating achievements. This can include
> your place or rank at world championships, affiliation with university debating
> teams (e.g., Oxford, Cambridge), or other notable accomplishments in
> competitive debating. Please be as specific as possible."*

This file defines the binary criteria each free-text response is coded against
to produce the headline cohort descriptors used in the manuscript.

The coding is done by an LLM (see `code/analysis/elite_debater/annotate.py`) applying
the rules below; outputs land in
`code/analysis/elite_debater/audit.csv` (one row per debater, with each
criterion as `TRUE`/`FALSE` plus a one-line justification).

## Major continental championships

Throughout the rubric, **"continental championship"** refers to one of the five
recognized continental university-debating championships:

| Region | Championship | Common abbreviations |
|---|---|---|
| Europe | European Universities Debating Championships | EUDC, "Euros", "European Championship(s)" |
| North America | North American Universities Debating Championships | NAUDC, "NorthAms", "North American Championships" |
| Australasia / Asia-Pacific | Australasian Intervarsity Debating Championships | Australs, APDC, "Australasian Championships", "Asia-Pacific Debating Championship" |
| Asia | United Asians Debating Championships | UADC, "Asian BP", "Asian British Parliamentary Debating Championships", "Asian (Debating) Championship" |
| Africa | Pan-African Universities Debate Championships | PAUDC, "Pan-African (Universities) Debating Championships" |

These competitions typically run multiple parallel divisions:

* **Open** — the unrestricted main competition (the default when no division
  is specified).
* **ESL** — English-as-a-Second-Language (a parallel competition for non-native
  English speakers).
* **EFL** — English-as-a-Foreign-Language (parallel competition; less English
  exposure than ESL).
* **WGM / WAGMC** — Women & Gender Minorities (a separate parallel competition
  in the Australasian region in particular).

Other tournaments mentioned by debaters but **not** counted as continental
championships under this rubric:

* **Tournaments / IVs**: Oxford IV, Cambridge IV, Yale IV, LSE Open, Princeton
  IV, Hart House IV, McGill IV, KCL IV, Vienna IV, etc. — these are
  invitational tournaments, not continental championships.
* **National championships**: Australian Easters / "Australian Intervarsity",
  US Universities Debating Championships (USUDC) / "US Nationals", Israeli
  Nationals, German Nationals, "Scottish champion", etc. — these are
  national-level, not continental.
* **WSDC** — World Schools Debating Championship — is a *high-school*
  competition, not the WUDC (university) world championship.
* **Round-robin / invitationals** like HWS Round Robin — invitational
  tournaments, not championships.

Also note: at WUDC and continental championships, a "**top breaking team**"
or "broke first" means the team with the best record in the preliminary rounds
(prelims) — this is **not** the same as winning the championship; the team
still has to win the elimination bracket.

---

## Tournament progression vocabulary

For most major debating tournaments, the elimination bracket runs:

> **Octofinals (Top 16) → Quarterfinals (Top 8) → Semifinals (Top 4) → Final (Top 2)**

* "Reached / made the **semifinals**" = lost in semis (Top 4, did not reach
  final).
* "**Finalist** / Grand Finalist / Open Finalist" = reached the final, may have
  won or lost.
* "**Champion** / Won / Winner" = won the final.

So **"reached at least the semifinals"** = semifinalist *or* finalist *or*
champion. **"Reached at least the finals"** = finalist *or* champion.

Some tournaments (e.g., recent WUDC) have an extra "partial-double-octofinals"
(PD-octos) round before octofinals; "PD-octofinalist" is below octofinals and
well below semis.

---

## Binary criteria (one column per criterion in the audit CSV)

For each criterion, the LLM outputs `TRUE` or `FALSE` and a one-line
justification quoting the relevant fragment of the response.

### 1. `is_world_champion`

**Definition.** Won the **Open** division of the **World Universities Debating
Championship (WUDC)** in any year.

**Include** (TRUE):
* "I was the 2024 World Champion at WUDC"
* "WUDC 2021 — Open Champion"
* "Champion, World Universities Debating Championship 2022"
* "I won WUDC in Vietnam in 2023" — TRUE *only* if Open is implied or
  explicitly stated; if a later sentence clarifies it was the ESL division,
  set to FALSE.

**Exclude** (FALSE):
* WUDC ESL Champion or EFL Champion (different divisions).
* WSDC Champion (high-school world championship — not WUDC).
* "World #1 best speaker" or "best individual speaker at WUDC" without a team
  championship (that's a speaker award, see criterion 6).
* WUDC finalist, semifinalist, quarterfinalist, octofinalist.
* Top breaking team, best speaker tab placement.

### 2. `is_continental_champion`

**Definition.** Won the **Open** division of one of the five continental
championships (EUDC, NAUDC, Australasian/APDC, UADC, PAUDC).

**Include** (TRUE):
* "I won the European Universities Debating Championships in 2024"
* "I have won the Australasian Intervarsity Debating Championships"
* "I was the North American Universities Debating Champion in 2025"
* "NAUDC champion"
* "UADC Champion 2025"
* "Pan African Universities Debate Championship winner"
* "Asian Champion ... (Asian British Parliamentary Debating Championships)"

**Exclude** (FALSE):
* ESL / EFL / WGM division wins (those go in
  `is_continental_champion_any_division`, not here).
* "Top breaking team at EUDC" — top of prelims, not Open champion.
* "EUDC finalist" or any semifinalist / quarterfinalist.
* Tournament wins (Oxford IV, Cambridge IV, etc.).
* National championship wins (US Nationals, Australian Easters / Intervarsity,
  Israeli Nationals, etc.).

### 3. `is_continental_champion_any_division`

**Definition.** Won **any titled division** (Open, ESL, EFL, WGM/WAGMC) at any
of the five continental championships.

**Include** (TRUE):
* Anything that satisfies criterion 2.
* "ESL Champion at Glasgow EUDC 2024"
* "EUDC 2021 ESL Champion"
* "Australasian Women and Gender Minorities Champion 2024"
* "Won the Australasian Women and Gender Minorities Championship"
* "I'm a former English-as-Second-Language European Champion"

**Exclude** (FALSE):
* Tournament-level wins (Oxford IV, Cambridge IV, etc.).
* National championship wins (US Nationals, Australian Easters, etc.).
* WSDC (school) wins, in any division.
* Finalists/semifinalists in any division.
* "Top breaking team" / "broke first".

### 4. `reached_continental_semis_or_better`

**Definition.** Reached at least the **Open semifinals** (i.e., semifinalist,
finalist, or champion of the Open division) at any of the five continental
championships.

**Include** (TRUE):
* "I have reached the semi finals of the European Debating Championships"
* "Open Semifinalist at the European Universities Debating Championships"
* "Open Finalist at Copenhagen EUDC 2025"
* Anything that satisfies criterion 2 (Open champion implies semifinalist or
  better).

**Exclude** (FALSE):
* Continental quarterfinals, octofinals, partial-double-octofinals, or
  "broke" (advanced past prelims) without a specified semis-or-better stage.
* Open Finals/semis at WUDC (that's world, not continental — see criterion 5).
* ESL/EFL/WGM division semis or finals (Open only for this criterion).
* Tournament semifinals (Oxford IV, Cambridge IV, Yale IV, etc.).
* "Top breaking team" / "broke first" — that's a prelim achievement.

### 5. `reached_major_semis_or_better`

**Definition.** Reached at least the Open semifinals (semifinalist, finalist,
or champion) at WUDC **or** at any of the five continental championships.

This is the **cohort inclusion criterion** that the manuscript references:
*"all of whom had reached at least the semifinals of a major international
debating competition."* It should be `TRUE` for the great majority of the 56
debaters; if it is `FALSE` for any debater, that debater fails the inclusion
criterion as currently described.

**Include** (TRUE):
* Anything that satisfies criterion 4 (continental semis-or-better).
* "Open Semifinal at WUDC 2021"
* "WUDC quarter / semi / final / Champion".
* "Three-time WUDC Grand Finalist".

**Exclude** (FALSE):
* Only WUDC quarterfinalist (or below) AND only continental quarterfinalist
  (or below).
* ESL/EFL/WGM-only major semis/finals if no Open major semis-or-better is
  reported.

### 6. `is_best_speaker_at_major`

**Definition.** Was the **#1 best speaker** (top of the Open speaker tab) at
WUDC OR at any of the five continental championships, in any year.

**Include** (TRUE):
* "WUDC 2022 Best Speaker"
* "best individual speaker (world #1) at WUDC 2024"
* "Best Speaker of Asia in 2024 (Asian British Parliamentary Debating
  Championships)"
* "ranked best speaker at Euros"
* "best speaker of Australs"

**Exclude** (FALSE):
* Top-N best speakers ("8th best speaker", "5th best speaker", "Top 10
  speaker"). The criterion is the #1 spot specifically.
* Best speaker in a *sub-division* ("Best ESL speaker", "best EFL speaker",
  "2nd best EFL speaker") — those are sub-division speaker awards.
* Best speaker at a tournament ("Best speaker at Oxford IV", "best speaker at
  Cambridge IV", "Best Speaker of LSE IV").
* "Best speaker of Serbian delegation" or other regional/national-team
  rankings.

### 7. `is_continental_finalist`

**Definition.** Reached the **Open finals** (finalist or champion) of one of
the five continental championships.

**Include** (TRUE):
* "Open Finalist at Copenhagen EUDC 2025"
* "Australian (Australasian) Open Finalist 2024"
* "I was a grand finalist ... at the Asia Pacific Debating Championship
  (Australs)"
* Anything that satisfies criterion 2 (Open champion implies finalist or
  better).

**Exclude** (FALSE):
* Continental semifinalists, quarterfinalists, etc.
* ESL/EFL/WGM finalists.
* Tournament finalists (Oxford IV finalist, Cambridge IV finalist, etc.).
* WUDC finalists (that's world, not continental).
* "Top breaking team" / "broke first".

---

## Output schema

For each of the 56 debaters, the annotator outputs JSON of the form:

```json
{
  "persuader_id": "6880e932cf...",
  "is_world_champion":                       true,
  "is_world_champion_evidence":              "I was the 2024 World Champion (and 8th best speaker) at WUDC.",
  "is_continental_champion":                 false,
  "is_continental_champion_evidence":        "EUDC 2023 Semifinalist (not champion).",
  "is_continental_champion_any_division":    false,
  "is_continental_champion_any_division_evidence": "EUDC 2023 Semifinalist; no continental division wins reported.",
  "reached_continental_semis_or_better":     true,
  "reached_continental_semis_or_better_evidence":  "2023 European Semifinalist (Open).",
  "reached_major_semis_or_better":           true,
  "reached_major_semis_or_better_evidence":  "WUDC 2024 Champion implies major semis or better.",
  "is_best_speaker_at_major":                false,
  "is_best_speaker_at_major_evidence":       "8th best speaker at WUDC, but not #1.",
  "is_continental_finalist":                 false,
  "is_continental_finalist_evidence":        "EUDC 2023 Semifinalist (not finalist)."
}
```

A short evidence quote/explanation accompanies each boolean.

The aggregator then collapses the per-debater JSON into the audit CSV
(`code/analysis/elite_debater/audit.csv`) and emits per-criterion totals
into `_numbers.json` under the `debater.*` namespace.
