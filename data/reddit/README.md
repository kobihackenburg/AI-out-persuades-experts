# AI-generated posts on r/PoliticalDiscussion (Pangram)

Data behind the SI Appendix section "AI-generated content on r/PoliticalDiscussion", cited in the Introduction:
in a random sample of 1,000 posts from the twelve months to 7 October 2026, Pangram estimates that 36% were at least
80% AI-generated.

**No post text.** Reddit's terms bar redistributing the posts, so this folder carries post ids, timestamps and
Pangram scores only. Each post can be looked up on Reddit by its id (`https://www.reddit.com/comments/<id>`), though
some will have been edited or deleted since.

```bash
python3 code/reddit/reddit_pangram.py   # or: make reddit
# 364 of 1,000 sampled posts (36.4%, rounded 36%) are at least 80% AI-generated; 95% CI 33.5 to 39.4%.
```

Python 3.10 or later, no third-party packages, no network access. The script checks the files, then writes
`results.json` and `results.md` (all tables) to `output/results/reddit/`.

## Files

| File | Contents |
|---|---|
| `..._sample1000_seed42.scores.jsonl` | One Pangram 4.0 result per sampled post: id, `created_utc`, word count, `fraction_ai`, `fraction_ai_assisted`, `fraction_human`, Pangram's verdict, and per-window labels and character offsets (window text removed). |
| `..._sample1000_seed42.meta.json` | Run record of the scrape: window, counts, cleaning counts and the sampling record (seed, 1,000 of 1,197 eligible of 10,348). |
| `..._sample1000_seed42.pangram.meta.json` | Run record of the scoring sessions. |

## How the data were produced

1. **Scrape.** All posts submitted to r/PoliticalDiscussion from 2025-10-08 to 2026-10-08 (exclusive), from the
   [Arctic Shift](https://github.com/ArthurHeitmann/arctic_shift) archive, on 2026-10-08: 10,348 posts. The archive
   captures each post once, about 20 seconds after submission.
2. **Clean** (`code/reddit/pangram_pipeline.py clean`). HTML entities decoded; zero-width characters, quoted lines,
   link URLs and bold or italic markers removed.
3. **Sample** (`pangram_pipeline.py sample -n 1000 --seed 42`). Eligible posts have a body that is not `[removed]` or
   `[deleted]`, are not by a moderator or AutoModerator, and have text: 1,197 of 10,348 (9,148 were already
   `[removed]` when captured, 2 were moderator posts, 1 was empty). 1,000 drawn without replacement.
4. **Score** (`pangram_pipeline.py score`). Title and body sent to Pangram on 2026-10-08, in two sessions (545, then
   455 posts). `fraction_ai` is the share of the text, by characters, in windows labelled AI-generated.

## Checks

Before release, `reddit_pangram.py` was run on the full files (with the post text) and confirmed that the sample is
1,000 unique, in-window, scoreable posts drawn with seed 42; that re-cleaning the text changes nothing; that each
Pangram result's windows add back up to exactly the title and body sent; and that a fresh seed-42 draw from the
12-month scrape reproduces the sample. On these files it re-runs every check that needs no text: the run records
describe a complete twelve-month scrape and scoring run, every post is scored once by Pangram 4.0, the fractions sum
to one, `fraction_ai` equals the AI character share of the windows, and 364 posts reach the threshold.

## Caveats

- **Denominator.** The sample is random among the 1,197 posts whose text the archive captured, not among all 10,348
  submissions: 9,148 were removed within seconds of submission, almost certainly by automated moderation. The figure
  describes the posts readers could see.
- **Threshold.** The 364 posts are exactly those Pangram's own verdict labels `AI`. The share is 40.1% at a 50%
  threshold, 33.7% for posts labelled AI-generated in full, and 37.8% counting AI-assisted text too
  (`fraction_ai + fraction_ai_assisted >= 0.8`).
- **Re-scoring.** Pangram updates its model, so scoring the posts again would give somewhat different results.
