# AGENTS.md instructions

- **Always refer to the latest version on disk.** Before quoting,
  diffing, proposing, or applying any edit, re-read the target file.
  Never rely on in-memory copies from earlier turns — the user may have
  edited the file manually in the meantime.
- Minimal edits to existing code.
- Minimal edits to existing texts in Rmd or other text format.
- Less wordy. Use bullet points where useful.
- All claims based on citations with verifiable DOIs. Check DOIs
  internally before displaying.

## Manuscript Editing Protocol

- Before flagging any claim as wrong, verify against the source code.
- Default assumption is intentional design. Only flag with high
  confidence.
- Reviews are judged by signal, not volume. Produce few high-confidence
  findings, not exhaustive lists. If you cannot articulate why a flagged
  issue would actually mislead a careful reader, do not flag it.
- “Reviewer might ask” is not enough. The bar is “a careful reviewer
  will ask” — i.e. the gap is real, not hypothetical.
- Do not proceed to the next task without being explicitly asked.
- When the user says “stop”, stop immediately — do not complete the
  current action.
- Work one paragraph at a time for Rmd/text edits.
- Before drafting, restate the factual decision logic in 3 short
  bullets.
- Do not rewrite surrounding structure unless explicitly asked.
- Draft for correctness first, then compress.
- Keep claim scope narrow: package-wide, branch-specific, output, or
  limitation.
- Prefer narrower claims when uncertain.
- Use existing manuscript terminology where possible.
- Avoid function calls in abstracts.
- Show a diff before applying unless the user explicitly says “apply”.
- Preserve the statistical mechanism before improving prose.
- Re-read the target lines on every edit turn; do not trust in-memory
  file state across user turns.
- No edit unless a concrete defect can be named. “Smoother”, “tighter”,
  or stylistic preference is not enough. If the original reads cleanly,
  say so and propose no change.
- Inference is not redundancy. Only flag a sentence as redundant with
  prior text if the prior text states the same claim literally, not by
  implication.
- Preserve existing manuscript terminology verbatim when smoothing. Do
  not replace established phrases (e.g. “enter automated routing”) with
  weaker verbs (“do”, “do so”) for compression.
- Diff first, rationale second. Keep rationale to three lines maximum;
  no per-substitution bullet lists, no “Notes:” headings.
- One proposal per turn, with conviction. If uncertain, propose “leave
  as-is” instead of floating a weaker alternative.

## Citation Rules

- **Never cite a source that has not been read and verified.** Do not
  suggest, display, or insert any citation — including BibTeX entries —
  unless the full text has been accessed and the relevant claim
  confirmed. Web search snippets, abstracts, and secondary sources do
  not count.
- **Vignette appendix**: when defining a test statistic, cite the
  original paper.
- **R Journal paper**: only cite original papers for test statistics
  implemented by the package author
  ([`levene.test()`](https://shhschilling.github.io/visStatistics/reference/levene.test.md),
  [`bp.test()`](https://shhschilling.github.io/visStatistics/reference/bp.test.md),
  [`games.howell()`](https://shhschilling.github.io/visStatistics/reference/games.howell.md)).
  Base R / stats package functions are not cited with their original
  papers.

## Worktree rule

- All approved edits go to the main source tree of the project, never to
  the worktree at `.claude/worktrees/epic-pike-e0d2dc/`.

## Archive before structural edits

- Before any structural reordering of `vignettes/visStatistics.Rmd`
  (e.g. moving sections, merging Decision logic with Examples, large
  refactors), first copy the current file to
  `visstatisticsArchive/YYYYMMDD_visStatistics.Rmd` using today’s date
  in `YYYYMMDD` format. This matches the existing convention
  (e.g. `20260518_visStatistics.Rmd`).
- Only after the archive copy is in place, re-read the source from disk
  and begin the reordering.
- This rule applies to structural edits only, not to small prose or
  paragraph-level smoothing.

## R idioms — not discrepancies

- [`aov()`](https://rdrr.io/r/stats/aov.html) is a wrapper around
  [`lm()`](https://rdrr.io/r/stats/lm.html); the two describe the same
  fit. Do not flag wording like “fits using
  [`aov()`](https://rdrr.io/r/stats/aov.html)” vs “fits with
  [`lm()`](https://rdrr.io/r/stats/lm.html)” as a contradiction.
