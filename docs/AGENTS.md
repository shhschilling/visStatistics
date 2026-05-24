# AGENTS.md instructions

- Keep this `AGENTS.md` active in memory for every task in this project.
- Minimal edits to existing code.
- Minimal edits to existing texts in Rmd or other text format.
- Less wordy. Use bullet points where useful.
- Avoid long, nonsensical, or speculative responses.
- All claims based on citations with verifiable DOIs. Check DOIs
  internally before displaying.
- Never edit `vignettes/REFERENCES.bib`; the user maintains the
  references through Zotero.

## References And Source PDFs

- Do not edit `vignettes/REFERENCES.bib`; the user maintains references
  through Zotero.
- In the vignette appendix, when defining a test statistic, cite the
  original paper.
- In the paper, cite only test statistics that are implemented by the
  package itself.
- Zotero PDF attachments are stored under
  `/Users/sschilli/Library/CloudStorage/OneDrive-HochschuleLuzern/storage/`.
- When checking citation support, first inspect the BibTeX
  `file = {...}` field in `vignettes/REFERENCES.bib`, then read the
  linked local PDF from the Zotero storage folder.
- If no local PDF is linked or readable, report that before suggesting a
  replacement citation.
- Do not infer citation support from title or memory alone.

## Manuscript Editing Protocol

- Before proposing changes to the paper, read the whole current paper
  source, not only the local paragraph, and keep the full structure,
  existing definitions, figure references, appendices, and neighbouring
  sections in view.
- Before editing vignette text, or auxiliary Rmd intended for the
  vignette, read the current whole vignette source and keep existing
  definitions, notation, and neighbouring sections in view.
- Do not repeat notation definitions in appendix/table material when the
  vignette already defines them.
- Work one paragraph at a time for Rmd/text edits.
- Never change code without asking first, unless the user explicitly
  says “apply” for that code change.
- Do not write insertion-point notes inside draft Rmd files; provide
  insertion suggestions only in the chat.
- Before drafting, restate the factual decision logic in 3 short
  bullets.
- Do not rewrite surrounding structure unless explicitly asked.
- Verify the mechanism in code or paper before wording technical claims.
- State the precise technical claim first, then compress without losing
  the mechanism.
- Optimise manuscript suggestions for precision, flow, lack of
  redundancy, and contextual overview of the whole package.
- For statistical manuscript claims, state the exact scope, the object
  being tested, the competing mechanisms, and the package-specific
  selling point.
- Keep claim scope narrow: package-wide, branch-specific, output, or
  limitation.
- Prefer narrower claims when uncertain.
- Use existing manuscript terminology where possible.
- Do not invent new technical terms in paper or vignette writing; stay
  as close as possible to the user’s wording.
- Every word in existing manuscript text counts. Do not invent, shorten,
  or replace wording unless the user explicitly asks for that exact
  change.
- If wording is unclear, orphaned, or has a weak referent, flag the
  problem before changing it; do not invent a bridge sentence.
- For manuscript and vignette edits, always show a diff before changing
  anything, unless the user explicitly says “apply”.
- Avoid function calls in abstracts.
- Show a diff before applying unless the user explicitly says “apply”.
- Wrap proposed manuscript diffs and text snippets at about 80
  characters per line so they are readable in chat.
- In proposed manuscript diffs, wrap both removed (`-`) and added (`+`)
  lines at about 80 characters where possible; do not show unreadably
  long deletion lines.
- Preserve the statistical mechanism before improving prose.

## Surgical Manuscript Editing

- Before suggesting manuscript wording, read the current paragraph and
  the relevant code branch.
- Do not infer package behaviour from memory.
- Do not rewrite; propose the smallest possible textual change.
- If the user says “raus”, do not patch unless the user also says
  “apply”.
- Stay as close as possible to the current wording.
- Do not paraphrase unless explicitly asked.
- Avoid AI-style polishing.
- Make only surgical edits to the requested sentence, paragraph, or
  heading.
- Preserve existing sentence logic, terminology, and wording order where
  possible.
- Change headings, order, or framing only when explicitly asked.
- When moving or rewording text, keep the whole paragraph and
  neighbouring context in view.
- Recheck connectors after edits; do not leave stale transitions such as
  “the same issue” when the referent has changed.

## Full CLAUDE.md Content

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

## Terminology rules

- **Never abbreviate “general linear model” as “GLM”.** In statistics,
  “GLM” universally denotes *generalized* linear model (different model
  class: link function, exponential-family error distribution). Always
  write “general linear model” in full. This rule applies to prose,
  abstracts, headings, and figure captions. Section labels and code
  identifiers (e.g. `#sec:glm`) are exempt because they are never
  rendered as visible text.

## Thesis check before flagging

- Before flagging any notation, terminology, or symbol re-use as
  inconsistent, restate the paper’s central unifying claim in one line.
- If the apparent inconsistency is actually the unified expression of
  that claim (e.g. `k` denoting both groups in one-way ANOVA and
  parameters in the general linear model, because they coincide under
  the general linear model unification), do not flag.
- If you cannot articulate the connection to the paper’s thesis, do not
  propose the edit.
- Same check applies to apparent prose redundancy, repeated phrases, and
  visual/numerical separations: ask first whether the repetition or
  coupling is itself the contribution being made.
