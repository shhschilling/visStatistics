# AGENTS.md instructions

- Minimal edits to existing code.
- Minimal edits to existing texts in Rmd or other text format.
- Less wordy. Use bullet points where useful.
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
- Avoid function calls in abstracts.
- Show a diff before applying unless the user explicitly says “apply”.
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
