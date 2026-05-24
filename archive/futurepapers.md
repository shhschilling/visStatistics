# Future Paper Ideas

## One-page clinical note

Working title:

> Routine statistics in clinical score development: why the test choice should be visible

Best target:

- Clinical research network newsletter
- Clinical methods society blog
- Local or project-linked clinician-facing research bulletin

Less suitable:

- R Journal: already covered by the main software paper.
- JMIR Formative Research: too large if the intended piece is only one page.
- Another software journal: would duplicate the R Journal article.

Core angle:

- Clinical score-development projects often need routine two-variable tests.
- The problem is not advanced modelling, but informal test choice.
- A browser/server workflow can let clinicians inspect assumption diagnostics,
  selected tests, effect sizes where returned, and plots without using R directly.
- The note should cite the R Journal article as the full technical software
  reference.

Suggested structure:

1. Clinical problem
   Routine comparisons and associations arise repeatedly during score
   development, but the valid test depends on assumptions.

2. Workflow
   Clinicians use a browser interface; R runs on the server; output remains
   reproducible and inspectable.

3. Why it matters
   The value is not a rare method. The value is making ordinary statistical
   choices visible during collaborative clinical work.

4. Pointer
   The full package implementation and decision logic are described in the
   R Journal software paper.

Do not reuse:

- Full routing trees.
- All worked examples.
- S3 method details.
- Package architecture text.

Possible title variant:

> Assumption-aware routine statistics in the browser: a short note for clinical score development
