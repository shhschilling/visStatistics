## effect_size_refs.R -----------------------------------------------------------
##
## The effect-size table in children/_appendix.Rmd is emitted as raw LaTeX and
## raw HTML, which pandoc passes through untouched, so citation keys placed
## inside it are never resolved and its sources never reach the bibliography.
##
## effect_size_nocite() derives the keys instead: it collects every DOI and URL
## linked in the table, matches them against REFERENCES.bib, and returns the
## corresponding keys as a nocite string. Adding, removing or re-sourcing a row
## therefore updates the reference list automatically.
##
## Used from the vignette YAML header:
##   nocite: '`r source("effect_size_refs.R"); effect_size_nocite()`'
## -------------------------------------------------------------------------------

effect_size_nocite <- function(appendix = "children/_appendix.Rmd",
                               bib = "REFERENCES.bib") {
  if (!file.exists(appendix) || !file.exists(bib)) {
    return("")
  }
  a <- readLines(appendix, warn = FALSE)

  ## links carried by the table: \tbdoi{doi}{label}, \href{url}{label},
  ## and the <a href="..."> of the HTML branch
  doi <- unlist(regmatches(a, gregexpr("(?<=\\\\tbdoi\\{)[^}]+", a, perl = TRUE)))
  latex_url <- unlist(regmatches(a, gregexpr("(?<=\\\\href\\{)[^}]+", a, perl = TRUE)))
  html_url <- unlist(regmatches(a, gregexpr('(?<=<a href=")[^"]+', a, perl = TRUE)))
  targets <- unique(c(doi, latex_url, html_url))
  targets <- targets[!grepl("^#", targets)] # drop internal anchors
  targets <- sub("^https?://doi\\.org/", "", targets)
  if (!length(targets)) {
    return("")
  }

  ## split the bibliography into entries
  b <- readLines(bib, warn = FALSE)
  starts <- grep("^@", b)
  if (!length(starts)) {
    return("")
  }
  keys <- sub("^@[^{]+\\{([^,]+),.*$", "\\1", b[starts])
  ends <- c(starts[-1] - 1L, length(b))

  hit <- character(0)
  for (i in seq_along(starts)) {
    entry <- paste(b[starts[i]:ends[i]], collapse = " ")
    if (any(vapply(targets, grepl, logical(1), x = entry, fixed = TRUE))) {
      hit <- c(hit, keys[i])
    }
  }
  if (!length(hit)) {
    return("")
  }
  paste0("@", sort(unique(hit)), collapse = ", ")
}
