# Render the manually edited R Journal submission from any working directory.
# Source this file from the package root or any subdirectory:
#
#   source("tools/helperfunctions/renderRjournal.R")
#
# Both PDF and HTML output are written to tools/RJournal/.
# HTML is opened in the default browser. On macOS, a stable /private/tmp
# copy of the PDF is opened in Skim, which refreshes changed PDFs.
# Requires: rmarkdown, rjtools, rprojroot, tinytex

if (!requireNamespace("rprojroot", quietly = TRUE)) {
  stop("Please install rprojroot: install.packages('rprojroot')", call. = FALSE)
}

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Please install devtools: install.packages('devtools')", call. = FALSE)
}




# ── 2. Locate the Rmd and render ──────────────────────────────────────────────
pkg_root <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))
rj_dir   <- normalizePath(file.path(pkg_root, "tools", "RJournal"))
rmd_file <- file.path(rj_dir, "visStatistics-RJournal.Rmd")
supp_file <- file.path(rj_dir, "Supplement.Rmd")
pdf_file <- file.path(rj_dir, "visStatistics-RJournal.pdf")
html_file <- file.path(rj_dir, "visStatistics-RJournal.html")

if (!file.exists(rmd_file)) {
  stop("Cannot find: ", rmd_file, call. = FALSE)
}
if (!file.exists(supp_file)) {
  stop("Cannot find: ", supp_file, call. = FALSE)
}

devtools::load_all(pkg_root, quiet = TRUE)

supp_html_out <- rmarkdown::render(
  input         = supp_file,
  output_format = "rjtools::rjournal_web_article",
  output_dir    = rj_dir,
  knit_root_dir = rj_dir
)

supp_pdf_out <- rmarkdown::render(
  input         = supp_file,
  output_format = "rjtools::rjournal_pdf_article",
  output_dir    = rj_dir,
  knit_root_dir = rj_dir
)

html_out <- rmarkdown::render(
  input         = rmd_file,
  output_format = "rjtools::rjournal_web_article",
  output_dir    = rj_dir,
  knit_root_dir = rj_dir
)

pdf_out <- rmarkdown::render(
  input         = rmd_file,
  output_format = "rjtools::rjournal_pdf_article",
  output_dir    = rj_dir,
  knit_root_dir = rj_dir
)

html_file <- normalizePath(html_out, mustWork = TRUE)
pdf_file <- normalizePath(pdf_out, mustWork = TRUE)
supp_html_file <- normalizePath(supp_html_out, mustWork = TRUE)
supp_pdf_file <- normalizePath(supp_pdf_out, mustWork = TRUE)

if (.Platform$OS.type == "unix" && Sys.info()[["sysname"]] == "Darwin") {
  invisible(system2("xattr", c("-d", "com.apple.quarantine", pdf_file),
                    stdout = FALSE, stderr = FALSE))
}

open_file <- function(file, app = NULL) {
  if (.Platform$OS.type == "unix" && Sys.info()[["sysname"]] == "Darwin") {
    args <- if (is.null(app)) file else c("-a", app, file)
    status <- system2("open", args, stdout = FALSE, stderr = FALSE)
    return(identical(status, 0L))
  }

  browseURL(file)
  TRUE
}

if (.Platform$OS.type == "unix" && Sys.info()[["sysname"]] == "Darwin") {
  pdf_open_file <- "/private/tmp/visStatistics-RJournal.pdf"
  if (!file.copy(pdf_file, pdf_open_file, overwrite = TRUE)) {
    warning("Could not copy PDF to: ", pdf_open_file, call. = FALSE)
    pdf_open_file <- pdf_file
  }
  invisible(system2("xattr", c("-d", "com.apple.quarantine", pdf_open_file),
                    stdout = FALSE, stderr = FALSE))
} else {
  pdf_open_file <- pdf_file
}

if (open_file(html_file)) {
  message("Opened HTML: ", html_file)
}

message("Rendered supplement HTML: ", supp_html_file)
message("Rendered supplement PDF: ", supp_pdf_file)

if (open_file(pdf_open_file, "Skim")) {
  message("Opened PDF in Skim: ", pdf_open_file)
} else {
  if (!open_file(pdf_open_file, "Preview")) {
    if (open_file(pdf_open_file)) {
      message("Opened PDF in default viewer: ", pdf_open_file)
    }
  } else {
    message("Opened PDF in Preview: ", pdf_open_file)
  }
}
