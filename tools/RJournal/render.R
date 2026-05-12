# Render the R Journal submission from any working directory.
# Source this file from the package root or any subdirectory:
#
#   source("tools/RJournal/render.R")
#
# Both PDF and HTML output are written to tools/RJournal/.
# Requires: rmarkdown, rjtools, rprojroot, tinytex

if (!requireNamespace("rprojroot", quietly = TRUE)) {
  stop("Please install rprojroot: install.packages('rprojroot')", call. = FALSE)
}

# ── 1. Install all LaTeX packages required by the rjtools template ───────────
#    tinytex::tlmgr_install() selects a working mirror automatically.
rj_latex_pkgs <- c(
  # fonts
  "palatino", "mathpazo", "inconsolata", "ae",
  # layout & spacing
  "setspace", "geometry", "parskip", "fancyhdr", "footmisc", "lastpage",
  # references & links
  "natbib", "hyperref", "doi", "url",
  # tables & figures
  "booktabs", "caption", "float", "enumitem", "subfig",
  # misc
  "microtype", "etoolbox", "xcolor", "upquote", "listings",
  "environ", "trimspaces", "pgf", "ms"
)

tinytex::tlmgr_install(rj_latex_pkgs)

# ── 2. Locate the Rmd and render ──────────────────────────────────────────────
pkg_root <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))
rj_dir   <- normalizePath(file.path(pkg_root, "tools", "RJournal"))
rmd_file <- file.path(rj_dir, "visStatistics-RJournal.Rmd")

if (!file.exists(rmd_file)) {
  stop("Cannot find: ", rmd_file, call. = FALSE)
}

rmarkdown::render(
  input         = rmd_file,
  output_format = "all",
  output_dir    = rj_dir,
  knit_root_dir = rj_dir
)
