args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1L]]) else getwd()
script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
project_root <- normalizePath(file.path(script_dir, "..", ".."))

vignette_file <- file.path(project_root, "vignettes", "visStatistics.Rmd")
appendix_file <- file.path(project_root, "vignettes", "_appendix.md")
rjournal_dir <- file.path(project_root, "tools", "RJournal")

main_file <- file.path(rjournal_dir, "visStatistics-RJournal.Rmd")
supp_file <- file.path(rjournal_dir, "Supplement.Rmd")

read_utf8 <- function(path) {
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

write_utf8 <- function(x, path) {
  writeLines(enc2utf8(x), path, useBytes = TRUE)
}

collapse <- function(x) paste(x, collapse = "\n")

replace_fixed <- function(text, pattern, replacement) {
  gsub(pattern, replacement, text, fixed = TRUE)
}

supplement_link <- '`r supplement_link(NULL, "the Supplement")`'
supplement_link_cap <- '`r supplement_link(NULL, "Supplement")`'

vignette <- read_utf8(vignette_file)

abstract_start <- grep("^# Abstract$", vignette)
intro_start <- grep("^# Introduction$", vignette)
appendix_start <- grep("^```\\{r appendix-child", vignette)

if (length(abstract_start) != 1L ||
    length(intro_start) != 1L ||
    length(appendix_start) != 1L) {
  stop("Could not identify Abstract, Introduction, or appendix child.")
}

abstract <- vignette[(abstract_start + 1L):(intro_start - 1L)]
abstract <- abstract[nzchar(trimws(abstract))]
abstract <- paste(abstract, collapse = " ")

main_body <- vignette[intro_start:(appendix_start - 1L)]
main_text <- collapse(main_body)

previous_text <- NULL
while (!identical(previous_text, main_text)) {
  previous_text <- main_text
  main_text <- gsub("(?s)<!--.*?-->", "", main_text, perl = TRUE)
}

main_text <- replace_fixed(
  main_text,
  "To reduce dependencies on other packages, `visStatistics` implements `levene.test()` for the variance gate in grouped mean-based tests (Eq. \\@ref(eq:levene-f)), `bp.test()` for regression diagnostics (Eq. \\@ref(eq:breusch-pagan-bp)), and `games.howell()` for Welch-ANOVA post-hoc comparisons (Eq. \\@ref(eq:games-howell-t)).",
  paste(
    "To reduce dependencies on other packages, `visStatistics` implements",
    "`levene.test()` for the variance gate in grouped mean-based tests,",
    "`bp.test()` for regression diagnostics, and `games.howell()` for",
    paste0("Welch-ANOVA post-hoc comparisons; their definitions are given in ",
           supplement_link, "."),
    sep = "\n"
  )
)

main_text <- replace_fixed(
  main_text,
  paste(
    "Definitions of all implemented test statistics, rank-correlation coefficients,",
    "and effect sizes are given in Appendices",
    "\\@ref(sec:tests)--\\@ref(sec:rank-correlations).",
    sep = "\n"
  ),
  paste(
    "Definitions of all implemented test statistics, rank-correlation coefficients,",
    paste0("and effect sizes are given in ", supplement_link, "."),
    sep = "\n"
  )
)

main_text <- replace_fixed(main_text, "Appendix \\@ref(sec:effect-size)",
                           supplement_link)
main_text <- replace_fixed(main_text, "Appendix\n\\@ref(sec:effect-size)",
                           supplement_link)
main_text <- replace_fixed(main_text, "Appendices \\@ref(sec:tests)--\\@ref(sec:rank-correlations)",
                           supplement_link)
main_text <- replace_fixed(main_text, "Section \\@ref(sec:gh)",
                           supplement_link)
main_text <- replace_fixed(main_text, "Section \\@ref(sec:cooks-distance)",
                           supplement_link)

supp_eq_labels <- c(
  "shapiro-w", "anderson-a2", "levene-f", "bartlett-k2",
  "breusch-pagan-bp", "wilcoxon-w", "kruskal-h", "student-t",
  "fisher-f", "tukey-hsd-q", "welch-t", "welch-f",
  "games-howell-t", "kendall-tau-b", "spearman-rho",
  "r-squared", "pearson-chi", "pearson-residual",
  "leverage-simple-regression"
)

for (label in supp_eq_labels) {
  ref_nb <- paste0("Eq. \\@ref(eq:", label, ")")
  ref_sp <- paste0("Eq. \\@ref(eq:", label, ")")
  ref_nl_nb <- paste0("Eq. \n\\@ref(eq:", label, ")")
  ref_nl_sp <- paste0("Eq. \n\\@ref(eq:", label, ")")
  main_text <- replace_fixed(main_text, paste0("(", ref_nb, ")"),
                             paste0("(see ", supplement_link_cap, ")"))
  main_text <- replace_fixed(main_text, paste0("(", ref_sp, ")"),
                             paste0("(see ", supplement_link_cap, ")"))
  main_text <- replace_fixed(main_text, paste0("(", ref_nl_nb, ")"),
                             paste0("(see ", supplement_link_cap, ")"))
  main_text <- replace_fixed(main_text, paste0("(", ref_nl_sp, ")"),
                             paste0("(see ", supplement_link_cap, ")"))
  main_text <- replace_fixed(main_text, paste0("; ", ref_nb),
                             paste0("; see ", supplement_link_cap))
  main_text <- replace_fixed(main_text, paste0("; ", ref_sp),
                             paste0("; see ", supplement_link_cap))
  main_text <- replace_fixed(main_text, paste0("; ", ref_nl_nb),
                             paste0("; see ", supplement_link_cap))
  main_text <- replace_fixed(main_text, paste0("; ", ref_nl_sp),
                             paste0("; see ", supplement_link_cap))
  main_text <- replace_fixed(main_text, ref_nb, supplement_link_cap)
  main_text <- replace_fixed(main_text, ref_sp, supplement_link_cap)
  main_text <- replace_fixed(main_text, ref_nl_nb, supplement_link_cap)
  main_text <- replace_fixed(main_text, ref_nl_sp, supplement_link_cap)
}

main_text <- replace_fixed(
  main_text,
  paste(
    "**Route 3** For simple linear regression, the diagnostic panel displays",
    "the residual histogram, the normal Q--Q plot, z-scaled residuals versus",
    "fitted values, and z-scaled residuals versus leverage",
    paste0("(", supplement_link_cap, " with Cook's distance contours computed"),
    paste0("on the same z-residual scale (see ", supplement_link, ")."),
    sep = " "
  ),
  paste(
    "**Route 3** For simple linear regression, the diagnostic panel displays",
    "the residual histogram, the normal Q--Q plot, z-scaled residuals versus",
    "fitted values, and z-scaled residuals versus leverage, with Cook's",
    "distance contours computed on the same z-residual scale",
    paste0("(see ", supplement_link, ")."),
    sep = " "
  )
)

main_text <- gsub(
  "Section[[:space:]\n]+\\\\@ref\\((sec:[^)]+)\\)",
  "`r section_link(\"\\1\")`",
  main_text,
  perl = TRUE
)

main_text <- replace_fixed(
  main_text,
  paste(
    "The Route 1 and Route 3 diagnostic-panel designs are illustrated in",
    "the examples in Figures \\@ref(fig:welch-anova-example), left, and",
    "\\@ref(fig:regression-example), left.",
    sep = " "
  ),
  paste(
    "The Route 1 and Route 3 diagnostic-panel designs are illustrated in",
    "the examples in Figures \\@ref(fig:welch-anova-example), top, and",
    "\\@ref(fig:regression-example), top.",
    sep = " "
  )
)

main_text <- replace_fixed(
  main_text,
  "The diagnostic panel in Figure \\@ref(fig:regression-example), left,",
  "The diagnostic panel in Figure \\@ref(fig:regression-example), top,"
)

remove_chunk_option <- function(options, name) {
  option_pattern <- paste0(
    ",[[:space:]]*",
    name,
    "[[:space:]]*=[[:space:]]*(\"[^\"]*\"|'[^']*'|[^,}]+)"
  )
  gsub(option_pattern, "", options, perl = TRUE)
}

stack_visstat_chunk <- function(text, label, file_slug) {
  chunk_pattern <- paste0("(?s)```\\{r ", label, "([^}]*)\\}\\n(.*?)\\n```")
  match <- regexec(chunk_pattern, text, perl = TRUE)
  parts <- regmatches(text, match)[[1L]]
  if (length(parts) == 0L) {
    stop("Could not find chunk: ", label, call. = FALSE)
  }

  options <- parts[[2L]]
  body <- parts[[3L]]
  for (option in c("echo", "results", "out\\.width", "fig\\.height",
                   "fig\\.show")) {
    options <- remove_chunk_option(options, option)
  }
  options <- paste0(options, ", echo=FALSE, out.width=\"100%\"")

  body_lines <- strsplit(body, "\n", fixed = TRUE)[[1L]]
  visstat_line <- grep("<-[[:space:]]*visstat\\(", body_lines)
  if (length(visstat_line) != 1L) {
    stop("Expected exactly one visstat assignment in chunk: ", label,
         call. = FALSE)
  }
  result_name <- sub(
    "^[[:space:]]*([[:alnum:]_.]+)[[:space:]]*<-[[:space:]]*visstat\\(.*$",
    "\\1",
    body_lines[[visstat_line]]
  )
  body_lines[[visstat_line]] <- sub(
    "<-[[:space:]]*visstat\\((.*)\\)[[:space:]]*$",
    "<- capture_visstat_result(visstat(\\1))",
    body_lines[[visstat_line]],
    perl = TRUE
  )

  panel_file <- paste0(result_name, "_panel_file")
  body_lines <- c(
    body_lines,
    paste0(panel_file, " <- file.path(tempdir(), \"", file_slug,
           "_panel.png\")"),
    paste0("make_visstat_panel(", result_name, ", ", panel_file, ")"),
    paste0("knitr::include_graphics(", panel_file, ")")
  )

  replacement <- paste0(
    "```{r ", label, options, "}\n",
    paste(body_lines, collapse = "\n"),
    "\n```"
  )
  start <- match[[1L]][[1L]]
  length <- attr(match[[1L]], "match.length")[[1L]]
  paste0(
    substr(text, 1L, start - 1L),
    replacement,
    substr(text, start + length, nchar(text))
  )
}

for (chunk in list(
  c("student-ttest-example", "student_ttest_example"),
  c("ttest-example", "ttest_example"),
  c("welch-anova-example", "welch_anova_example"),
  c("wilcoxon-example", "wilcoxon_example"),
  c("kruskal-example", "kruskal_example"),
  c("regression-example", "regression_example"),
  c("ozone-lm-triage", "ozone_lm_triage")
)) {
  main_text <- stack_visstat_chunk(main_text, chunk[[1L]], chunk[[2L]])
}

rjournal_setup <- c(
  "```{r setup, include=FALSE}",
  "rj_dir <- normalizePath(dirname(knitr::current_input(dir = TRUE)))",
  "library(visStatistics)",
  "knitr::opts_knit$set(root.dir = rj_dir)",
  "knitr::opts_chunk$set(",
  "  fig.width  = 7,",
  "  fig.height = 4.5,",
  "  out.width  = \"100%\",",
  "  echo = TRUE",
  ")",
  "example_alpha <- 0.05",
  "combine_panel_images <- function(img_a, img_b, file,",
  "                                 labels = c(\"A\", \"B\")) {",
  "  png(file, width = 1200, height = 1600, res = 120)",
  "  on.exit(dev.off())",
  "  par(mar = c(0, 0, 0, 0))",
  "  plot.new()",
  "  plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = \"i\", yaxs = \"i\")",
  "  rasterImage(as.raster(img_a), 0, 0.52, 1, 0.98)",
  "  rasterImage(as.raster(img_b), 0, 0.02, 1, 0.48)",
  "  text(0.02, 0.98, labels[1], font = 2, cex = 1.8, adj = c(0, 1))",
  "  text(0.02, 0.48, labels[2], font = 2, cex = 1.8, adj = c(0, 1))",
  "  invisible(file)",
  "}",
  "capture_visstat_result <- function(expr, width = 1400, height = 900,",
  "                                   res = 200) {",
  "  tmp <- tempfile(fileext = \".png\")",
  "  png(tmp, width = width, height = height, res = res)",
  "  dev.control(\"enable\")",
  "  result <- tryCatch(eval.parent(substitute(expr)), finally = dev.off())",
  "  result",
  "}",
  "capture_visstat_image <- function(result, which, width = 1400,",
  "                                  height = 900, res = 200) {",
  "  tmp <- tempfile(fileext = \".png\")",
  "  png(tmp, width = width, height = height, res = res)",
  "  dev.control(\"enable\")",
  "  tryCatch(plot(result, which = which), finally = dev.off())",
  "  png::readPNG(tmp)",
  "}",
  "make_visstat_panel <- function(result, file, labels = c(\"A\", \"B\")) {",
  "  if (!requireNamespace(\"png\", quietly = TRUE)) {",
  "    stop(\"The png package is required to build panel figures.\")",
  "  }",
  "  if (length(attr(result, \"captured_plots\")) < 2) {",
  "    stop(\"Expected at least two captured plots.\")",
  "  }",
  "  img_a <- capture_visstat_image(result, 1)",
  "  img_b <- capture_visstat_image(result, 2)",
  "  combine_panel_images(img_a, img_b, file, labels = labels)",
  "}",
  "supplement_link <- function(anchor = NULL, text = \"Supplement\") {",
  "  if (knitr::is_latex_output()) {",
  "    sprintf(\"\\\\href{Supplement.pdf}{%s}\", text)",
  "  } else {",
  "    suffix <- if (is.null(anchor)) \"\" else paste0(\"#\", anchor)",
  "    sprintf(\"[%s](Supplement.html%s)\", text, suffix)",
  "  }",
  "}",
  "section_link <- function(label, text = \"Section\") {",
  "  if (knitr::is_latex_output()) {",
  "    sprintf(\"\\\\hyperref[%s]{%s~\\\\ref*{%s}}\", label, text, label)",
  "  } else {",
  "    sprintf(\"[%s](#%s)\", text, label)",
  "  }",
  "}",
  "```"
)

yaml <- c(
  "---",
  "title: \"visStatistics: The right test, visualised\"",
  "author:",
  "  - name: Sabine Schilling",
  "    affiliation: Lucerne School of Business, Lucerne University of Applied Sciences and Arts",
  "    address:",
  "    - 6002 Lucerne, Switzerland",
  "    url: https://github.com/shhschilling/visStatistics",
  "    orcid: 0000-0002-8318-9421",
  "    email: sabineschilling@gmx.ch",
  "abstract: >",
  paste0("  ", abstract),
  "date: \"2026-05-27\"",
  "packages:",
  "  cran:",
  "    - visStatistics",
  "    - Cairo",
  "    - multcompView",
  "    - nortest",
  "    - png",
  "    - vcd",
  "  bioc: []",
  "output:",
  "  rjtools::rjournal_pdf_article:",
  "    self_contained: yes",
  "    toc: no",
  "  rjtools::rjournal_web_article:",
  "    self_contained: yes",
  "    toc: no",
  "bibliography: ../../vignettes/REFERENCES.bib",
  "link-citations: true",
  "header-includes:",
  "  - \\microtypesetup{nopatch=footnote}",
  "  - \\usepackage{array}",
  "  - \\usepackage{mathrsfs}",
  "editor_options:",
  "  markdown:",
  "    wrap: 72",
  "---"
)

main_rmd <- c(
  yaml,
  "",
  "<!-- Generated by tools/RJournal/build_paper_split.R; edit vignettes/visStatistics.Rmd. -->",
  "",
  rjournal_setup,
  "",
  paste0("Definitions of test statistics, rank-correlation coefficients, ",
         "influence diagnostics, and effect-size formulae are provided in ",
         supplement_link, "."),
  "",
  strsplit(main_text, "\n", fixed = TRUE)[[1L]]
)

write_utf8(main_rmd, main_file)

appendix <- read_utf8(appendix_file)
if (identical(appendix[1L], "# (APPENDIX) Appendix {.unnumbered}")) {
  appendix <- appendix[-1L]
}
appendix_text <- collapse(appendix)
appendix_text <- gsub(
  "```\\{=latex\\}\\n\\\\numberwithin\\{equation\\}\\{section\\}\\n```\\n*",
  "",
  appendix_text
)
appendix_text <- replace_fixed(
  appendix_text,
  "In `visStatistics`, the contours are drawn on the z residual scale used in the\ndiagnostic panel. With $z_i$ defined in Eq.\\ \\@ref(eq:z-residual), Cook's\ndistance for observation $i$ is",
  "In `visStatistics`, the contours are drawn on the z residual scale used in the\ndiagnostic panel. With $z_i = e_i / SE_\\text{res}$, Cook's distance for\nobservation $i$ is"
)
appendix_text <- replace_fixed(
  appendix_text,
  "Simple linear regression, Section~\\ref{sec:lin-reg} &",
  "Simple linear regression &"
)
appendix_text <- replace_fixed(
  appendix_text,
  "| [Simple linear regression](#sec:lin-reg) |",
  "| Simple linear regression |"
)

supp_yaml <- c(
  "---",
  "title: \"Supplement: visStatistics\"",
  "author:",
  "  - name: Sabine Schilling",
  "    affiliation: Lucerne School of Business, Lucerne University of Applied Sciences and Arts",
  "    address:",
  "    - 6002 Lucerne, Switzerland",
  "    url: https://github.com/shhschilling/visStatistics",
  "    orcid: 0000-0002-8318-9421",
  "    email: sabineschilling@gmx.ch",
  "date: \"2026-05-27\"",
  "output:",
  "  rjtools::rjournal_pdf_article:",
  "    self_contained: yes",
  "    toc: no",
  "  rjtools::rjournal_web_article:",
  "    self_contained: yes",
  "    toc: no",
  "bibliography: ../../vignettes/REFERENCES.bib",
  "link-citations: true",
  "header-includes:",
  "  - \\microtypesetup{nopatch=footnote}",
  "  - \\usepackage{array}",
  "  - \\usepackage{mathrsfs}",
  "  - \\renewcommand{\\theequation}{S.\\arabic{equation}}",
  "  - \\renewcommand{\\thefigure}{S\\arabic{figure}}",
  "  - \\renewcommand{\\thetable}{S\\arabic{table}}",
  "editor_options:",
  "  markdown:",
  "    wrap: 72",
  "---"
)

supp_setup <- c(
  "```{r setup, include=FALSE}",
  "rj_dir <- normalizePath(dirname(knitr::current_input(dir = TRUE)))",
  "knitr::opts_knit$set(root.dir = rj_dir)",
  "knitr::opts_chunk$set(echo = TRUE)",
  "main_article_link <- function(text = \"main article\") {",
  "  if (knitr::is_latex_output()) {",
  "    sprintf(\"\\\\href{visStatistics-RJournal.pdf}{%s}\", text)",
  "  } else {",
  "    sprintf(\"[%s](visStatistics-RJournal.html)\", text)",
  "  }",
  "}",
  "```"
)

supp_rmd <- c(
  supp_yaml,
  "",
  "<!-- Generated by tools/RJournal/build_paper_split.R; edit vignettes/_appendix.md. -->",
  "",
  supp_setup,
  "",
  paste0("This supplement accompanies `r main_article_link(\"the main article\")`."),
  "",
  strsplit(appendix_text, "\n", fixed = TRUE)[[1L]]
)

write_utf8(supp_rmd, supp_file)

figure_dir <- file.path(rjournal_dir, "figures")
dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)
for (figure in c("overview.png", "decision_tree.png")) {
  source <- file.path(project_root, "vignettes", "figures", figure)
  target <- file.path(figure_dir, figure)
  if (file.exists(source)) {
    file.copy(source, target, overwrite = TRUE)
  }
}

cat("Wrote:\n", main_file, "\n", supp_file, "\n", sep = "")
