## Combined, aligned Route 1 power figure.

if (!requireNamespace("patchwork", quietly = TRUE)) {
  stop("Package 'patchwork' is required.")
}

source_env <- function(path) {
  env <- new.env(parent = globalenv())
  sys.source(path, envir = env)
  env
}

power_env <- source_env(
  "dev/codexsimulation20160603_gamma_skew_sweep_4groups_power_plot.R"
)
pdf_env <- source_env(
  "dev/codexsimulation20160607_gamma_skew_sweep_4groups_power_pdf.R"
)

combined <- patchwork::wrap_plots(
  pdf_env$p,
  power_env$p_power,
  ncol = 1,
  heights = c(1, 2.35)
)

outfile <- file.path(
  "vignettes",
  "figures",
  "gamma_skew_sweep_4groups_power_with_pdf.png"
)
ggplot2::ggsave(outfile, combined, width = 14, height = 11.4, dpi = 360)

message("Wrote: ", outfile)
