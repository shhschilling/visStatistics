## Run visstat() for LG203 Team 2: prices by year.

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Package 'pkgload' is required.")
}

pkgload::load_all(".", quiet = TRUE)

dat_path <- file.path("dev", "LG203_Team2_visstat_year_price.rds")
out_dir <- file.path("dev", "LG203_Team2_visstat_year_price_outputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

dat <- readRDS(dat_path)
dat$year <- factor(dat$year)
dat$price_chf <- as.numeric(dat$price_chf)

result <- visstat(
  x = dat$year,
  y = dat$price_chf,
  graphicsoutput = "png",
  plotName = "LG203_Team2_price_by_year",
  plotDirectory = out_dir
)

print(result)
print(summary(result))
print(attr(result, "plot_paths"))
