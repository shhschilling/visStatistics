## ---------------------------------------------------------------------------
## The two alternatives on one page, homoscedastic designs only.
##
##   A  density strip   mu = (0, 0.25, 0.50, 0.75)   increasing trend
##   B  power           balanced homoscedastic
##   C  power           unbalanced homoscedastic
##   D  density strip   mu = (0, 0, 0, delta)        one-point
##   E  power           balanced homoscedastic
##   F  power           unbalanced homoscedastic
##
## Both halves are sigma = (1, 1, 1, 1), so the only thing that differs between
## them is the shape of the alternative. They are NOT matched in effect size and
## cannot be: the trend gives balanced above unbalanced, the one-point gives
## balanced below unbalanced at every delta, so no delta matches both rows.
## omega^2 is therefore printed on every row header and the halves are read at
## their own effect sizes.
##
## Kruskal-Wallis is the only rank test drawn, matching what visstat_core()
## selects.
##
## Usage:
##   Rscript figure_power_homoscedastic_combined.R [DELTA]
## DELTA defaults to 0.5 and selects the one-point grid.
##
## Output:
##   fleishman_4groups_power_homoscedastic_trend_vs_onepoint_d<delta>.png
## ---------------------------------------------------------------------------

for (pkg in c("ggplot2", "patchwork", "scales", "ggtext")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Package '", pkg, "' is required.")
}

SIMDIR <- local({
  here <- getwd()
  if (file.exists(file.path(here, "fleishman_route1_residual_helpers.R"))) here
  else {
    installed <- system.file("simulations", package = "visStatistics")
    if (!nzchar(installed)) stop("Run from inst/simulations/.")
    installed
  }
})
source(file.path(SIMDIR, "fleishman_route1_residual_helpers.R"))
source(file.path(SIMDIR, "fleishman_figure_typography.R"))
source(file.path(SIMDIR, "omega_scaling_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)
DELTA <- if (length(args) >= 1) as.numeric(args[1]) else 0.5
if (!is.finite(DELTA) || DELTA <= 0) stop("DELTA must be a positive number.")
DELTA_TAG <- sub("\\.", "", format(DELTA, nsmall = 2))

ggplot2 <- asNamespace("ggplot2"); patchwork <- asNamespace("patchwork")
scales <- asNamespace("scales")

D_BAL <- "balanced n, equal SD"
D_UNB <- "unbalanced n, equal SD"
DESIGN_WORDS <- c("balanced homoscedastic", "unbalanced homoscedastic")
names(DESIGN_WORDS) <- c(D_BAL, D_UNB)
NMULT <- c("1, 1, 1, 1", "0.5, 0.8, 1.2, 1.5")
names(NMULT) <- names(DESIGN_WORDS)
SD_EQ <- c(1, 1, 1, 1)

TREND_SHIFTS <- c(0, 0.25, 0.50, 0.75)
POINT_SHIFTS <- c(0, 0, 0, DELTA)

## ---- inputs -----------------------------------------------------------------
## The trend half: its homoscedastic cells were never rerun under the Brunner
## SD vectors because sigma = (1,1,1,1) is common to both sets, so they are the
## cells of the existing grid.
TREND_FILE <- file.path(SIMDIR, "fleishman_4groups_power.rds")
if (!file.exists(TREND_FILE)) stop("Input not found: ", basename(TREND_FILE))
trend <- readRDS(TREND_FILE)
trend <- trend[trend$design %in% names(DESIGN_WORDS), , drop = FALSE]

POINT_FILE <- file.path(
  SIMDIR, sprintf("fleishman_4groups_power_design_brunner_onepoint_d%s_B50000.csv",
                  DELTA_TAG))
if (!file.exists(POINT_FILE)) stop("Input not found: ", basename(POINT_FILE))
point <- read.csv(POINT_FILE, stringsAsFactors = FALSE)
point <- point[point$design %in% names(DESIGN_WORDS), , drop = FALSE]

## Confirm each half really carries the shift vector its strip will draw.
check_shifts <- function(d, shifts, what) {
  s <- unique(d$group_mean_offsets)
  if (length(s) != 1) stop(what, " holds more than one shift vector")
  got <- as.numeric(strsplit(s, ",[ ]*")[[1]])
  if (!isTRUE(all.equal(got, shifts, tolerance = 1e-8))) {
    stop(what, " holds shifts (", paste(got, collapse = ", "),
         ") but the strip would draw (", paste(shifts, collapse = ", "), ")")
  }
}
check_shifts(trend, TREND_SHIFTS, "the trend grid")
check_shifts(point, POINT_SHIFTS, "the one-point grid")

keep <- c("design", "n_per_group", "panel", "fisher_power", "welch_power",
          "mean_power", "rank_power", "sw_power", "gate_power",
          "route_fisher_probability", "route_welch_probability",
          "route_rank_probability")
for (nm in c("trend", "point")) {
  d <- get(nm)
  miss <- setdiff(keep, names(d))
  if (length(miss)) stop("columns absent from the ", nm, " grid: ",
                         paste(miss, collapse = ", "))
  assign(nm, d[, keep])
}

## eta_H^2 per (design, panel), from eta_h_own_derivation.R. Each half is
## labelled from its own grid.
ETA_FILE <- file.path(SIMDIR, "eta_h_own_by_design_panel.csv")
if (!file.exists(ETA_FILE)) {
  stop("eta_h_own_by_design_panel.csv not found; run eta_h_own_derivation.R first.")
}
ETA_ALL <- read.csv(ETA_FILE, stringsAsFactors = FALSE)
eta_for <- function(grid) {
  e <- ETA_ALL[ETA_ALL$grid == grid & ETA_ALL$design %in% names(DESIGN_WORDS), ,
               drop = FALSE]
  if (nrow(e) != length(DESIGN_WORDS) * 5) {
    stop("eta_h_own_by_design_panel.csv holds ", nrow(e), " homoscedastic rows ",
         "for grid \"", grid, "\", expected ", length(DESIGN_WORDS) * 5, ".")
  }
  e
}
ETA_TREND <- eta_for("legacy")
ETA_POINT <- eta_for(sprintf("brunner_onepoint_d%s", DELTA_TAG))

H_PDF <- 1
H_POWER <- 2
BAND_SCALE <- 2 / H_POWER

PANELS <- sort(unique(point$panel))
NS_TO_PLOT <- c(10, 20, 30, 50, 100)
groups <- c("A", "B", "C", "D")
xlim <- c(-2.5, 5); y_cap <- 0.7

STRATS <- c("1. Fisher always" = "F", "2. Welch always" = "W",
            "3. Levene-gated Fisher/Welch" = "L", "4. Kruskal-Wallis always" = "KW",
            "5. Shapiro-Wilk routed Welch/KW" = "SW",
            "6. Shapiro-Wilk plus Levene" = "SW+L")
COLS <- c("#B79F00", "#56B4E9", "#009E73", "#000000", "#D55E00", "#0072B2")
SHP <- c(0, 2, 5, 4, 1, 1); SZ <- c(4, 3.2, 3.6, 3.8, 5.8, 7.2)
names(COLS) <- names(SHP) <- names(SZ) <- names(STRATS)
COLUMN <- c("fisher_power", "welch_power", "mean_power", "rank_power",
            "sw_power", "gate_power")
names(COLUMN) <- names(STRATS)
USE <- names(STRATS)

omega_of <- function(design_name, shifts) population_omega_sq(
  as.numeric(strsplit(NMULT[[design_name]], ",[ ]*")[[1]]), SD_EQ, shifts, 1)
regime_of <- function(design_name) omega_sq_regime(
  as.numeric(strsplit(NMULT[[design_name]], ",[ ]*")[[1]]), SD_EQ)

panel_title <- function(p) {
  one <- fleishman_cases[fleishman_cases$panel == p, , drop = FALSE]
  if (p == 1) return("N(0, 1)\n\n\nskew = 0\nexcess kurtosis = 0")
  sprintf(paste("Fleishman polynomial", "a = %.3f, b = %.3f", "c = -a, d = %.3f",
                "skew = %s", "excess kurtosis = %s", sep = "\n"),
          one$a, one$b, one$d, one$skew, one$excess_kurtosis)
}

make_pdf_panel <- function(shifts, letter, description) {
  num <- function(x) format(round(x, 3), trim = TRUE, drop0trailing = TRUE)
  lab <- sprintf("%s (mean shift = %s, SD = 1)", groups, num(shifts))
  names(lab) <- groups
  lev <- vapply(PANELS, panel_title, character(1))
  rows <- list(); i <- 1
  for (p in PANELS) for (j in seq_along(shifts)) {
    x <- seq(xlim[1], xlim[2], length.out = 700)
    d <- fleishman_scaled_density(x, p, sd = SD_EQ[j], shift = shifts[j])
    d[!is.finite(d)] <- NA_real_
    rows[[i]] <- data.frame(distribution = panel_title(p), group = groups[j],
                            x = x, density = d, stringsAsFactors = FALSE); i <- i + 1
  }
  dat <- do.call(rbind, rows); dat$distribution <- factor(dat$distribution, levels = lev)
  nums <- data.frame(distribution = factor(lev, levels = lev),
                     number = paste0(PANELS, ")"), x = xlim[1], y = y_cap)
  refs <- do.call(rbind, lapply(PANELS, function(p) {
    one <- fleishman_cases[fleishman_cases$panel == p, , drop = FALSE]
    data.frame(distribution = factor(panel_title(p), levels = lev),
               group = factor(rep(groups, 2), levels = groups),
               value = c(shifts, one$a * SD_EQ + shifts),
               line_type = factor(rep(c("mean", "median"), each = 4),
                                  levels = c("mean", "median")))
  }))
  ggplot2$ggplot(dat, ggplot2$aes(x = x, y = density, colour = group, group = group)) +
    ggplot2$geom_vline(data = refs,
      ggplot2$aes(xintercept = value, colour = group, linetype = line_type),
      linewidth = 0.42, alpha = 0.75, inherit.aes = FALSE,
      show.legend = c(colour = FALSE, linetype = TRUE)) +
    ggplot2$geom_line(linewidth = 0.7, na.rm = TRUE) +
    ggplot2$geom_text(data = nums, ggplot2$aes(x = x, y = y, label = number),
      inherit.aes = FALSE, family = FLEISHMAN_FONT_FAMILY, hjust = 0, vjust = -2.0,
      size = FLEISHMAN_GEOM_TEXT$panel_number) +
    ggplot2$facet_wrap(~distribution, nrow = 1) +
    ggplot2$coord_cartesian(xlim = xlim, ylim = c(0, y_cap), clip = "off") +
    ggplot2$scale_colour_manual(values = fleishman_group_cols, breaks = groups,
      labels = lab, name = "group,\ngroup mean offset") +
    ggplot2$scale_linetype_manual(values = c(mean = "dashed", median = "dotted"),
      name = "reference line") +
    ggplot2$labs(title = fleishman_panel_title(letter, description),
                 x = "Response value with group shift", y = "Theoretical density") +
    ggplot2$theme_minimal(base_size = 10) +
    ggplot2$theme(
      strip.text = ggplot2$element_text(size = FLEISHMAN_TEXT$strip,
        family = FLEISHMAN_FONT_FAMILY, lineheight = FLEISHMAN_LINEHEIGHT$panel_title),
      legend.title = ggplot2$element_text(size = FLEISHMAN_TEXT$legend),
      legend.text = ggplot2$element_text(size = FLEISHMAN_TEXT$legend),
      axis.title = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_title),
      axis.text = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_text),
      plot.title = ggtext::element_markdown(hjust = 0,
        size = FLEISHMAN_TEXT$panel_letter, family = FLEISHMAN_FONT_FAMILY),
      plot.title.position = "plot",
      text = ggplot2$element_text(family = FLEISHMAN_FONT_FAMILY))
}

make_power_panel <- function(power, eta, shifts, design_name, letter) {
  dat <- power[power$design == design_name, , drop = FALSE]
  if (!nrow(dat)) stop("no rows for ", design_name)
  dat$power_panel <- factor(paste0(dat$panel, ")"), levels = paste0(PANELS, ")"))

  e <- eta[eta$design == design_name, , drop = FALSE]
  if (nrow(e) != length(PANELS)) stop("eta_H^2 missing for ", design_name)
  e <- e[order(e$panel), ]
  eta_lab <- setNames(sprintf('"%d)"~~eta[H]^2 == "%.3f"', e$panel, e$eta_h_sq_own),
                      paste0(e$panel, ")"))
  dat$power_panel <- factor(unname(eta_lab[as.character(dat$power_panel)]),
                            levels = unname(eta_lab[paste0(PANELS, ")")]))

  long <- do.call(rbind, lapply(USE, function(s) {
    v <- dat[[COLUMN[[s]]]]
    if (all(is.na(v))) return(NULL)
    transform(dat, strategy = s, power = v)
  }))
  long$strategy <- factor(long$strategy, levels = names(STRATS))
  long <- subset(long, n_per_group %in% NS_TO_PLOT)

  base <- subset(dat, n_per_group %in% NS_TO_PLOT)
  INSET_HEADROOM <- 0.15 * BAND_SCALE
  inset_below <- min(long$power, na.rm = TRUE) < INSET_HEADROOM
  gate_ys <- BAND_SCALE * (if (inset_below) c(-0.075, -0.125, -0.175)
                           else c(0.085, 0.055, 0.025))
  title_y <- BAND_SCALE * (if (inset_below) -0.038 else 0.125)
  y_min <- if (inset_below) BAND_SCALE * -0.21 else 0
  hline_from <- if (inset_below) 0 else 0.2 * BAND_SCALE

  gv <- rbind(
    transform(base, gate_row = "F",  gate_y = gate_ys[1], gate_rate = route_fisher_probability),
    transform(base, gate_row = "W",  gate_y = gate_ys[2], gate_rate = route_welch_probability),
    transform(base, gate_row = "KW", gate_y = gate_ys[3], gate_rate = route_rank_probability))
  gv$lab <- sprintf("%.0f", 100 * gv$gate_rate)
  grows <- unique(gv[c("power_panel", "gate_row", "gate_y")])
  gtit <- unique(base["power_panel"]); gtit$t <- "SW+L selection (%)"

  header <- sprintf(paste0("power simulations, %s; (n<sub>1</sub>, n<sub>2</sub>, ",
      "n<sub>3</sub>, n<sub>4</sub>) = n&#772;(%s); (SD<sub>1</sub>, SD<sub>2</sub>, ",
      "SD<sub>3</sub>, SD<sub>4</sub>) = (1, 1, 1, 1); &omega;<sup>2</sup>",
      "<sub>%s</sub> = %.3f"),
    DESIGN_WORDS[[design_name]], NMULT[[design_name]],
    regime_of(design_name), omega_of(design_name, shifts))

  ggplot2$ggplot() +
    ggplot2$geom_vline(xintercept = NS_TO_PLOT, colour = "grey88", linewidth = 0.35) +
    ggplot2$geom_hline(yintercept = seq(hline_from, 1, by = 0.1), colour = "grey88",
                       linewidth = 0.35) +
    (if (inset_below)
       ggplot2$geom_hline(yintercept = -0.02 * BAND_SCALE, colour = "grey60",
                          linewidth = 0.3)
     else ggplot2$geom_blank()) +
    ggplot2$geom_point(data = long,
      ggplot2$aes(x = n_per_group, y = power, colour = strategy, shape = strategy,
                  size = strategy, group = strategy), stroke = 1.15) +
    ggplot2$geom_text(data = gtit, ggplot2$aes(x = 26, y = title_y, label = t),
      colour = "grey25", family = FLEISHMAN_FONT_FAMILY, size = FLEISHMAN_GEOM_TEXT$inset) +
    ggplot2$geom_text(data = grows, ggplot2$aes(x = 5.8, y = gate_y, label = gate_row),
      colour = "grey25", family = FLEISHMAN_FONT_FAMILY, size = FLEISHMAN_GEOM_TEXT$inset) +
    ggplot2$geom_text(data = gv,
      ggplot2$aes(x = n_per_group, y = gate_y, label = lab), colour = "grey25",
      family = FLEISHMAN_FONT_FAMILY, size = FLEISHMAN_GEOM_TEXT$inset) +
    ggplot2$facet_grid(stats::as.formula(". ~ power_panel"),
      labeller = ggplot2$label_parsed) +
    ggplot2$scale_y_continuous(limits = c(y_min, 1), breaks = seq(0, 1, by = 0.1),
                               labels = scales::percent) +
    ggplot2$scale_x_log10(breaks = NS_TO_PLOT, limits = c(5.5, 130)) +
    ggplot2$scale_shape_manual(values = SHP, breaks = USE, labels = STRATS[USE],
                               name = "test strategy") +
    ggplot2$scale_size_manual(values = SZ, breaks = USE, labels = STRATS[USE],
                              name = "test strategy") +
    ggplot2$scale_colour_manual(values = COLS, breaks = USE, labels = STRATS[USE],
                                name = "test strategy") +
    ggplot2$labs(title = fleishman_panel_title(letter, header),
                 x = "n per group", y = "simulated rejection rate") +
    ggplot2$theme_minimal(base_size = 10) +
    ggplot2$theme(
      legend.position = "right",
      legend.title = ggplot2$element_text(size = FLEISHMAN_TEXT$legend),
      legend.text = ggplot2$element_text(size = FLEISHMAN_TEXT$legend),
      axis.title = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_title),
      axis.text = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_text),
      axis.text.x = ggplot2$element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2$element_blank(),
      panel.border = ggplot2$element_rect(colour = "black", fill = NA, linewidth = 0.25),
      strip.background = ggplot2$element_blank(),
      strip.text.x = ggplot2$element_text(size = FLEISHMAN_TEXT$power_strip,
        face = "plain", family = FLEISHMAN_FONT_FAMILY, hjust = 0),
      plot.title = ggtext::element_markdown(hjust = 0,
        size = FLEISHMAN_TEXT$panel_letter, family = FLEISHMAN_FONT_FAMILY),
      plot.title.position = "plot",
      text = ggplot2$element_text(family = FLEISHMAN_FONT_FAMILY))
}

## ---- layout -----------------------------------------------------------------
pdf_title <- function(sd_vec) sprintf("input distributions, SD = (%s)",
                                      paste(format(sd_vec), collapse = ", "))

IN_PER_UNIT <- 27.5 / 6

## HALF selects which alternative is drawn. "trend" reproduces the existing
## vignette figure block for block, with omega^2 and eta_H^2 added.
HALF <- if (length(args) >= 2) args[2] else "both"
if (!HALF %in% c("both", "trend", "onepoint")) {
  stop("HALF must be \"both\", \"trend\" or \"onepoint\", not \"", HALF, "\"")
}

## DESIGNS selects which power panels follow each density strip. "balanced" is
## the vignette's arrangement: one strip and the balanced panel only.
DESIGNS <- if (length(args) >= 3) args[3] else "balanced"
if (!DESIGNS %in% c("balanced", "both")) {
  stop("DESIGNS must be \"balanced\" or \"both\", not \"", DESIGNS, "\"")
}
DES <- if (DESIGNS == "both") c(D_BAL, D_UNB) else D_BAL

letters_left <- LETTERS
take <- function(n) { out <- letters_left[seq_len(n)]
                      letters_left <<- letters_left[-seq_len(n)]; out }

blocks <- list()
heights <- numeric(0)
add_half <- function(power, eta, shifts) {
  L <- take(1 + length(DES))
  blocks <<- c(blocks, list(make_pdf_panel(shifts, L[1], pdf_title(SD_EQ))))
  for (i in seq_along(DES)) {
    blocks <<- c(blocks, list(make_power_panel(power, eta, shifts, DES[i], L[i + 1])))
  }
  heights <<- c(heights, H_PDF, rep(H_POWER, length(DES)))
}
if (HALF %in% c("both", "trend"))    add_half(trend, ETA_TREND, TREND_SHIFTS)
if (HALF %in% c("both", "onepoint")) add_half(point, ETA_POINT, POINT_SHIFTS)

fig <- patchwork$wrap_plots(blocks, ncol = 1, heights = heights)

outfile <- switch(HALF,
  both     = sprintf("fleishman_4groups_power_homoscedastic_trend_vs_onepoint_d%s.png",
                     DELTA_TAG),
  trend    = "fleishman_4groups_power_homoscedastic_trend.png",
  onepoint = sprintf("fleishman_4groups_power_homoscedastic_onepoint_d%s.png",
                     DELTA_TAG))
ggplot2$ggsave(outfile, fig, width = 20, height = sum(heights) * IN_PER_UNIT,
               dpi = FLEISHMAN_DPI)
message("Saved: ", outfile)
