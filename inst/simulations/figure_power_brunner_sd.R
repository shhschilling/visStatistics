## ---------------------------------------------------------------------------
## Power figures for the Brunner-SD design variant.
##
## Inputs, pooled by variance structure rather than by balance, so that no
## density strip is drawn twice:
##
##   homoscedastic  SD = (1,1,1,1)                 both designs from
##                                                 fleishman_4groups_power.rds
##                                                 (inputs identical, not rerun)
##   heteroscedastic SD = (1, sqrt(2), 2, sqrt(5)) and its reverse, from
##                                                 fleishman_4groups_power_design_brunner_B50000.csv
##                                                 + rankfd_route1_power_design_brunner_B50000.csv
##
## The shift vector is (0, 0.25, 0.50, 0.75) in EVERY design -- no rescaling --
## so omega^2 is reported as the consequence of the design. See
## route1_power_design_variants.R for why the old sqrt(mean(SD^2)) rescaling was
## dropped.
##
## Two variants, chosen by the first argument:
##   kw    (default) only Kruskal-Wallis among the rank tests, matching what
##         visstat_core() can select
##   full  adds RK, ATS and ATSp, for inspection
##
## Output (new names; nothing existing is overwritten):
##   fleishman_4groups_power_brunner_<variant>_homoscedastic.png
##   fleishman_4groups_power_brunner_<variant>_heteroscedastic.png
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
VARIANT <- if (length(args) >= 1) args[1] else "kw"
if (!VARIANT %in% c("kw", "full")) stop("VARIANT must be \"kw\" or \"full\".")

ggplot2 <- asNamespace("ggplot2"); patchwork <- asNamespace("patchwork")
scales <- asNamespace("scales")

D_BAL_EQ  <- "balanced n, equal SD"
D_UNB_EQ  <- "unbalanced n, equal SD"
D_BAL_HET <- "balanced n, unequal SD"
D_POS     <- "unbalanced n, larger n with larger SD"
D_NEG     <- "unbalanced n, larger n with smaller SD"

DESIGN_WORDS <- c(
  "balanced homoscedastic", "unbalanced homoscedastic", "balanced heteroscedastic",
  "unbalanced heteroscedastic (positive pairing)",
  "unbalanced heteroscedastic (negative pairing)"
)
names(DESIGN_WORDS) <- c(D_BAL_EQ, D_UNB_EQ, D_BAL_HET, D_POS, D_NEG)
NMULT <- c("1, 1, 1, 1", "0.5, 0.8, 1.2, 1.5", "1, 1, 1, 1",
           "0.5, 0.8, 1.2, 1.5", "0.5, 0.8, 1.2, 1.5")
names(NMULT) <- names(DESIGN_WORDS)

SD_EQ  <- c(1, 1, 1, 1)
SD_POS <- c(1, sqrt(2), 2, sqrt(5))
SD_NEG <- rev(SD_POS)
SHIFTS <- c(0, 0.25, 0.50, 0.75)

## Brunner et al. (2017), Table 2, p. 1477 writes the scaling vectors as
## (1, sqrt(2), 2, sqrt(5)) and its reverse, because the variances behind them
## are the integers sigma^2 = (1, 2, 4, 5). Printing 1.414 and 2.236 loses that,
## so every label -- panel titles, row headers and the density legend -- goes
## through this formatter. U+221A renders in plain ggplot text and in the
## ggtext markdown titles alike.
sd_label <- function(x) {
  vapply(x, function(v) {
    if (isTRUE(all.equal(v, round(v)))) return(format(round(v)))
    sq <- v^2
    if (isTRUE(all.equal(sq, round(sq)))) return(paste0("√", format(round(sq))))
    format(round(v, 3), trim = TRUE, drop0trailing = TRUE)
  }, character(1))
}
sd_vector_label <- function(x) paste(sd_label(x), collapse = ", ")

## ---- assemble the grid ------------------------------------------------------
het <- read.csv(file.path(SIMDIR, "fleishman_4groups_power_design_brunner_B50000.csv"),
                stringsAsFactors = FALSE)
rk <- read.csv(file.path(SIMDIR, "rankfd_route1_power_design_brunner_B50000.csv"),
               stringsAsFactors = FALSE)
key <- function(d) paste(d$design, d$n_per_group, d$panel)
het$rk_power   <- rk$ps_kw_power[match(key(het), key(rk))]
het$ats_power  <- rk$ps_ats_power[match(key(het), key(rk))]
het$atsp_power <- rk$ats_h0p_power[match(key(het), key(rk))]

## The two homoscedastic designs were not rerun: their inputs are identical to
## the existing grid, so they are taken from it and their rank arms from the
## existing rankfd files.
homo <- readRDS(file.path(SIMDIR, "fleishman_4groups_power.rds"))
homo <- homo[homo$design %in% c(D_BAL_EQ, D_UNB_EQ), , drop = FALSE]
rkh <- read.csv(file.path(SIMDIR, "rankfd_route1_power_B50000.csv"), stringsAsFactors = FALSE)
homo$rk_power  <- rkh$ps_kw_power[match(key(homo), key(rkh))]
homo$ats_power <- rkh$ps_ats_power[match(key(homo), key(rkh))]
rkp <- file.path(SIMDIR, "rankfd_route1_power_h0p_B50000.csv")
homo$atsp_power <- if (file.exists(rkp)) {
  h <- read.csv(rkp); h$ats_h0p_power[match(key(homo), key(h))]
} else NA_real_

keep <- c("design", "n_per_group", "panel", "fisher_power", "welch_power",
          "mean_power", "rank_power", "sw_power", "gate_power",
          "route_fisher_probability", "route_welch_probability",
          "route_rank_probability", "rk_power", "ats_power", "atsp_power")
power <- rbind(homo[, keep], het[, keep])

## omega^2 recomputed from the design constants actually simulated -- never read
## from a stored column (see CLAUDE.md, rule 3).
SDS <- list(SD_EQ, SD_EQ, SD_POS, SD_POS, SD_NEG)
names(SDS) <- names(DESIGN_WORDS)
omega_of <- function(d) population_omega_sq(
  as.numeric(strsplit(NMULT[[d]], ",[ ]*")[[1]]), SDS[[d]], SHIFTS, 1)
regime_of <- function(d) omega_sq_regime(
  as.numeric(strsplit(NMULT[[d]], ",[ ]*")[[1]]), SDS[[d]])

## Relative block heights: a power panel is twice a density strip.
##
## The routing inset is positioned in data units while its text is sized in
## points, so a shorter panel would need a proportionally taller band to keep
## the same physical line spacing. BAND_SCALE carries that correction and is 1
## at the full height used here; it exists so that changing H_POWER cannot
## silently make the three split rows close up.
H_PDF <- 1
H_POWER <- 2
BAND_SCALE <- 2 / H_POWER

PANELS <- sort(unique(power$panel))
power$power_panel <- factor(paste0(power$panel, ")"), levels = paste0(PANELS, ")"))
NS_TO_PLOT <- c(10, 20, 30, 50, 100)
groups <- c("A", "B", "C", "D")
xlim <- c(-2.5, 5); y_cap <- 0.7

STRATS <- c("1. Fisher always" = "F", "2. Welch always" = "W",
            "3. Levene-gated Fisher/Welch" = "L", "4. Kruskal-Wallis always" = "KW",
            "4b. Pseudo-rank Kruskal-Wallis" = "RK", "4c. ANOVA-type statistic" = "ATS",
            "4d. ANOVA-type statistic (H0p)" = "ATSp",
            "5. Shapiro-Wilk routed Welch/KW" = "SW", "6. Shapiro-Wilk plus Levene" = "SW+L")
COLS <- c("#B79F00", "#56B4E9", "#009E73", "#000000", "#999999", "#CC79A7",
          "#E69F00", "#D55E00", "#0072B2")
SHP <- c(0, 2, 5, 4, 3, 6, 8, 1, 1); SZ <- c(4, 3.2, 3.6, 3.8, 3.8, 3.8, 3.8, 5.8, 7.2)
names(COLS) <- names(SHP) <- names(SZ) <- names(STRATS)
## The SW and SW+L markers are oversized so that, when the gate reproduces one
## of the fixed strategies, the ring reads as "these coincide". Once nine
## strategies are dodged side by side that sizing only costs separation, so the
## full variant shrinks everything to a common small marker.
if (length(STRATS) > 0 && identical(VARIANT, "full")) SZ[] <- 2.9
COLUMN <- c("fisher_power", "welch_power", "mean_power", "rank_power", "rk_power",
            "ats_power", "atsp_power", "sw_power", "gate_power")
names(COLUMN) <- names(STRATS)
USE <- if (VARIANT == "kw")
  setdiff(names(STRATS), c("4b. Pseudo-rank Kruskal-Wallis", "4c. ANOVA-type statistic",
                           "4d. ANOVA-type statistic (H0p)")) else names(STRATS)

panel_title <- function(p) {
  one <- fleishman_cases[fleishman_cases$panel == p, , drop = FALSE]
  if (p == 1) return("N(0, 1)\n\n\nskew = 0\nexcess kurtosis = 0")
  sprintf(paste("Fleishman polynomial", "a = %.3f, b = %.3f", "c = -a, d = %.3f",
                "skew = %s", "excess kurtosis = %s", sep = "\n"),
          one$a, one$b, one$d, one$skew, one$excess_kurtosis)
}

make_pdf_panel <- function(sd_vec, letter, description) {
  num <- function(x) format(round(x, 3), trim = TRUE, drop0trailing = TRUE)
  lab <- sprintf("%s (mean shift = %s, SD = %s)", groups, num(SHIFTS), sd_label(sd_vec))
  names(lab) <- groups
  lev <- vapply(PANELS, panel_title, character(1))
  rows <- list(); i <- 1
  for (p in PANELS) for (j in seq_along(SHIFTS)) {
    x <- seq(xlim[1], xlim[2], length.out = 700)
    d <- fleishman_scaled_density(x, p, sd = sd_vec[j], shift = SHIFTS[j])
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
               value = c(SHIFTS, one$a * sd_vec + SHIFTS),
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

make_power_panel <- function(design_name, letter) {
  dat <- power[power$design == design_name, , drop = FALSE]
  if (!nrow(dat)) stop("no rows for ", design_name)
  long <- do.call(rbind, lapply(USE, function(s) {
    v <- dat[[COLUMN[[s]]]]
    if (all(is.na(v))) return(NULL)
    transform(dat, strategy = s, power = v)
  }))
  long$strategy <- factor(long$strategy, levels = names(STRATS))
  long <- subset(long, n_per_group %in% NS_TO_PLOT)

  ## Under heteroscedasticity the gate routes to KW in most replications, so
  ## SW, SW+L and KW take the same value, and RK/ATS/ATSp sit on it too. Drawn
  ## at the same x they stack into one blob and only the last one plotted is
  ## visible. Dodge multiplicatively, because the axis is log10: each strategy
  ## is offset by a constant FACTOR, so the spread looks identical at n = 10 and
  ## n = 100. DODGE_SPAN is the total width as a fraction of the tick spacing;
  ## the ticks are a factor 2 apart at the narrowest, so 12% stays unambiguous.
  DODGE_SPAN <- if (length(USE) > 6) 0.55 else 0.30
  ns <- length(USE)
  step <- if (ns > 1) DODGE_SPAN / (ns - 1) else 0
  offset <- (match(as.character(long$strategy), USE) - (ns + 1) / 2) * step
  long$x_dodged <- long$n_per_group * (1 + offset)

  ## Where the routing inset goes depends on the data, because it only ever
  ## conflicts with it in one direction. The original placement puts the three
  ## split rows inside the 0-12% strip of the plotting area, which is free
  ## whenever power stays clear of it. At omega^2_het = 0.024-0.030 it is not:
  ## rejection rates fall to about 5% and the markers overprint the numbers.
  ##
  ## So: keep the inset inside the panel when the lowest plotted rate leaves
  ## room, and only then carve out a band below zero, which costs 21% of the
  ## panel's height. The homoscedastic block keeps its full 0-100%.
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

  omega <- omega_of(design_name)
  header <- sprintf(paste0("power simulations, %s; (n<sub>1</sub>, n<sub>2</sub>, ",
      "n<sub>3</sub>, n<sub>4</sub>) = n&#772;(%s); (SD<sub>1</sub>, SD<sub>2</sub>, ",
      "SD<sub>3</sub>, SD<sub>4</sub>) = (%s); &omega;<sup>2</sup><sub>%s</sub> = %.3f"),
    DESIGN_WORDS[[design_name]], NMULT[[design_name]],
    sd_vector_label(SDS[[design_name]]), regime_of(design_name), omega)

  ggplot2$ggplot() +
    ggplot2$geom_vline(xintercept = NS_TO_PLOT, colour = "grey88", linewidth = 0.35) +
    ggplot2$geom_hline(yintercept = seq(hline_from, 1, by = 0.1), colour = "grey88",
                       linewidth = 0.35) +
    ## rule separating the reserved routing band, only when there is one
    (if (inset_below)
       ggplot2$geom_hline(yintercept = -0.02 * BAND_SCALE, colour = "grey60",
                          linewidth = 0.3)
     else ggplot2$geom_blank()) +
    ggplot2$geom_point(data = long,
      ggplot2$aes(x = x_dodged, y = power, colour = strategy, shape = strategy,
                  size = strategy, group = strategy), stroke = 1.15) +
    ggplot2$geom_text(data = gtit, ggplot2$aes(x = 26, y = title_y, label = t),
      colour = "grey25", family = FLEISHMAN_FONT_FAMILY, size = FLEISHMAN_GEOM_TEXT$inset) +
    ggplot2$geom_text(data = grows, ggplot2$aes(x = 5.8, y = gate_y, label = gate_row),
      colour = "grey25", family = FLEISHMAN_FONT_FAMILY, size = FLEISHMAN_GEOM_TEXT$inset) +
    ggplot2$geom_text(data = gv,
      ggplot2$aes(x = n_per_group, y = gate_y, label = lab), colour = "grey25",
      family = FLEISHMAN_FONT_FAMILY, size = FLEISHMAN_GEOM_TEXT$inset) +
    ggplot2$facet_grid(stats::as.formula(". ~ power_panel")) +
    ## limits reach below 0 to hold the routing band; breaks stop at 0 so the
    ## axis never labels a negative rejection rate.
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

## ---- pooled layout ----------------------------------------------------------
## One density strip per SD vector, serving every power panel beneath it. This
## is what removes the duplication of the old balanced/unbalanced split, where
## the equal-SD and positive-pairing strips were each drawn twice.
pdf_title <- function(sd_vec) sprintf("input distributions, SD = (%s)",
                                      sd_vector_label(sd_vec))

## H_PDF and H_POWER are defined above, with BAND_SCALE, because the inset
## geometry depends on them.
## Inches per unit of the heights vector, chosen so a density strip is the same
## physical size as before (27.5/6 in per unit).
IN_PER_UNIT <- 27.5 / 6

homo_h <- c(H_PDF, H_POWER, H_POWER)
het_h <- c(H_PDF, H_POWER, H_POWER, H_PDF, H_POWER)

homo_fig <- patchwork$wrap_plots(
  make_pdf_panel(SD_EQ, "A", pdf_title(SD_EQ)),
  make_power_panel(D_BAL_EQ, "B"),
  make_power_panel(D_UNB_EQ, "C"),
  ncol = 1, heights = homo_h)

het_fig <- patchwork$wrap_plots(
  make_pdf_panel(SD_POS, "A", pdf_title(SD_POS)),
  make_power_panel(D_BAL_HET, "B"),
  make_power_panel(D_POS, "C"),
  make_pdf_panel(SD_NEG, "D", pdf_title(SD_NEG)),
  make_power_panel(D_NEG, "E"),
  ncol = 1, heights = het_h)

f1 <- sprintf("fleishman_4groups_power_brunner_%s_homoscedastic.png", VARIANT)
f2 <- sprintf("fleishman_4groups_power_brunner_%s_heteroscedastic.png", VARIANT)
ggplot2$ggsave(f1, homo_fig, width = 20, height = sum(homo_h) * IN_PER_UNIT,
               dpi = FLEISHMAN_DPI)
ggplot2$ggsave(f2, het_fig, width = 20, height = sum(het_h) * IN_PER_UNIT,
               dpi = FLEISHMAN_DPI)
message("Saved (variant = ", VARIANT, "):\n  ", f1, "\n  ", f2)
