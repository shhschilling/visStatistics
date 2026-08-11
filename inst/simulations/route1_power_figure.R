## Fleishman Route 1 power figures for the 50k simulation run.

if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
if (!requireNamespace("patchwork", quietly = TRUE)) stop("Package 'patchwork' is required.")
if (!requireNamespace("scales", quietly = TRUE)) stop("Package 'scales' is required.")
if (!requireNamespace("ggtext", quietly = TRUE)) stop("Package 'ggtext' is required.")

## Locate the simulation output and the shared helpers, whether this script is
## sourced from inside inst/simulations/ or from anywhere else with the package
## installed. A referee replicating the figures reads the saved Monte Carlo
## output; the simulation itself is not rerun.
SIMDIR <- local({
  here <- getwd()
  if (file.exists(file.path(here, "fleishman_route1_residual_helpers.R"))) {
    here
  } else {
    installed <- system.file("simulations", package = "visStatistics")
    if (!nzchar(installed)) {
      stop("Cannot locate the simulations directory: run from inst/simulations/ ",
           "or install visStatistics.")
    }
    installed
  }
})

source(file.path(SIMDIR, "fleishman_route1_residual_helpers.R"))
source(file.path(SIMDIR, "fleishman_figure_typography.R"))
## omega_sq_regime(), to name which of the three population omega^2 applies.
source(file.path(SIMDIR, "omega_scaling_helpers.R"))

## Unweighted relative effects psi_i per (design, panel) for THIS grid, from
## psi_relative_effects.R. Rows are selected by grid == "legacy" so the labels
## can only come from the design constants this grid actually simulated.
ETA_OWN <- local({
  f <- file.path(SIMDIR, "psi_by_design_panel.csv")
  if (!file.exists(f)) {
    message("eta_h_own_by_design_panel.csv not found; columns labelled by number only.")
    return(NULL)
  }
  e <- read.csv(f, stringsAsFactors = FALSE)
  e <- e[e$grid == "legacy", , drop = FALSE]
  if (nrow(e) == 0) NULL else e
})

OUTDIR <- "."
FIGDIR <- "."
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIGDIR, showWarnings = FALSE, recursive = TRUE)

RESULTS <- file.path(SIMDIR, "fleishman_4groups_power.rds")
power <- readRDS(RESULTS)

## Results from before the unbalanced design was added carry no `design`
## column; treat them as the balanced series so the script still runs on them.
if (is.null(power$design)) {
  power$design <- "balanced n, equal SD"
  power$n_vector <- paste(rep(power$n_per_group, each = 1), collapse = ", ")
}
power$design <- as.character(power$design)

DESIGN_BALANCED <- "balanced n, equal SD"
DESIGN_UNBALANCED <- "unbalanced n, equal SD"
DESIGN_UNBAL_HOMO <- DESIGN_UNBALANCED
DESIGN_BAL_UNEQ <- "balanced n, unequal SD"
DESIGN_UNBAL_POS <- "unbalanced n, larger n with larger SD"
DESIGN_UNBAL_NEG <- "unbalanced n, larger n with smaller SD"

## Two knobs, both defaulting to the behaviour this script has always had, so
## that a caller can source it for a variant without copying it (see
## figure_power_kw_only.R). Set them BEFORE sourcing.
##   POWER_INCLUDE_PSEUDORANK  FALSE drops the RK, ATS and ATSp rows, leaving
##                             the six strategies of the current implementation.
##   POWER_OUT_PREFIX          output basename, so a variant cannot overwrite
##                             the reference PNGs.
if (!exists("POWER_INCLUDE_PSEUDORANK")) POWER_INCLUDE_PSEUDORANK <- TRUE
if (!exists("POWER_OUT_PREFIX")) POWER_OUT_PREFIX <- "fleishman_4groups_power"

## Pseudo-rank arm (RK, ATS), simulated by rankfd_route1_power.R on streams
## that continue where route1_simulations.R stopped. Joined on design, group
## size and input shape; both rows are omitted when the file is absent.
RKP_FILE <- file.path(SIMDIR, "rankfd_route1_power_B50000.csv")
HAS_RKP <- POWER_INCLUDE_PSEUDORANK && file.exists(RKP_FILE)
if (HAS_RKP) {
  rkp <- read.csv(RKP_FILE)
  keyp <- function(d) paste(d$design, d$n_per_group, d$panel)
  power$rk_power <- rkp$ps_kw_power[match(keyp(power), keyp(rkp))]
  power$ats_power <- rkp$ps_ats_power[match(keyp(power), keyp(rkp))]
} else {
  power$rk_power <- NA_real_
  power$ats_power <- NA_real_
}

## Population effect sizes of each (design, panel) cell, from
## effect_sizes_by_design_panel.R. Both are needed to read a power column: a
## strategy that rejects rarely may be a weak test, or may be facing a nearly
## zero effect. omega^2 is constant across the panels of a design (the
## Fleishman panels are standardised to variance 1) but eta_H^2 is not, so the
## columns of one design are NOT equally hard for the rank-based tests.
## Set ES_SCALING to "omega_fixed" before sourcing to label the rerun grid.
if (!exists("ES_SCALING")) ES_SCALING <- "legacy"
ES_FILE <- file.path(SIMDIR, sprintf("effect_sizes_by_design_panel_%s.csv", ES_SCALING))
HAS_ES <- file.exists(ES_FILE)
ES_TAB <- if (HAS_ES) read.csv(ES_FILE, stringsAsFactors = FALSE) else NULL
if (!HAS_ES) {
  message("Effect-size table not found (", ES_FILE,
          "); power columns will be labelled by number only.")
}

## ATS under the nonparametric Behrens-Fisher null H0p (rankfd_route1_power_h0p.R),
## joined the same way as the RKP arm above. Omitted when the file is absent.
RKP_H0P_FILE <- file.path(SIMDIR, "rankfd_route1_power_h0p_B50000.csv")
HAS_RKP_H0P <- POWER_INCLUDE_PSEUDORANK && file.exists(RKP_H0P_FILE)
if (HAS_RKP_H0P) {
  rkp_h0p <- read.csv(RKP_H0P_FILE)
  keyp <- function(d) paste(d$design, d$n_per_group, d$panel)
  power$ats_h0p_power <- rkp_h0p$ats_h0p_power[match(keyp(power), keyp(rkp_h0p))]
} else {
  power$ats_h0p_power <- NA_real_
}

## Panel headers, all built the same way: design in words, then the size
## multipliers, then the SD vector (each group is scaled by its own SD,
## route1_simulations.R:262; the common shift vector is scaled once more, by
## sqrt(mean(SD^2)), so that omega^2 stays fixed across the panels -- this
## does not hold the rank-based effect fixed, see Brunner et al. 2017, JRSSB,
## pp. 1464-1465).
## Naming follows Brunner et al. 2017, JRSSB, Table 2 (p. 1477): homoscedastic
## vs heteroscedastic, and positive/negative pairing for the direction in
## which the SDs are paired with the (unbalanced) group sizes.
## omega^2 is constant across the 5 panels of a design (verified against
## effect_sizes_by_design_panel_<ES_SCALING>.csv: identical to 6 decimals in
## every panel of every design), so it belongs once in the row header, after
## the N and SD vectors -- not repeated in every column header.
omega_sq_for_design <- function(design_name) {
  if (!HAS_ES) return(NA_real_)
  es <- ES_TAB[ES_TAB$design == design_name, , drop = FALSE]
  if (nrow(es) == 0) return(NA_real_)
  unique(round(es$omega_sq, 3))[1]
}

## Three different population parameters are written omega^2 in
## _effect_size_table.Rmd, Eqs. (omega-sq-population),
## (-unbalanced) and (-heteroscedastic): they are not the same quantity, so a
## row must name the one that applies to its design rather than a bare
## "omega^2". The subscripts bal/unb/het are those returned by
## omega_sq_regime() in omega_scaling_helpers.R, so figure, text and code use
## one vocabulary. Any unequal SD makes a design heteroscedastic whether or not
## the group sizes are balanced.
## The subscript is exactly what omega_sq_regime() returns -- bal, unbal, het --
## the same three strings the vignette uses, so there is no mapping to keep in
## step between figure, text and code.
omega_sq_symbol <- function(nmult, sdvec) {
  num <- function(s) as.numeric(strsplit(s, ",[ ]*")[[1]])
  paste0("&omega;<sup>2</sup><sub>", omega_sq_regime(num(nmult), num(sdvec)), "</sub>")
}

sd_header <- function(what, nmult, sdvec, design_name) {
  omega <- omega_sq_for_design(design_name)
  omega_part <- if (is.na(omega)) "" else {
    sprintf("; %s = %s", omega_sq_symbol(nmult, sdvec), format(omega, nsmall = 3))
  }
  paste0(
    "power simulations, ", what, "; ",
    "(n<sub>1</sub>, n<sub>2</sub>, n<sub>3</sub>, n<sub>4</sub>) = ",
    "n&#772;(", nmult, "); ",
    "(SD<sub>1</sub>, SD<sub>2</sub>, SD<sub>3</sub>, SD<sub>4</sub>) = (", sdvec, ")",
    omega_part
  )
}
NMULT_BAL <- "1, 1, 1, 1"
NMULT_UNBAL <- "0.5, 0.8, 1.2, 1.5"
SDV_EQ <- "1, 1, 1, 1"
SDV_POS <- "1, 1.3, 1.7, 2.2"
SDV_NEG <- "2.2, 1.7, 1.3, 1"

HEADER_BALANCED   <- sd_header("balanced homoscedastic",   NMULT_BAL,   SDV_EQ,  DESIGN_BALANCED)
HEADER_BAL_UNEQ   <- sd_header("balanced heteroscedastic", NMULT_BAL,   SDV_POS, DESIGN_BAL_UNEQ)
HEADER_UNBAL_HOMO <- sd_header("unbalanced homoscedastic", NMULT_UNBAL, SDV_EQ,  DESIGN_UNBAL_HOMO)
HEADER_UNBAL_POS  <- sd_header("unbalanced heteroscedastic (positive pairing)", NMULT_UNBAL, SDV_POS, DESIGN_UNBAL_POS)
HEADER_UNBAL_NEG  <- sd_header("unbalanced heteroscedastic (negative pairing)", NMULT_UNBAL, SDV_NEG, DESIGN_UNBAL_NEG)

ggplot2 <- asNamespace("ggplot2")
patchwork <- asNamespace("patchwork")
scales <- asNamespace("scales")
group_cols <- fleishman_group_cols

panel_title <- function(panel) {
  one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
  if (nrow(one) != 1) stop("Unknown Fleishman panel: ", panel)
  if (panel == 1) {
    return("N(0, 1)\n\n\nskew = 0\nexcess kurtosis = 0")
  }
  sprintf(
    paste(
      "Fleishman polynomial",
      "a = %.3f, b = %.3f",
      "c = -a, d = %.3f",
      "skew = %s",
      "excess kurtosis = %s",
      sep = "\n"
    ),
    one$a,
    one$b,
    one$d,
    format(one$skew, trim = TRUE, scientific = FALSE),
    format(one$excess_kurtosis, trim = TRUE, scientific = FALSE)
  )
}

power$panel <- as.integer(power$panel)
power$skew_label <- factor(
  vapply(power$panel, function(panel) {
    one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
    if (panel == 1) {
      return("Normal distribution\nskew = 0; excess kurtosis = 0")
    }
    sprintf(
      "F.P.\nskew = %s\nexcess kurtosis = %s",
      format(one$skew, trim = TRUE, scientific = FALSE),
      format(one$excess_kurtosis, trim = TRUE, scientific = FALSE)
    )
  }, character(1)),
  levels = vapply(fleishman_cases$panel, function(panel) {
    one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
    if (panel == 1) {
      return("Normal distribution\nskew = 0; excess kurtosis = 0")
    }
    sprintf(
      "F.P.\nskew = %s\nexcess kurtosis = %s",
      format(one$skew, trim = TRUE, scientific = FALSE),
      format(one$excess_kurtosis, trim = TRUE, scientific = FALSE)
    )
  }, character(1))
)

skew_levels <- levels(power$skew_label)
panel_levels <- paste0(seq_along(skew_levels), ")")

group_means <- as.numeric(strsplit(power$group_mean_offsets[1], ", ")[[1]])
groups <- names(group_cols)[seq_along(group_means)]
group_legend_labels <- sprintf("%s (mean shift = %.2f)", groups, group_means)
names(group_legend_labels) <- groups
xlim <- c(-2.5, 5)
y_cap <- 0.7

make_density <- function(panel, shift, sd_i) {
  x <- seq(xlim[1], xlim[2], length.out = 700)
  density <- fleishman_scaled_density(x, panel, sd = sd_i, shift = shift)
  density[!is.finite(density)] <- NA_real_
  data.frame(x = x, density = density, piece = "density")
}

## One density strip per variance configuration: the shift vector is the same in
## every design, so a strip is fixed by the SD vector alone. The reverse pairing
## puts the largest shift on the narrowest group and therefore needs its own.
make_pdf_panel <- function(sd_vec, shifts, panel_letter, panel_description) {
  num <- function(x) format(x, trim = TRUE, drop0trailing = TRUE)
  legend_labels <- sprintf("%s (mean shift = %s, SD = %s)", groups, num(round(shifts, 2)), num(sd_vec))
  names(legend_labels) <- groups

  pdf_rows <- list()
  idx <- 1
  for (panel in sort(unique(power$panel))) {
    for (i in seq_along(shifts)) {
      curve <- make_density(panel, shifts[i], sd_vec[i])
      pdf_rows[[idx]] <- data.frame(
        panel = panel,
        distribution = panel_title(panel),
        group = groups[i],
        shift = shifts[i],
        x = curve$x,
        density = curve$density,
        piece = curve$piece,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  pdf_data <- do.call(rbind, pdf_rows)
  pdf_data$distribution <- factor(
    pdf_data$distribution,
    levels = vapply(sort(unique(power$panel)), panel_title, character(1))
  )
  pdf_panel_numbers <- data.frame(
    distribution = factor(
      vapply(sort(unique(power$panel)), panel_title, character(1)),
      levels = levels(pdf_data$distribution)
    ),
    number = paste0(sort(unique(power$panel)), ")"),
    x = xlim[1],
    y = y_cap,
    stringsAsFactors = FALSE
  )
  reference_lines <- do.call(rbind, lapply(sort(unique(power$panel)), function(panel) {
    one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
    data.frame(
      distribution = factor(panel_title(panel), levels = levels(pdf_data$distribution)),
      group = factor(rep(groups, 2), levels = groups),
      value = c(shifts, one$a * sd_vec + shifts),
      line_type = factor(
        rep(c("mean", "median"), each = length(groups)),
        levels = c("mean", "median")
      )
    )
  }))

    ggplot2$ggplot(
    pdf_data,
    ggplot2$aes(x = x, y = density, colour = group, group = interaction(group, piece))
  ) +
    ggplot2$geom_vline(
      data = reference_lines,
      ggplot2$aes(xintercept = value, colour = group, linetype = line_type),
      linewidth = 0.42,
      alpha = 0.75,
      inherit.aes = FALSE,
      show.legend = c(colour = FALSE, linetype = TRUE)
    ) +
    ggplot2$geom_line(linewidth = 0.7, na.rm = TRUE) +
    ggplot2$geom_text(
      data = pdf_panel_numbers,
      ggplot2$aes(x = x, y = y, label = number),
      inherit.aes = FALSE,
      family = FLEISHMAN_FONT_FAMILY,
      fontface = "plain",
      hjust = 0,
      vjust = -2.0,
      size = FLEISHMAN_GEOM_TEXT$panel_number
    ) +
    ggplot2$facet_wrap(~distribution, nrow = 1) +
    ggplot2$coord_cartesian(xlim = xlim, ylim = c(0, y_cap), clip = "off") +
    ggplot2$scale_colour_manual(
      values = group_cols,
      breaks = groups,
      labels = legend_labels,
      name = "group,\ngroup mean offset"
    ) +
    ggplot2$scale_linetype_manual(
      values = c(mean = "dashed", median = "dotted"),
      name = "reference line"
    ) +
    ggplot2$guides(
      colour = ggplot2$guide_legend(
        order = 1
      ),
      linetype = ggplot2$guide_legend(
        order = 2,
        override.aes = list(colour = "grey25")
      )
    ) +
    ggplot2$labs(
      title = fleishman_panel_title(panel_letter, panel_description),
      subtitle = NULL,
      x = "Response value with group shift",
      y = "Theoretical density"
    ) +
    ggplot2$theme_minimal(
      base_size = FLEISHMAN_TEXT$section_title,
      base_family = FLEISHMAN_FONT_FAMILY
    ) +
    ggplot2$theme(
      panel.grid.minor = ggplot2$element_blank(),
      panel.border = ggplot2$element_rect(colour = "grey35", fill = NA, linewidth = 0.35),
      strip.text = ggplot2$element_text(
        size = FLEISHMAN_TEXT$panel_title,
        family = FLEISHMAN_FONT_FAMILY,
        lineheight = FLEISHMAN_LINEHEIGHT$panel_title
      ),
      strip.background = ggplot2$element_blank(),
      legend.position = "right",
      legend.title = ggplot2$element_text(size = FLEISHMAN_TEXT$legend),
      legend.text = ggplot2$element_text(size = FLEISHMAN_TEXT$legend),
      axis.title.x = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_title),
      axis.title.y = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_title),
      plot.title = ggtext::element_markdown(
        hjust = 0,
        size = FLEISHMAN_TEXT$panel_letter,
        family = FLEISHMAN_FONT_FAMILY
      ),
      plot.subtitle = ggplot2$element_text(size = FLEISHMAN_TEXT$section_title),
      plot.title.position = "plot",
      plot.margin = ggplot2$margin(14, 5.5, 5.5, 5.5)
    )
}

SD_HOMO <- c(1, 1, 1, 1)
SD_POS <- c(1.0, 1.3, 1.7, 2.2)
SD_NEG <- rev(SD_POS)

STRATEGY_LABELS <- c(
  "1. Fisher always" = "F",
  "2. Welch always" = "W",
  "3. Levene-gated Fisher/Welch" = "L",
  "4. Kruskal-Wallis always" = "KW",
  "4b. Pseudo-rank Kruskal-Wallis" = "RK",
  "4c. ANOVA-type statistic" = "ATS",
  "4d. ANOVA-type statistic (H0p)" = "ATSp",
  "5. Shapiro-Wilk routed Welch/KW" = "SW",
  "6. Shapiro-Wilk plus Levene" = "SW+L"
)
STRATEGY_SHAPES <- c(
  "1. Fisher always" = 0,
  "2. Welch always" = 2,
  "3. Levene-gated Fisher/Welch" = 5,
  "4. Kruskal-Wallis always" = 4,
  "4b. Pseudo-rank Kruskal-Wallis" = 3,
  "4c. ANOVA-type statistic" = 6,
  "4d. ANOVA-type statistic (H0p)" = 8,
  "5. Shapiro-Wilk routed Welch/KW" = 1,
  "6. Shapiro-Wilk plus Levene" = 1
)
STRATEGY_SIZES <- c(
  "1. Fisher always" = 4.0,
  "2. Welch always" = 3.2,
  "3. Levene-gated Fisher/Welch" = 3.6,
  "4. Kruskal-Wallis always" = 3.8,
  "4b. Pseudo-rank Kruskal-Wallis" = 3.8,
  "4c. ANOVA-type statistic" = 3.8,
  "4d. ANOVA-type statistic (H0p)" = 3.8,
  "5. Shapiro-Wilk routed Welch/KW" = 5.8,
  "6. Shapiro-Wilk plus Levene" = 7.2
)
STRATEGY_COLOURS <- c(
  "1. Fisher always" = "#B79F00",
  "2. Welch always" = "#56B4E9",
  "3. Levene-gated Fisher/Welch" = "#009E73",
  "4. Kruskal-Wallis always" = "#000000",
  "4b. Pseudo-rank Kruskal-Wallis" = "#999999",
  "4c. ANOVA-type statistic" = "#CC79A7",
  "4d. ANOVA-type statistic (H0p)" = "#E69F00",
  "5. Shapiro-Wilk routed Welch/KW" = "#D55E00",
  "6. Shapiro-Wilk plus Levene" = "#0072B2"
)
NS_TO_PLOT <- c(10, 20, 30, 50, 100)
EFFECTS_TO_PLOT <- "moderate ordered effect: 0, 0.25, 0.50, 0.75 SD"

power$effect_size <- factor(power$effect_size, levels = EFFECTS_TO_PLOT)
power$power_panel <- factor(paste0(power$panel, ")"), levels = panel_levels)
power$n_label <- factor(paste0("n = ", power$n_per_group),
  levels = paste0("n = ", sort(unique(power$n_per_group)))
)

to_long <- function(dat) {
  rows <- list(
    transform(dat, strategy = "1. Fisher always", power = fisher_power),
    transform(dat, strategy = "2. Welch always", power = welch_power),
    transform(dat, strategy = "3. Levene-gated Fisher/Welch", power = mean_power),
    transform(dat, strategy = "4. Kruskal-Wallis always", power = rank_power)
  )
  if (HAS_RKP) {
    rows <- c(rows, list(
      transform(dat, strategy = "4b. Pseudo-rank Kruskal-Wallis", power = rk_power),
      transform(dat, strategy = "4c. ANOVA-type statistic", power = ats_power)
    ))
  }
  if (HAS_RKP_H0P) {
    rows <- c(rows, list(
      transform(dat, strategy = "4d. ANOVA-type statistic (H0p)", power = ats_h0p_power)
    ))
  }
  rows <- c(rows, list(
    transform(dat, strategy = "5. Shapiro-Wilk routed Welch/KW", power = sw_power),
    transform(dat, strategy = "6. Shapiro-Wilk plus Levene", power = gate_power)
  ))
  out <- do.call(rbind, rows)
  out$strategy <- factor(out$strategy, levels = names(STRATEGY_LABELS))
  out
}

## One power panel per design. Everything below is identical between the two
## series except the data subset and the header, so it is built once.
make_power_plot <- function(design_name, panel_letter, panel_description,
                            show_legend = TRUE,
                            x_label = "n per group") {
  dat <- power[power$design == design_name, , drop = FALSE]
  if (nrow(dat) == 0) {
    stop("No power results for design: ", design_name)
  }

  ## omega^2 is constant across the 5 panels of a design (see
  ## omega_sq_for_design() above), so it is stated once in the row header.
  ##
  ## The unweighted relative effects psi_i are NOT constant across columns: they
  ## respond to distribution shape, so they belong in the column strips. psi_i
  ## is allocation-free and is an effect size in the strict sense
  ## (Zimmermann et al. 2021, p. 125), unlike the weighted theta_i that
  ## kruskal.test() actually estimates. All designs here have a = 4, which is
  ## the condition under which deviations from 1/2 are comparable
  ## (Zimmermann et al. 2021, Sect. 3.3). See psi_relative_effects.R.
  eta_lab <- stats::setNames(panel_levels, panel_levels)
  has_eta <- FALSE
  if (!is.null(ETA_OWN)) {
    e <- ETA_OWN[ETA_OWN$design == design_name, , drop = FALSE]
    if (nrow(e) == length(panel_levels)) {
      e <- e[order(e$panel), ]
      eta_lab <- stats::setNames(
        sprintf('"%d)"~~psi == "%s"', e$panel,
                vapply(strsplit(e$psi, ",[ ]*"), function(v)
                  paste(sprintf("%.2f", as.numeric(v)), collapse = " "), character(1))),
        paste0(e$panel, ")")
      )
      has_eta <- TRUE
    }
  }
  dat$power_panel <- factor(unname(eta_lab[as.character(dat$power_panel)]),
                            levels = unname(eta_lab[panel_levels]))

  power_long <- to_long(dat)
  power_plot <- subset(power_long, n_per_group %in% NS_TO_PLOT)

  gate_labels <- subset(dat, n_per_group %in% NS_TO_PLOT)
  gate_labels$gate_title <- "SW+L selection (%)"
  gate_labels$gate_row <- "SW+L"
  gate_labels$gate_y <- 0.085

  gate_rate_values <- rbind(
    transform(gate_labels,
      gate_row = "F", gate_y = 0.085,
      gate_rate = route_fisher_probability
    ),
    transform(gate_labels,
      gate_row = "W", gate_y = 0.055,
      gate_rate = route_welch_probability
    ),
    transform(gate_labels,
      gate_row = "KW", gate_y = 0.025,
      gate_rate = route_rank_probability
    )
  )
  gate_rate_values$gate_rate_label <- sprintf("%.0f", 100 * gate_rate_values$gate_rate)
  gate_rate_rows <- unique(gate_rate_values[c("power_panel", "gate_row", "gate_y")])
  gate_rate_title <- unique(gate_labels["power_panel"])
  gate_rate_title$gate_title <- "SW+L selection (%)"

  ggplot2$ggplot() +
  ggplot2$geom_vline(xintercept = NS_TO_PLOT, colour = "grey88", linewidth = 0.35) +
  ## horizontal guides start at 20 %: the band below carries the SW+L selection
  ## inset, which the lines would otherwise cross.
  ggplot2$geom_hline(
    yintercept = seq(0.2, 1, by = 0.1), colour = "grey88", linewidth = 0.35
  ) +
  ggplot2$geom_point(
    data = power_plot,
    ggplot2$aes(
      x = n_per_group, y = power, colour = strategy,
      shape = strategy, size = strategy, group = strategy
    ),
    stroke = 1.15
  ) +
  ggplot2$geom_text(
    data = gate_rate_title,
    ggplot2$aes(x = 26, y = 0.125, label = gate_title),
    colour = "grey25",
    family = FLEISHMAN_FONT_FAMILY,
    size = FLEISHMAN_GEOM_TEXT$inset
  ) +
  ggplot2$geom_text(
    data = gate_rate_rows,
    ggplot2$aes(x = 5.8, y = gate_y, label = gate_row),
    colour = "grey25",
    family = FLEISHMAN_FONT_FAMILY,
    hjust = 0,
    size = FLEISHMAN_GEOM_TEXT$inset
  ) +
  ggplot2$geom_text(
    data = gate_rate_values,
    ggplot2$aes(x = n_per_group, y = gate_y, label = gate_rate_label),
    colour = "grey25",
    family = FLEISHMAN_FONT_FAMILY,
    size = FLEISHMAN_GEOM_TEXT$inset
  ) +
  ggplot2$facet_grid(stats::as.formula(". ~ power_panel"),
    labeller = if (has_eta) ggplot2$label_parsed else ggplot2$label_value) +
  ggplot2$scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0.1, 1, by = 0.1),
    labels = scales::percent
  ) +
  ggplot2$scale_x_log10(breaks = NS_TO_PLOT, limits = c(5.5, 130)) +
  ggplot2$scale_shape_manual(
    values = STRATEGY_SHAPES,
    breaks = names(STRATEGY_LABELS),
    labels = STRATEGY_LABELS
  ) +
  ggplot2$scale_size_manual(
    values = STRATEGY_SIZES,
    breaks = names(STRATEGY_LABELS),
    labels = STRATEGY_LABELS
  ) +
  ggplot2$scale_colour_manual(
    values = STRATEGY_COLOURS,
    breaks = names(STRATEGY_LABELS),
    labels = STRATEGY_LABELS
  ) +
  ggplot2$labs(
    title = fleishman_panel_title(panel_letter, panel_description),
    x = x_label,
    y = "simulated rejection rate",
    colour = "test strategy",
    shape = "test strategy",
    size = "test strategy"
  ) +
  ggplot2$theme_minimal(
    base_size = FLEISHMAN_TEXT$section_title,
    base_family = FLEISHMAN_FONT_FAMILY
  ) +
  ggplot2$theme(
    legend.position = if (show_legend) "right" else "none",
    legend.title = ggplot2$element_text(size = FLEISHMAN_TEXT$legend),
    legend.text = ggplot2$element_text(size = FLEISHMAN_TEXT$legend),
    axis.title.x = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_title),
    axis.title.y = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_title),
    axis.text = ggplot2$element_text(size = FLEISHMAN_TEXT$axis_text),
    axis.text.x = ggplot2$element_text(angle = 45, hjust = 1),
    panel.grid.major.x = ggplot2$element_blank(),
    panel.grid.minor.x = ggplot2$element_blank(),
    panel.grid.major.y = ggplot2$element_blank(),
    panel.grid.minor.y = ggplot2$element_blank(),
    panel.border = ggplot2$element_rect(colour = "black", fill = NA, linewidth = 0.25),
    strip.background = ggplot2$element_blank(),
    strip.text.x = ggplot2$element_text(
      size = FLEISHMAN_TEXT$power_strip,
      face = "plain",
      family = FLEISHMAN_FONT_FAMILY,
      hjust = 0
    ),
    plot.title = ggtext::element_markdown(
      hjust = 0,
      size = FLEISHMAN_TEXT$panel_letter,
      family = FLEISHMAN_FONT_FAMILY
    ),
    plot.title.position = "plot"
  )
}

p_power <- make_power_plot(DESIGN_BALANCED, "B", HEADER_BALANCED,
  show_legend = TRUE
)

nbar_label <- expression(paste(bar(n), " per group"))
p_power_bal_uneq   <- make_power_plot(DESIGN_BAL_UNEQ,   "D", HEADER_BAL_UNEQ,   show_legend = TRUE)
## Unbalanced homoscedastic (DESIGN_UNBALANCED) had no panel anywhere despite
## being simulated (fleishman_4groups_power.csv carries it). Added here as the
## first block of the unbalanced figure, ahead of the two heteroscedastic
## pairings, so the homoscedastic baseline is seen before the contrast it sets
## up.
p_power_unbal_homo <- make_power_plot(DESIGN_UNBAL_HOMO, "B", HEADER_UNBAL_HOMO, show_legend = TRUE,
                                     x_label = nbar_label)
p_power_unbal_pos <- make_power_plot(DESIGN_UNBAL_POS, "D", HEADER_UNBAL_POS, show_legend = TRUE,
                                     x_label = nbar_label)
p_power_unbal_neg <- make_power_plot(DESIGN_UNBAL_NEG, "F", HEADER_UNBAL_NEG, show_legend = TRUE,
                                     x_label = nbar_label)

## Two figures, each carrying the densities actually simulated beneath it.
SHIFTS_HOMO <- as.numeric(strsplit(
  power$group_mean_offsets[power$design == DESIGN_BALANCED][1], ", ")[[1]])
SHIFTS_HETERO <- as.numeric(strsplit(
  power$group_mean_offsets[power$design == DESIGN_UNBAL_POS][1], ", ")[[1]])

p_pdf_homo <- make_pdf_panel(SD_HOMO, SHIFTS_HOMO, "A", "input distributions, equal SD")
p_pdf_pos <- make_pdf_panel(SD_POS, SHIFTS_HETERO, "A",
  "input distributions, SD = (1, 1.3, 1.7, 2.2)")
p_pdf_neg <- make_pdf_panel(SD_NEG, SHIFTS_HETERO, "D",
  "input distributions, SD = (2.2, 1.7, 1.3, 1)")

## One figure per size design: balanced, then unbalanced. Each carries two
## density strips, because the equal-SD and unequal-SD blocks below them are
## drawn from different distributions, and in the unbalanced figure the reverse
## pairing puts the largest mean shift on the narrowest group.
p_pdf_homo_A <- make_pdf_panel(SD_HOMO, SHIFTS_HOMO, "A", "input distributions, equal SD")
p_pdf_pos_C  <- make_pdf_panel(SD_POS,  SHIFTS_HETERO, "C",
  "input distributions, SD = (1, 1.3, 1.7, 2.2)")
p_pdf_neg_E  <- make_pdf_panel(SD_NEG,  SHIFTS_HETERO, "E",
  "input distributions, SD = (2.2, 1.7, 1.3, 1)")

combined_balanced <- patchwork$wrap_plots(
  p_pdf_homo_A, p_power, p_pdf_pos_C, p_power_bal_uneq,
  ncol = 1, heights = c(1, 2.0, 1, 2.0)
)
combined_unbalanced <- patchwork$wrap_plots(
  p_pdf_homo_A, p_power_unbal_homo,
  p_pdf_pos_C,  p_power_unbal_pos,
  p_pdf_neg_E,  p_power_unbal_neg,
  ncol = 1, heights = c(1, 2.0, 1, 2.0, 1, 2.0)
)

COMBINED_WIDTH <- 20
HEIGHT_BALANCED <- 27.5
HEIGHT_UNBALANCED <- 27.5 * 3 / 2

for (dir in unique(c(OUTDIR, FIGDIR))) {
  ggplot2$ggsave(file.path(dir, paste0(POWER_OUT_PREFIX, "_balanced.png")),
    combined_balanced, width = COMBINED_WIDTH, height = HEIGHT_BALANCED, dpi = FLEISHMAN_DPI)
  ggplot2$ggsave(file.path(dir, paste0(POWER_OUT_PREFIX, "_unbalanced.png")),
    combined_unbalanced, width = COMBINED_WIDTH, height = HEIGHT_UNBALANCED, dpi = FLEISHMAN_DPI)
}

message("Updated plots in: ", OUTDIR, " (prefix ", POWER_OUT_PREFIX, ")")
