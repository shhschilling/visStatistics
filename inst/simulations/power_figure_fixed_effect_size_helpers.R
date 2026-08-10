## ---------------------------------------------------------------------------
## Shared builder for the two REFERENCE power figures in which an effect size is
## held constant across the five designs:
##
##   figure_power_omega_fixed.R   omega^2 held at the balanced baseline
##   figure_power_etaH_fixed.R    eta_H^2 held at the balanced baseline
##
## These are reference figures for inspection, NOT the vignette figures. The
## vignette's power figure is built by route1_power_figure.R from
## fleishman_4groups_power.rds, where the shift vector is the same in every
## design and the effect size is therefore allowed to differ between them.
##
## Why these grids exist, and their limitation
## -------------------------------------------
## In route1_simulations.R the common shift vector is scaled once by
## sqrt(mean(SD^2)), which holds omega^2 at the balanced homoscedastic baseline
## only when the group sizes are equal. The two grids plotted here instead
## rescale the shifts per design so that the named effect size is identical in
## every row (see omega_scaling_helpers.R), isolating the effect of the design
## from the effect of its effect size.
##
## The cost of that construction, and the reason these are reference figures
## only: holding the effect size fixed forces the parametric branch to show
## essentially the same curve in every design by construction, so what remains
## visible is mostly the rank branch. Comparing the same distributions across
## designs -- letting the effect size vary as a consequence of the allocation --
## is the comparison that answers "what does imbalance do", and that is what the
## vignette figure shows.
##
## eta_H^2 caveat: unlike omega^2, eta_H^2 has no established population
## definition independent of N (ranks have no population-level existence the way
## (mu_j, sigma_j^2) do). The eta_H^2-fixed grid holds a quadrature-computed
## value fixed; that value is reported here as the quantity the simulation
## actually held constant, not as a citable population parameter.
##
## Only the six strategies simulated in these grids are drawn (F, W, L, KW, SW,
## SW+L). The pseudo-rank arms (RK, ATS, ATSp) were simulated on the vignette's
## shift vector and must NOT be joined onto these grids: the keys would match
## while the underlying shifts would not.
## ---------------------------------------------------------------------------

for (pkg in c("ggplot2", "patchwork", "scales", "ggtext")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Package '", pkg, "' is required.")
}

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

ggplot2 <- asNamespace("ggplot2")
patchwork <- asNamespace("patchwork")
scales <- asNamespace("scales")

## Design names as written by route1_power_omega_fixed.R / _etaH_fixed.R.
DESIGN_BALANCED   <- "balanced n, equal SD"
DESIGN_UNBAL_HOMO <- "unbalanced n, equal SD"
DESIGN_BAL_UNEQ   <- "balanced n, unequal SD"
DESIGN_UNBAL_POS  <- "unbalanced n, larger n with larger SD"
DESIGN_UNBAL_NEG  <- "unbalanced n, larger n with smaller SD"

## Naming follows Brunner et al. 2017, JRSSB, Table 2: homoscedastic vs
## heteroscedastic, and positive/negative pairing for the direction in which the
## SDs are paired with the (unbalanced) group sizes.
DESIGN_WORDS <- c(
  "balanced homoscedastic",
  "unbalanced homoscedastic",
  "balanced heteroscedastic",
  "unbalanced heteroscedastic (positive pairing)",
  "unbalanced heteroscedastic (negative pairing)"
)
names(DESIGN_WORDS) <- c(DESIGN_BALANCED, DESIGN_UNBAL_HOMO, DESIGN_BAL_UNEQ,
                         DESIGN_UNBAL_POS, DESIGN_UNBAL_NEG)

## Only the six strategies these grids simulate.
STRATEGY_LABELS <- c(
  "1. Fisher always" = "F",
  "2. Welch always" = "W",
  "3. Levene-gated Fisher/Welch" = "L",
  "4. Kruskal-Wallis always" = "KW",
  "5. Shapiro-Wilk routed Welch/KW" = "SW",
  "6. Shapiro-Wilk plus Levene" = "SW+L"
)
STRATEGY_SHAPES <- c(
  "1. Fisher always" = 0, "2. Welch always" = 2,
  "3. Levene-gated Fisher/Welch" = 5, "4. Kruskal-Wallis always" = 4,
  "5. Shapiro-Wilk routed Welch/KW" = 1, "6. Shapiro-Wilk plus Levene" = 1
)
STRATEGY_SIZES <- c(
  "1. Fisher always" = 4.0, "2. Welch always" = 3.2,
  "3. Levene-gated Fisher/Welch" = 3.6, "4. Kruskal-Wallis always" = 3.8,
  "5. Shapiro-Wilk routed Welch/KW" = 5.8, "6. Shapiro-Wilk plus Levene" = 7.2
)
STRATEGY_COLOURS <- c(
  "1. Fisher always" = "#B79F00", "2. Welch always" = "#56B4E9",
  "3. Levene-gated Fisher/Welch" = "#009E73", "4. Kruskal-Wallis always" = "#000000",
  "5. Shapiro-Wilk routed Welch/KW" = "#D55E00", "6. Shapiro-Wilk plus Levene" = "#0072B2"
)

NS_TO_PLOT <- c(10, 20, 30, 50, 100)
groups <- c("A", "B", "C", "D")
group_cols <- fleishman_group_cols
xlim <- c(-2.5, 5)
y_cap <- 0.7

panel_title <- function(panel) {
  one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
  if (nrow(one) != 1) stop("Unknown Fleishman panel: ", panel)
  if (panel == 1) return("N(0, 1)\n\n\nskew = 0\nexcess kurtosis = 0")
  sprintf(
    paste("Fleishman polynomial", "a = %.3f, b = %.3f", "c = -a, d = %.3f",
          "skew = %s", "excess kurtosis = %s", sep = "\n"),
    one$a, one$b, one$d, one$skew, one$excess_kurtosis
  )
}

parse_vec <- function(s) as.numeric(strsplit(as.character(s), ",\\s*")[[1]])

## ---------------------------------------------------------------------------
## Density strip. Two differences from route1_power_figure.R, both consequences
## of rescaling the shifts to hold an effect size fixed:
##
##   * the shift vector differs between DESIGNS, so each design needs its own
##     strip rather than one strip per SD vector;
##   * for the eta_H^2 grid it differs between PANELS as well, because eta_H^2
##     depends on the shape of the distribution, so each column carries its own
##     shifts. `shifts_by_panel` is therefore a named list, one vector per panel.
##
## When the shifts are the same in every panel the legend states them; when they
## are not, it states the SDs only and the per-column shifts are left to the
## curves and the mean/median reference lines.
## ---------------------------------------------------------------------------
make_pdf_panel <- function(panels, sd_vec, shifts_by_panel, panel_letter, panel_description) {
  num <- function(x) format(x, trim = TRUE, drop0trailing = TRUE)
  shift_mat <- do.call(rbind, shifts_by_panel[as.character(panels)])
  shifts_constant <- all(apply(round(shift_mat, 6), 2, function(z) length(unique(z)) == 1))

  legend_labels <- if (shifts_constant) {
    sprintf("%s (mean shift = %s, SD = %s)", groups,
            num(round(shift_mat[1, ], 2)), num(sd_vec))
  } else {
    sprintf("%s (SD = %s; shifts differ by column)", groups, num(sd_vec))
  }
  names(legend_labels) <- groups
  lev <- vapply(panels, panel_title, character(1))

  pdf_rows <- list(); idx <- 1
  for (panel in panels) {
    sh <- shifts_by_panel[[as.character(panel)]]
    for (i in seq_along(sh)) {
      x <- seq(xlim[1], xlim[2], length.out = 700)
      dens <- fleishman_scaled_density(x, panel, sd = sd_vec[i], shift = sh[i])
      dens[!is.finite(dens)] <- NA_real_
      pdf_rows[[idx]] <- data.frame(
        distribution = panel_title(panel), group = groups[i],
        x = x, density = dens, stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  pdf_data <- do.call(rbind, pdf_rows)
  pdf_data$distribution <- factor(pdf_data$distribution, levels = lev)

  panel_numbers <- data.frame(
    distribution = factor(lev, levels = lev),
    number = paste0(panels, ")"), x = xlim[1], y = y_cap, stringsAsFactors = FALSE
  )
  reference_lines <- do.call(rbind, lapply(panels, function(panel) {
    one <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
    sh <- shifts_by_panel[[as.character(panel)]]
    data.frame(
      distribution = factor(panel_title(panel), levels = lev),
      group = factor(rep(groups, 2), levels = groups),
      value = c(sh, one$a * sd_vec + sh),
      line_type = factor(rep(c("mean", "median"), each = length(groups)),
                         levels = c("mean", "median"))
    )
  }))

  ggplot2$ggplot(pdf_data,
      ggplot2$aes(x = x, y = density, colour = group, group = group)) +
    ggplot2$geom_vline(
      data = reference_lines,
      ggplot2$aes(xintercept = value, colour = group, linetype = line_type),
      linewidth = 0.42, alpha = 0.75, inherit.aes = FALSE,
      show.legend = c(colour = FALSE, linetype = TRUE)
    ) +
    ggplot2$geom_line(linewidth = 0.7, na.rm = TRUE) +
    ggplot2$geom_text(
      data = panel_numbers, ggplot2$aes(x = x, y = y, label = number),
      inherit.aes = FALSE, family = FLEISHMAN_FONT_FAMILY, fontface = "plain",
      hjust = 0, vjust = -2.0, size = FLEISHMAN_GEOM_TEXT$panel_number
    ) +
    ggplot2$facet_wrap(~distribution, nrow = 1) +
    ggplot2$coord_cartesian(xlim = xlim, ylim = c(0, y_cap), clip = "off") +
    ggplot2$scale_colour_manual(values = group_cols, breaks = groups,
                                labels = legend_labels,
                                name = "group,\ngroup mean offset") +
    ggplot2$scale_linetype_manual(values = c(mean = "dashed", median = "dotted"),
                                  name = "reference line") +
    ggplot2$labs(title = fleishman_panel_title(panel_letter, panel_description),
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
      text = ggplot2$element_text(family = FLEISHMAN_FONT_FAMILY)
    )
}

## ---------------------------------------------------------------------------
## Power panel. The effect size is constant within a design (that is the point
## of these grids), so it is stated once in the row header, after the n and SD
## vectors -- never repeated column by column.
## ---------------------------------------------------------------------------
## `es_value` is a vector, one entry per panel, always recomputed from the design
## constants the simulation actually used (never read from a stored column). The
## header prints that value and nothing else: one number when the panels agree,
## the range when they do not, so a row can never claim an effect size it does
## not have.
## An effect size may be printed here ONLY if it has a published population
## definition, because a row header states a property of the design, not a
## sample summary. omega^2 qualifies: Eqs. (omega-sq-population),
## (-unbalanced) and (-heteroscedastic) of _effect_size_table.Rmd define it from
## (mu_j, sigma_j^2, p_j), citing Steiger (2004), Carroll & Nordholm (1975) and
## Shieh (2012).
##
## eta_H^2 does not qualify: no source we have read defines a population
## counterpart of (H - k + 1)/(N - k). Passing es_label = NA omits the clause.
## Note this is a citation objection, not an N-dependence one: measured across
## n per group 10, 20, 30, 50, 100, 200 the sample quantity is stable at
## 0.0681, 0.0699, 0.0701, 0.0700, 0.0697, 0.0702 (balanced equal SD, panel 1),
## so it does converge. A stable estimator is still not a population parameter
## while nothing in the literature names its limit.
row_header <- function(design_name, nmult, sdvec, es_label, es_value) {
  base <- sprintf(
    paste0("%s; (n<sub>1</sub>, n<sub>2</sub>, n<sub>3</sub>, n<sub>4</sub>) = ",
           "n&#772;(%s); (SD<sub>1</sub>, SD<sub>2</sub>, SD<sub>3</sub>, SD<sub>4</sub>) = (%s)"),
    DESIGN_WORDS[[design_name]], nmult, sdvec
  )
  if (length(es_label) != 1 || is.na(es_label)) return(base)
  fmt <- function(x) format(round(x, 4), nsmall = 4)
  es_txt <- if (diff(range(es_value)) < 5e-4) fmt(es_value[1]) else
    sprintf("%s&ndash;%s by column", fmt(min(es_value)), fmt(max(es_value)))
  paste0(base, sprintf("; %s = %s", es_label, es_txt))
}

make_power_plot <- function(power, design_name, panel_letter, header, show_legend = TRUE) {
  dat <- power[power$design == design_name, , drop = FALSE]
  if (nrow(dat) == 0) stop("No power results for design: ", design_name)

  rows <- list(
    transform(dat, strategy = "1. Fisher always", power = fisher_power),
    transform(dat, strategy = "2. Welch always", power = welch_power),
    transform(dat, strategy = "3. Levene-gated Fisher/Welch", power = mean_power),
    transform(dat, strategy = "4. Kruskal-Wallis always", power = rank_power),
    transform(dat, strategy = "5. Shapiro-Wilk routed Welch/KW", power = sw_power),
    transform(dat, strategy = "6. Shapiro-Wilk plus Levene", power = gate_power)
  )
  long <- do.call(rbind, rows)
  long$strategy <- factor(long$strategy, levels = names(STRATEGY_LABELS))
  long <- subset(long, n_per_group %in% NS_TO_PLOT)

  ## SW+L routing inset, as in route1_power_figure.R.
  base <- subset(dat, n_per_group %in% NS_TO_PLOT)
  gate_vals <- rbind(
    transform(base, gate_row = "F",  gate_y = 0.085, gate_rate = route_fisher_probability),
    transform(base, gate_row = "W",  gate_y = 0.055, gate_rate = route_welch_probability),
    transform(base, gate_row = "KW", gate_y = 0.025, gate_rate = route_rank_probability)
  )
  gate_vals$gate_rate_label <- sprintf("%.0f", 100 * gate_vals$gate_rate)
  gate_rows <- unique(gate_vals[c("power_panel", "gate_row", "gate_y")])
  gate_title <- unique(base["power_panel"]); gate_title$gate_title <- "SW+L selection (%)"

  ggplot2$ggplot() +
    ggplot2$geom_vline(xintercept = NS_TO_PLOT, colour = "grey88", linewidth = 0.35) +
    ggplot2$geom_hline(yintercept = seq(0.2, 1, by = 0.1), colour = "grey88", linewidth = 0.35) +
    ggplot2$geom_point(
      data = long,
      ggplot2$aes(x = n_per_group, y = power, colour = strategy,
                  shape = strategy, size = strategy, group = strategy),
      stroke = 1.15
    ) +
    ggplot2$geom_text(
      data = gate_title, ggplot2$aes(x = 26, y = 0.125, label = gate_title),
      colour = "grey25", family = FLEISHMAN_FONT_FAMILY, size = FLEISHMAN_GEOM_TEXT$inset
    ) +
    ggplot2$geom_text(
      data = gate_rows, ggplot2$aes(x = 5.8, y = gate_y, label = gate_row),
      colour = "grey25", family = FLEISHMAN_FONT_FAMILY, size = FLEISHMAN_GEOM_TEXT$inset
    ) +
    ggplot2$geom_text(
      data = gate_vals,
      ggplot2$aes(x = n_per_group, y = gate_y, label = gate_rate_label),
      colour = "grey25", family = FLEISHMAN_FONT_FAMILY, size = FLEISHMAN_GEOM_TEXT$inset
    ) +
    ggplot2$facet_grid(stats::as.formula(". ~ power_panel")) +
    ggplot2$scale_y_continuous(limits = c(0, 1), breaks = seq(0.1, 1, by = 0.1),
                               labels = scales::percent) +
    ggplot2$scale_x_log10(breaks = NS_TO_PLOT, limits = c(5.5, 130)) +
    ggplot2$scale_shape_manual(values = STRATEGY_SHAPES, breaks = names(STRATEGY_LABELS),
                               labels = STRATEGY_LABELS, name = "test strategy") +
    ggplot2$scale_size_manual(values = STRATEGY_SIZES, breaks = names(STRATEGY_LABELS),
                              labels = STRATEGY_LABELS, name = "test strategy") +
    ggplot2$scale_colour_manual(values = STRATEGY_COLOURS, breaks = names(STRATEGY_LABELS),
                                labels = STRATEGY_LABELS, name = "test strategy") +
    ggplot2$labs(title = fleishman_panel_title(panel_letter, header),
                 x = "n per group", y = "simulated rejection rate") +
    ggplot2$theme_minimal(base_size = 10) +
    ggplot2$theme(
      legend.position = if (show_legend) "right" else "none",
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
      text = ggplot2$element_text(family = FLEISHMAN_FONT_FAMILY)
    )
}

## ---------------------------------------------------------------------------
## Entry point used by the two wrapper scripts.
##   results_file : CSV written by route1_power_omega_fixed.R / _etaH_fixed.R
##   es_column    : "omega_sq" or "eta_h_sq"
##   es_label     : rendered into the row header, e.g. "&omega;<sup>2</sup>"
##   out_prefix   : output PNG prefix
## ---------------------------------------------------------------------------
build_fixed_es_figures <- function(results_file, es_column, es_label, out_prefix,
                                   outdir = ".") {
  if (!file.exists(results_file)) stop("Results file not found: ", results_file)
  power <- read.csv(results_file, stringsAsFactors = FALSE)
  if (!es_column %in% names(power)) {
    stop("Column '", es_column, "' not in ", basename(results_file))
  }

  panels <- sort(unique(power$panel))
  power$power_panel <- factor(paste0(power$panel, ")"), levels = paste0(panels, ")"))

  ## NEVER label a row with the effect size stored in the CSV. The omega^2 grid
  ## was simulated before omega_scaling_helpers.R was corrected to the cited
  ## Shieh (2012) / Kulinskaya & Staudte (2006) construction, so its stored
  ## omega_sq column reads 0.0725 in every design while the shifts it actually
  ## used give 0.0633-0.1024 in the three heteroscedastic ones. Labelling from
  ## the stored column put a false number on three of five rows.
  ##
  ## The label is therefore always RECOMPUTED from the shifts and SDs the
  ## simulation actually used, by the same function that defines omega^2 for the
  ## rest of the package. eta_H^2 has no such closed form, so for that grid the
  ## stored column is used and the row header says it was solved for.
  ## Only omega^2 may be printed in a row header at all (see row_header): it is
  ## a function of the design constants and does not move with N. eta_H^2 does
  ## move with N, so `es_label = NA` suppresses the clause for that grid and
  ## `es_column` is then used only for the constancy check below.
  es_recompute <- identical(es_column, "omega_sq")
  if (es_recompute) source(file.path(SIMDIR, "omega_scaling_helpers.R"), local = FALSE)

  designs <- c(DESIGN_BALANCED, DESIGN_BAL_UNEQ, DESIGN_UNBAL_HOMO,
               DESIGN_UNBAL_POS, DESIGN_UNBAL_NEG)
  nmult <- c(DESIGN_BALANCED = "1, 1, 1, 1", DESIGN_BAL_UNEQ = "1, 1, 1, 1",
             DESIGN_UNBAL_HOMO = "0.5, 0.8, 1.2, 1.5",
             DESIGN_UNBAL_POS = "0.5, 0.8, 1.2, 1.5",
             DESIGN_UNBAL_NEG = "0.5, 0.8, 1.2, 1.5")
  names(nmult) <- designs

  spec <- lapply(designs, function(d) {
    sub <- power[power$design == d, , drop = FALSE]
    one <- sub[1, ]
    ## One shift vector per panel: identical across panels for the omega^2 grid,
    ## panel-specific for the eta_H^2 grid (see make_pdf_panel).
    shifts_by_panel <- lapply(panels, function(p) {
      parse_vec(sub$group_mean_offsets[sub$panel == p][1])
    })
    names(shifts_by_panel) <- as.character(panels)
    sd_vec <- parse_vec(one$sd_per_group)
    es_val <- if (es_recompute) {
      vapply(shifts_by_panel, function(sh) {
        population_omega_sq(parse_vec(nmult[[d]]), sd_vec, sh, 1)
      }, numeric(1))
    } else {
      vapply(panels, function(p) sub[[es_column]][sub$panel == p][1], numeric(1))
    }
    list(design = d, sd = sd_vec, shifts_by_panel = shifts_by_panel,
         es = es_val, sd_txt = one$sd_per_group,
         es_stored = one[[es_column]])
  })
  names(spec) <- designs

  ## Panel letters alternate: density strip, then the power panel beneath it.
  letters_pdf   <- c("A", "C", "A", "C", "E")
  letters_power <- c("B", "D", "B", "D", "F")

  mk <- function(d, li, show_legend = TRUE) {
    s <- spec[[d]]
    p_pdf <- make_pdf_panel(
      panels, s$sd, s$shifts_by_panel, letters_pdf[li],
      sprintf("input distributions, SD = (%s), shifts rescaled to hold the effect size fixed",
              s$sd_txt)
    )
    p_pow <- make_power_plot(
      power, d, letters_power[li],
      row_header(d, nmult[[d]], s$sd_txt, es_label, s$es),
      show_legend = show_legend
    )
    list(pdf = p_pdf, power = p_pow)
  }

  bal <- list(mk(DESIGN_BALANCED, 1), mk(DESIGN_BAL_UNEQ, 2))
  unb <- list(mk(DESIGN_UNBAL_HOMO, 3), mk(DESIGN_UNBAL_POS, 4), mk(DESIGN_UNBAL_NEG, 5))

  combined_balanced <- patchwork$wrap_plots(
    bal[[1]]$pdf, bal[[1]]$power, bal[[2]]$pdf, bal[[2]]$power,
    ncol = 1, heights = c(1, 2.0, 1, 2.0)
  )
  combined_unbalanced <- patchwork$wrap_plots(
    unb[[1]]$pdf, unb[[1]]$power, unb[[2]]$pdf, unb[[2]]$power,
    unb[[3]]$pdf, unb[[3]]$power,
    ncol = 1, heights = c(1, 2.0, 1, 2.0, 1, 2.0)
  )

  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  f_bal <- file.path(outdir, paste0(out_prefix, "_balanced.png"))
  f_unb <- file.path(outdir, paste0(out_prefix, "_unbalanced.png"))
  ggplot2$ggsave(f_bal, combined_balanced, width = 20, height = 27.5, dpi = FLEISHMAN_DPI)
  ggplot2$ggsave(f_unb, combined_unbalanced, width = 20, height = 27.5 * 3 / 2,
                 dpi = FLEISHMAN_DPI)
  message("Saved:\n  ", f_bal, "\n  ", f_unb)
  invisible(c(f_bal, f_unb))
}
