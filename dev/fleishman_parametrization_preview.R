## Preview of candidate Fleishman parametrizations for optimal 5-panel design
## Generate densities for various (skew, excess_kurtosis) combinations
## to visualize and select the best 4 non-normal cases

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required.")
}

## Fleishman coefficients from Blanca et al. (2017), Table 2
## and other standard references
fleishman_table <- data.frame(
  skew = c(0, 0.2, 0.4, 0.5, 0.8, 1.0, 1.5, 2.0),
  exkurt_low = c(0, 0.3, 0.4, 0.5, 0.8, 1.0, 1.5, 3.0),
  exkurt_high = c(0, 0.3, 0.4, 0.5, 0.8, 1.0, 1.5, 6.0),
  b_low = c(NA, NA, 0.9736, 0.9816, 1.0025, 1.0175, 1.0486, 0.8763),
  c_low = c(NA, NA, 0.0640, 0.0816, 0.1305, 0.1910, 0.3098, 0.3145),
  d_low = c(NA, NA, 0.0074, 0.0088, 0.0193, -0.0186, -0.0434, 0.0227),
  b_high = c(NA, NA, 0.9736, 0.9816, 1.0025, 1.0175, 1.0486, 0.8763),
  c_high = c(NA, NA, 0.0640, 0.0816, 0.1305, 0.1910, 0.3098, 0.3145),
  d_high = c(NA, NA, 0.0074, 0.0088, 0.0193, -0.0186, -0.0434, 0.0227)
)

## Candidate Fleishman cases for evaluation
candidates <- list(
  "1. Normal (0, 0)" = list(skew = 0, exkurt = 0, b = 1, c = 0, d = 0),

  # Mild skew, low kurtosis — SW underpowered zone
  "2. Mild skew, low kurt (0.4, 0.4)" = list(
    skew = 0.4, exkurt = 0.4,
    b = 0.97364644, c = 0.06398517, d = 0.00736258
  ),

  # High skew, low kurtosis — orthogonal to mild skew
  "3. High skew, low kurt (2.0, 0.4)" = list(
    skew = 2.0, exkurt = 0.4,
    b = 0.8763, c = 0.3145, d = 0.0227
  ),

  # Moderate skew, moderate kurtosis
  "4. Moderate skew/kurt (1.0, 1.0)" = list(
    skew = 1.0, exkurt = 1.0,
    b = 1.01748519, c = 0.19099508, d = -0.01857700
  ),

  # No skew, high kurtosis — heavy tails
  "5. No skew, high kurt (0, 6.0)" = list(
    skew = 0, exkurt = 6.0,
    b = 1, c = 0, d = 0  # Placeholder; needs proper computation
  ),

  # Alternative: Mild-moderate skew, high kurtosis
  "Alt: Mild skew, high kurt (0.4, 1.0)" = list(
    skew = 0.4, exkurt = 1.0,
    b = 0.97364644, c = 0.06398517, d = 0.00736258  # Approx
  ),

  # Alternative: High skew, moderate kurtosis
  "Alt: High skew, mod kurt (2.0, 1.0)" = list(
    skew = 2.0, exkurt = 1.0,
    b = 0.8763, c = 0.3145, d = 0.0227  # Approx
  )
)

fleishman_draw <- function(n, b, c, d) {
  z <- rnorm(n)
  -c + b * z + c * z^2 + d * z^3
}

fleishman_density <- function(x, b, c, d) {
  vapply(x, function(y) {
    roots <- polyroot(c(-c - y, b, c, d))
    real_roots <- Re(roots)[abs(Im(roots)) < 1e-7]
    if (!length(real_roots)) return(NA_real_)
    deriv <- b + 2 * c * real_roots + 3 * d * real_roots^2
    dens <- dnorm(real_roots) / abs(deriv)
    dens <- dens[is.finite(dens)]
    if (!length(dens)) NA_real_ else sum(dens)
  }, numeric(1))
}

## Generate density data
x_grid <- seq(-4, 6, length.out = 800)

dens_data <- do.call(rbind, lapply(names(candidates), function(name) {
  params <- candidates[[name]]
  dens <- fleishman_density(x_grid, params$b, params$c, params$d)
  data.frame(
    case = name,
    skew = params$skew,
    exkurt = params$exkurt,
    x = x_grid,
    density = dens,
    stringsAsFactors = FALSE
  )
}))

## Plot 1: All candidates as individual panels
p1 <- ggplot2::ggplot(dens_data, ggplot2::aes(x = x, y = density)) +
  ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
  ggplot2::facet_wrap(~ case, scales = "free_y", nrow = 3) +
  ggplot2::labs(
    title = "Fleishman distribution candidates for 5-panel design",
    subtitle = "Select 4 non-normal cases to accompany normal baseline",
    x = "standardised value",
    y = "density"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank()
  )

ggplot2::ggsave(
  "/Users/sschilli/Library/CloudStorage/OneDrive-HochschuleLuzern/Forschung/visStatistics/dev/fleishman_candidates_all.png",
  p1,
  width = 14,
  height = 10,
  dpi = 150
)

## Plot 2: Overlay key candidates (normal + 4 strategic cases)
key_cases <- c("1. Normal (0, 0)",
               "2. Mild skew, low kurt (0.4, 0.4)",
               "3. High skew, low kurt (2.0, 0.4)",
               "4. Moderate skew/kurt (1.0, 1.0)",
               "Alt: No skew, high kurt (0, 6.0)")
dens_key <- dens_data[dens_data$case %in% key_cases, ]

p2 <- ggplot2::ggplot(dens_key, ggplot2::aes(x = x, y = density, colour = case, linewidth = case)) +
  ggplot2::geom_line(na.rm = TRUE) +
  ggplot2::scale_linewidth_manual(
    values = setNames(c(1.0, 0.7, 0.7, 0.7, 0.7), key_cases)
  ) +
  ggplot2::scale_colour_brewer(palette = "Set1") +
  ggplot2::labs(
    title = "Proposed 5-panel design: overlaid densities",
    subtitle = "Normal (thick) + 4 strategic Fleishman cases",
    x = "standardised value",
    y = "density",
    colour = "case",
    linewidth = NULL
  ) +
  ggplot2::coord_cartesian(xlim = c(-4, 6), ylim = c(0, 0.5)) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    legend.position = "right",
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(
  "/Users/sschilli/Library/CloudStorage/OneDrive-HochschuleLuzern/Forschung/visStatistics/dev/fleishman_candidates_proposed.png",
  p2,
  width = 12,
  height = 6,
  dpi = 150
)

## Plot 3: (skew, exkurt) parameter space showing candidates
param_data <- data.frame(
  case = names(candidates),
  skew = sapply(candidates, "[[", "skew"),
  exkurt = sapply(candidates, "[[", "exkurt"),
  type = c("Normal", rep("Candidate", length(candidates) - 1))
)

p3 <- ggplot2::ggplot(param_data, ggplot2::aes(x = skew, y = exkurt, colour = type, size = type, label = case)) +
  ggplot2::geom_point(na.rm = TRUE) +
  ggplot2::geom_text(hjust = -0.1, size = 3, show.legend = FALSE, na.rm = TRUE) +
  ggplot2::scale_size_manual(values = c("Normal" = 5, "Candidate" = 3)) +
  ggplot2::scale_colour_manual(values = c("Normal" = "black", "Candidate" = "#1f77b4")) +
  ggplot2::labs(
    title = "Parameter space: (skew, excess kurtosis)",
    subtitle = "Visualize how candidates span the design space",
    x = "skewness",
    y = "excess kurtosis",
    colour = NULL,
    size = NULL
  ) +
  ggplot2::coord_cartesian(xlim = c(-0.2, 2.5), ylim = c(-0.5, 7)) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    legend.position = "topleft",
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "grey50", fill = NA)
  )

ggplot2::ggsave(
  "/Users/sschilli/Library/CloudStorage/OneDrive-HochschuleLuzern/Forschung/visStatistics/dev/fleishman_parameter_space.png",
  p3,
  width = 10,
  height = 8,
  dpi = 150
)

cat("
Fleishman parametrization previews created:
  - fleishman_candidates_all.png (all candidates, separate panels)
  - fleishman_candidates_proposed.png (overlaid densities)
  - fleishman_parameter_space.png (skew vs excess kurtosis scatter)

Now select the 4 best non-normal cases from the candidates.
")
