set.seed(20260605)

out_dir <- file.path("dev", "codextrial20260605_route1_levene_panel_old_vs_new")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

n <- c(A = 12, B = 20, C = 35, D = 50)
treatment <- factor(rep(names(n), n))
score <- c(
  rnorm(n["A"], mean = 0, sd = 1.0),
  rgamma(n["B"], shape = 2, scale = 1) - 2,
  rnorm(n["C"], mean = 0, sd = 2.0),
  rt(n["D"], df = 4)
)

fit <- aov(score ~ treatment)
r <- rstandard(fit)
group_mean_r <- ave(r, treatment, FUN = function(x) mean(x, na.rm = TRUE))

old_y <- abs(r)
new_y <- abs(r - group_mean_r)

group_id <- as.numeric(treatment)
x_jitter <- jitter(group_id, amount = 0.08)
y_lim <- c(0, max(3, old_y, new_y, na.rm = TRUE) * 1.1)
y_ticks <- seq(0, ceiling(y_lim[2]), by = 1)

plot_panel <- function(y, main, ylab) {
  plot(
    x_jitter, y,
    main = main,
    xlab = "treatment",
    ylab = ylab,
    xlim = c(0.5, length(levels(treatment)) + 0.5),
    ylim = y_lim,
    xaxt = "n",
    yaxt = "n",
    pch = 1,
    col = "grey40"
  )
  axis(1, at = seq_along(levels(treatment)), labels = levels(treatment))
  axis(2, at = y_ticks, las = 1)
  abline(h = 3, col = "grey85", lty = 2, lwd = 1)
  points(
    seq_along(levels(treatment)),
    tapply(y, treatment, mean, na.rm = TRUE),
    pch = 4,
    col = "red",
    cex = 0.9,
    lwd = 1.5
  )
  box()
}

png(
  file.path(out_dir, "route1_levene_panel_old_vs_new.png"),
  width = 2400,
  height = 1100,
  res = 180
)
op <- par(no.readonly = TRUE)
par(mfrow = c(1, 2), mar = c(5, 5, 4, 1), oma = c(0, 0, 3, 0), cex = 0.9)

plot_panel(
  old_y,
  "current panel",
  expression(abs(r[i]))
)

plot_panel(
  new_y,
  "Levene-matched panel",
  expression(abs(r[i] - bar(r)[g[i]]))
)

mtext(
  sprintf(
    "Route 1 one-way model: max |old - new| = %.2e; group means of r_i are zero up to rounding",
    max(abs(old_y - new_y), na.rm = TRUE)
  ),
  outer = TRUE,
  cex = 0.9,
  line = 1
)

par(op)
dev.off()

write.csv(
  data.frame(
    treatment = names(tapply(r, treatment, mean)),
    mean_r_i = as.numeric(tapply(r, treatment, mean)),
    mean_abs_old = as.numeric(tapply(old_y, treatment, mean)),
    mean_abs_new = as.numeric(tapply(new_y, treatment, mean))
  ),
  file.path(out_dir, "route1_levene_panel_old_vs_new_summary.csv"),
  row.names = FALSE
)
