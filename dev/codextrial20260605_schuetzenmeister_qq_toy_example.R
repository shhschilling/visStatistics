set.seed(20260605)

out_dir <- file.path("dev", "codextrial20260605_schuetzenmeister_qq_toy_example")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fit_one_way <- function(kind = c("normal", "skewed"), n_per_group = 25) {
  kind <- match.arg(kind)
  group <- factor(rep(LETTERS[1:4], each = n_per_group))
  if (kind == "normal") {
    y <- rnorm(length(group))
  } else {
    y <- rgamma(length(group), shape = 1, scale = 1) - 1
  }
  lm(y ~ group)
}

standardised_residuals <- function(fit) {
  residuals(fit) / (sigma(fit) * sqrt(1 - hatvalues(fit)))
}

simulated_residual_orders <- function(fit, nsim = 5000) {
  x <- model.matrix(fit)
  fitted_values <- fitted(fit)
  sigma_hat <- sigma(fit)
  h <- hatvalues(fit)
  qr_x <- qr(x)
  rank_x <- qr_x$rank
  df <- length(fitted_values) - rank_x

  replicate(nsim, {
    y_sim <- fitted_values + rnorm(length(fitted_values), sd = sigma_hat)
    fit_sim <- lm.fit(x = x, y = y_sim)
    sigma_sim <- sqrt(sum(fit_sim$residuals^2) / df)
    sort(fit_sim$residuals / (sigma_sim * sqrt(1 - h)))
  })
}

band_limits <- function(sim_orders, alpha = 0.05) {
  n <- nrow(sim_orders)

  pointwise <- apply(
    sim_orders, 1, quantile,
    probs = c(alpha / 2, 1 - alpha / 2),
    names = FALSE
  )

  bonferroni <- apply(
    sim_orders, 1, quantile,
    probs = c(alpha / (2 * n), 1 - alpha / (2 * n)),
    names = FALSE
  )

  coverage_for <- function(local_alpha) {
    lim <- apply(
      sim_orders, 1, quantile,
      probs = c(local_alpha / 2, 1 - local_alpha / 2),
      names = FALSE
    )
    mean(colSums(sim_orders >= lim[1, ] & sim_orders <= lim[2, ]) == n)
  }

  lower_alpha <- alpha / n
  upper_alpha <- alpha
  for (i in seq_len(35)) {
    mid_alpha <- (lower_alpha + upper_alpha) / 2
    if (coverage_for(mid_alpha) > 1 - alpha) {
      lower_alpha <- mid_alpha
    } else {
      upper_alpha <- mid_alpha
    }
  }
  simultaneous_alpha <- lower_alpha

  simultaneous <- apply(
    sim_orders, 1, quantile,
    probs = c(simultaneous_alpha / 2, 1 - simultaneous_alpha / 2),
    names = FALSE
  )

  list(
    expected = rowMeans(sim_orders),
    pointwise = pointwise,
    simultaneous = simultaneous,
    bonferroni = bonferroni,
    simultaneous_alpha = simultaneous_alpha,
    simultaneous_coverage = coverage_for(simultaneous_alpha),
    pointwise_coverage = mean(
      colSums(sim_orders >= pointwise[1, ] & sim_orders <= pointwise[2, ]) == n
    ),
    bonferroni_coverage = mean(
      colSums(sim_orders >= bonferroni[1, ] & sim_orders <= bonferroni[2, ]) == n
    )
  )
}

qq_object <- function(fit, nsim = 5000) {
  sim_orders <- simulated_residual_orders(fit, nsim = nsim)
  bands <- band_limits(sim_orders)
  list(
    observed = sort(standardised_residuals(fit)),
    bands = bands
  )
}

plot_schuetzenmeister_qq <- function(obj, main) {
  x <- obj$bands$expected
  y <- obj$observed
  y_lim <- range(
    y,
    obj$bands$pointwise,
    obj$bands$simultaneous,
    obj$bands$bonferroni,
    finite = TRUE
  )

  plot(
    x, y,
    type = "n",
    main = main,
    xlab = "expected ordered standardised residuals",
    ylab = "observed ordered standardised residuals",
    ylim = y_lim,
    las = 1
  )

  polygon(
    c(x, rev(x)),
    c(obj$bands$simultaneous[1, ], rev(obj$bands$simultaneous[2, ])),
    col = adjustcolor("grey75", alpha.f = 0.55),
    border = NA
  )
  lines(x, obj$bands$pointwise[1, ], col = "#2F6FAE", lty = 2, lwd = 1.2)
  lines(x, obj$bands$pointwise[2, ], col = "#2F6FAE", lty = 2, lwd = 1.2)
  lines(x, obj$bands$simultaneous[1, ], col = "grey25", lty = 1, lwd = 1.2)
  lines(x, obj$bands$simultaneous[2, ], col = "grey25", lty = 1, lwd = 1.2)
  lines(x, obj$bands$bonferroni[1, ], col = "grey25", lty = 3, lwd = 1.2)
  lines(x, obj$bands$bonferroni[2, ], col = "grey25", lty = 3, lwd = 1.2)
  abline(0, 1, col = "red", lwd = 1.3)
  points(x, y, pch = 1, col = "black", lwd = 1)

  legend(
    "topleft",
    legend = c(
      "observed residuals",
      "normal reference",
      "pointwise 95% TB",
      "simultaneous 95% STB",
      "Bonferroni 95% TB"
    ),
    pch = c(1, NA, NA, NA, NA),
    lty = c(NA, 1, 2, 1, 3),
    lwd = c(NA, 1.3, 1.2, 1.2, 1.2),
    col = c("black", "red", "#2F6FAE", "grey25", "grey25"),
    bty = "n",
    cex = 0.78
  )
}

normal_fit <- fit_one_way("normal")
skewed_fit <- fit_one_way("skewed")
normal_qq <- qq_object(normal_fit)
skewed_qq <- qq_object(skewed_fit)

png(
  file.path(out_dir, "schuetzenmeister_qq_toy_example.png"),
  width = 2400,
  height = 1200,
  res = 180
)
op <- par(no.readonly = TRUE)
par(mfrow = c(1, 2), mar = c(5, 5, 4, 1), oma = c(0, 0, 3, 0), cex = 0.9)
plot_schuetzenmeister_qq(normal_qq, "normal residuals")
plot_schuetzenmeister_qq(skewed_qq, "skewed residuals")
mtext(
  "Schuetzenmeister-style Q-Q bands for a one-way general linear model; 5000 simulations",
  outer = TRUE,
  line = 1,
  cex = 0.95
)
par(op)
dev.off()

summary_table <- data.frame(
  example = c("normal residuals", "skewed residuals"),
  pointwise_joint_coverage = c(
    normal_qq$bands$pointwise_coverage,
    skewed_qq$bands$pointwise_coverage
  ),
  simultaneous_joint_coverage = c(
    normal_qq$bands$simultaneous_coverage,
    skewed_qq$bands$simultaneous_coverage
  ),
  bonferroni_joint_coverage = c(
    normal_qq$bands$bonferroni_coverage,
    skewed_qq$bands$bonferroni_coverage
  ),
  simultaneous_local_alpha = c(
    normal_qq$bands$simultaneous_alpha,
    skewed_qq$bands$simultaneous_alpha
  )
)

write.csv(
  summary_table,
  file.path(out_dir, "schuetzenmeister_qq_toy_example_summary.csv"),
  row.names = FALSE
)
