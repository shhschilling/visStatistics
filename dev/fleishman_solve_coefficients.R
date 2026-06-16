## Solve Fleishman coefficients numerically for target (skew, excess_kurtosis)
## Y = -c + bZ + cZ^2 + dZ^3, Z ~ N(0,1)


# Target cases for 5-panel design
targets <- list(
  normal = list(skew = 0, exkurt = 0),
  mild_skew_low_kurt = list(skew = 0.4, exkurt = 0.4),
  high_skew_low_kurt = list(skew = 2.0, exkurt = 0.4),
  no_skew_high_kurt = list(skew = 0.0, exkurt = 6.0),
  moderate = list(skew = 1.0, exkurt = 1.0)
)

# Fleishman polynomial moments (theoretical formulas)
fleishman_moments <- function(b, c, d) {
  # For Y = -c + bZ + cZ^2 + dZ^3 where Z ~ N(0,1)
  # E[Z^k] values: E[Z]=0, E[Z^2]=1, E[Z^3]=0, E[Z^4]=3

  m1 <- -c  # E[Y]
  m2 <- b^2 + 2*c^2 + d^2  # E[Y^2] (should = 1 for standardization)
  m3 <- 6*b*c + 8*c*d  # E[Y^3]
  m4 <- 3*b^2 + 24*c*d + 6*c^2 + d^2  # E[Y^4]

  mean <- m1
  var <- m2 - m1^2
  skew <- (m3 - 3*m1*m2 + 2*m1^3) / var^(3/2)
  kurt <- (m4 - 4*m1*m3 + 6*m1^2*m2 - 3*m1^4) / var^2
  exkurt <- kurt - 3

  c(mean = mean, var = var, skew = skew, exkurt = exkurt)
}

# Solve for coefficients given target skew and exkurt
solve_fleishman <- function(target_skew, target_exkurt, verbose = TRUE) {

  if (target_skew == 0 && target_exkurt == 0) {
    return(list(b = 1, c = 0, d = 0))
  }

  # System of equations to solve
  # 1. E[Y] = 0  (gives c = 0 automatically)
  # 2. Var(Y) = 1
  # 3. Skew(Y) = target_skew
  # 4. ExKurt(Y) = target_exkurt

  # With c=0 for centering, solve for b, d
  equations <- function(x) {
    b <- x[1]
    d <- x[2]
    c_val <- 0  # Enforce E[Y] = 0

    moments <- fleishman_moments(b, c_val, d)

    c(
      moments["var"] - 1,           # Constraint 1: variance = 1
      moments["skew"] - target_skew  # Constraint 2: skewness match
    )
  }

  # Objective: minimize squared error
  objective <- function(x) {
    b <- x[1]
    c_val <- x[2]
    d <- x[3]

    moments <- fleishman_moments(b, c_val, d)

    sum((c(
      moments["var"] - 1,
      moments["skew"] - target_skew,
      moments["exkurt"] - target_exkurt
    ))^2)
  }

  best_sol <- NULL
  best_residual <- Inf

  for (b_init in c(0.8, 0.9, 1.0, 1.1, 1.2)) {
    for (c_init in c(-0.2, -0.1, 0, 0.1, 0.2)) {
      for (d_init in c(-0.2, -0.1, 0, 0.1, 0.2)) {
        sol <- optim(c(b_init, c_init, d_init), objective, method = "Nelder-Mead",
                     control = list(maxit = 5000, reltol = 1e-8))
        if (sol$value < best_residual) {
          best_residual <- sol$value
          best_sol <- sol$par
        }
      }
    }
  }

  b_sol <- best_sol[1]
  c_sol <- best_sol[2]
  d_sol <- best_sol[3]

  # Verify solution
  moments_final <- fleishman_moments(b_sol, c_sol, d_sol)
  if (verbose) {
    cat(sprintf("skew=%.2f, exkurt=%.2f: b=%.8f, c=%.8f, d=%.8f\n",
                target_skew, target_exkurt, b_sol, c_sol, d_sol))
    cat(sprintf("  Verified: skew=%.6f, exkurt=%.6f, var=%.6f\n",
                moments_final["skew"], moments_final["exkurt"], moments_final["var"]))
  }

  list(b = b_sol, c = c_sol, d = d_sol,
       actual_skew = moments_final["skew"],
       actual_exkurt = moments_final["exkurt"])
}

# Solve all targets
cat("Solving Fleishman coefficients...\n\n")
solutions <- lapply(targets, function(t) {
  solve_fleishman(t$skew, t$exkurt, verbose = TRUE)
})

# Output as CSV
coef_df <- data.frame(
  case = names(solutions),
  target_skew = sapply(targets, "[[", "skew"),
  target_exkurt = sapply(targets, "[[", "exkurt"),
  b = sapply(solutions, "[[", "b"),
  c = sapply(solutions, "[[", "c"),
  d = sapply(solutions, "[[", "d"),
  row.names = NULL
)

print(coef_df)

write.csv(coef_df,
  file.path("dev", "fleishman_5panel_coefficients.csv"),
  row.names = FALSE)

cat("\n\nCoefficients saved to: dev/fleishman_5panel_coefficients.csv\n")
