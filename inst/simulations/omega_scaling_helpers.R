## ---------------------------------------------------------------------------
## Shift-scale factors for the Route 1 power grid.
##
## Two ways to choose the common scale c applied to the shift vector delta:
##
##   scale_omega_fixed()  holds the population omega^2 at its balanced
##                        homoscedastic baseline value in EVERY design,
##                        including unbalanced ones.
##   scale_legacy()       what route1_simulations.R:303 does,
##                        sqrt(mean(sd^2)); correct only when n_j is constant.
##
## Notation (all quantities are population quantities, no estimates):
##   k          number of groups
##   n_j        size of group j,  N = sum_j n_j
##   p_j        = n_j / N, the allocation fraction of group j
##   delta_j    the group's entry in the base shift vector
##   c          the common scale, so that mu_j = c * delta_j
##   sigma_j^2  the variance of group j
##
## With  mu_bar_p = sum_j p_j mu_j  the allocation-weighted grand mean,
##
##   sigma^2_Effect = sum_j p_j (mu_j - mu_bar_p)^2      (weighted, not 1/k)
##   sigma^2_Error  = sum_j p_j sigma_j^2                 (weighted, not mean)
##   omega^2        = sigma^2_Effect / (sigma^2_Effect + sigma^2_Error)
##
## Writing V_p(delta) = sum_j p_j (delta_j - sum_l p_l delta_l)^2 for the
## allocation-weighted variance of the base shift vector, sigma^2_Effect
## = c^2 V_p(delta), so setting omega^2 equal to the balanced baseline
## omega^2_bal = V_u(delta) / (V_u(delta) + 1), where V_u = V_p at
## p_j = 1/k and sigma_j = 1, and solving for c gives
##
##   c^2 = V_u(delta) * sum_j p_j sigma_j^2 / V_p(delta).
##
## Both factors of that expression are 1 in the balanced homoscedastic case,
## and V_p = V_u whenever the design is balanced, so scale_omega_fixed()
## reduces to sqrt(mean(sigma_j^2)) = scale_legacy() for balanced designs.
## They differ only when the n_j differ.
## ---------------------------------------------------------------------------

## Allocation-weighted variance of x under allocation fractions p.
weighted_var <- function(x, p) {
  m <- sum(p * x)
  sum(p * (x - m)^2)
}

## Population omega^2 of a design, for a given scale c (see header).
population_omega_sq <- function(multipliers, sd_vec, shifts, c) {
  p <- multipliers / sum(multipliers)
  s2_effect <- c^2 * weighted_var(shifts, p)
  s2_error <- sum(p * sd_vec^2)
  s2_effect / (s2_effect + s2_error)
}

## The balanced homoscedastic baseline: p_j = 1/k, sigma_j = 1, c = 1.
baseline_omega_sq <- function(shifts) {
  k <- length(shifts)
  population_omega_sq(rep(1, k), rep(1, k), shifts, 1)
}

## Scale holding omega^2 at the balanced homoscedastic baseline.
scale_omega_fixed <- function(multipliers, sd_vec, shifts) {
  k <- length(shifts)
  p <- multipliers / sum(multipliers)
  p_bal <- rep(1 / k, k)
  sqrt(weighted_var(shifts, p_bal) * sum(p * sd_vec^2) / weighted_var(shifts, p))
}

## What route1_simulations.R currently uses.
scale_legacy <- function(multipliers, sd_vec, shifts) sqrt(mean(sd_vec^2))
