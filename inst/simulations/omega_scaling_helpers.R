## ---------------------------------------------------------------------------
## Population omega^2 and shift-scale factors for the Route 1 power grid.
##
## Three distinct formulas apply depending on the design, matching the three
## equations in vignettes/children/_effect_size_table.Rmd:
##
##   omega^2_bal    balanced, homoscedastic       (Steiger 2004)
##   omega^2_unb    unbalanced, homoscedastic     (Carroll & Nordholm 1975,
##                                                  reporting Hays 1963)
##   omega^2_het    heteroscedastic (any balance) (Shieh 2012, reporting
##                                                  Kulinskaya & Staudte 2006)
##
## population_omega_sq() always evaluates the general (heteroscedastic)
## formula; it is algebraically identical to the other two in their special
## cases (verified numerically to 6 decimals against direct computation of
## each of the three formulas), so one implementation is correct everywhere.
## omega_sq_regime() classifies which of the three a given design falls
## into, purely for labelling -- the number itself does not depend on which
## label is used.
##
## Notation (all population quantities, no estimates):
##   k          number of groups
##   n_j        size of group j,  N = sum_j n_j
##   p_j        = n_j / N, the allocation fraction of group j
##   delta_j    the group's entry in the base shift vector
##   c          the common scale, so that mu_j = c * delta_j
##   sigma_j^2  the variance of group j
##
## General formula (Shieh 2012, Eqs. 9-10, reporting Kulinskaya & Staudte
## 2006): with w_j = n_j / sigma_j^2 and the inverse-variance-weighted grand
## mean mu_tilde_w = sum_j w_j mu_j / sum_j w_j,
##
##   lambda   = sum_j p_j ((mu_j - mu_tilde_w) / sigma_j)^2
##   omega^2  = lambda / (1 + lambda)
##
## This reduces to the balanced formula when p_j = 1/k and sigma_j = sigma
## for all j (mu_tilde_w becomes the unweighted grand mean, sigma_j cancels
## to a constant sigma, and lambda = sigma^2_between / sigma^2), and to the
## unbalanced-homoscedastic formula when only sigma_j = sigma holds.
## ---------------------------------------------------------------------------

## Allocation-weighted variance of x under allocation fractions p.
weighted_var <- function(x, p) {
  m <- sum(p * x)
  sum(p * (x - m)^2)
}

## Population omega^2 of a design, for a given scale c (mu_j = c * shifts_j).
## Always the general (heteroscedastic) formula; see header for why this is
## correct in the balanced and unbalanced-homoscedastic special cases too.
population_omega_sq <- function(multipliers, sd_vec, shifts, c) {
  p <- multipliers / sum(multipliers)
  w <- multipliers / sd_vec^2
  mu <- c * shifts
  mu_tilde_w <- sum(w * mu) / sum(w)
  lambda <- sum(p * ((mu - mu_tilde_w) / sd_vec)^2)
  lambda / (1 + lambda)
}

## Which of the three cited formulas actually applies to a design, for
## labelling. Returns "bal", "unbal", or "het".
omega_sq_regime <- function(multipliers, sd_vec) {
  k <- length(multipliers)
  balanced <- isTRUE(all.equal(multipliers / sum(multipliers), rep(1 / k, k)))
  homoscedastic <- length(unique(sd_vec)) == 1
  if (!homoscedastic) "het" else if (balanced) "bal" else "unbal"
}

## The balanced homoscedastic baseline: p_j = 1/k, sigma_j = 1, c = 1.
baseline_omega_sq <- function(shifts) {
  k <- length(shifts)
  population_omega_sq(rep(1, k), rep(1, k), shifts, 1)
}

## Scale holding omega^2 at the balanced homoscedastic baseline, for any
## design (including heteroscedastic ones). Closed form: because
## mu_tilde_w(c) = c * (sum_j w_j delta_j / sum_j w_j) is linear in c (w_j
## does not depend on c), lambda(c) = c^2 * V*, where
##   V* = sum_j p_j ((delta_j - delta_tilde_w) / sigma_j)^2
## is a constant of the design. Matching lambda(c) to the balanced-baseline
## target lambda_bal = V_u(shifts) (at p_j = 1/k, sigma_j = 1) gives
## c = sqrt(V_u(shifts) / V*). Verified numerically against the achieved
## omega^2 for three designs (balanced/unbalanced heteroscedastic), matching
## the target to 6 decimals.
scale_omega_fixed <- function(multipliers, sd_vec, shifts) {
  k <- length(shifts)
  p <- multipliers / sum(multipliers)
  w <- multipliers / sd_vec^2
  delta_tilde_w <- sum(w * shifts) / sum(w)
  Vstar <- sum(p * ((shifts - delta_tilde_w) / sd_vec)^2)
  Vu <- weighted_var(shifts, rep(1 / k, k))
  sqrt(Vu / Vstar)
}

## What route1_simulations.R currently uses -- correct only for balanced
## designs (see effect_sizes_by_design_panel.R for the discrepancy this
## causes in the unbalanced and heteroscedastic designs).
scale_legacy <- function(multipliers, sd_vec, shifts) sqrt(mean(sd_vec^2))
