## ---------------------------------------------------------------------------
## Population eta_H^2 of a Route 1 design, computed exactly (by quadrature),
## not estimated by simulation.
##
## Derivation. The Kruskal-Wallis statistic is
##
##   H = 12 / (N(N+1)) * sum_j n_j (Rbar_j - (N+1)/2)^2,
##
## where Rbar_j is the mean rank of group j among all N observations. Writing
## the estimated weighted relative effect of group j as
##
##   rhat_j = (Rbar_j - 1/2) / N,
##
## we have Rbar_j - (N+1)/2 = N (rhat_j - 1/2), so
##
##   H = 12 N^2 / (N+1) * sum_j p_j (rhat_j - 1/2)^2,      p_j = n_j / N.
##
## H therefore grows linearly in N, and the (N-k) denominator of the sample
## effect size removes that growth:
##
##   eta_H^2 = (H - k + 1) / (N - k)  ->  12 * sum_j p_j (r_j - 1/2)^2,   (*)
##
## with the population weighted relative effect
##
##   r_j = integral G dF_j,        G = sum_l p_l F_l.
##
## This is the same cancellation that makes the sample omega^2 converge: there
## nu_2 = N - k removes the linear growth of nu_1 F.
##
## Note that r_j is Brunner's WEIGHTED relative effect, so (*) depends on the
## allocation fractions p_j and not on the distributions alone. eta_H^2 is a
## property of the design together with the distributions -- which is exactly
## the sample-size dependence of weighted effects discussed in Brunner et al.
## (2017, JRSSB, p. 1464).
##
## Evaluation. Each group is X_j = sd_j * P(Z) + shift_j with Z ~ N(0,1) and
## P the Fleishman polynomial of the panel. P is strictly increasing for every
## panel used here (its derivative b + 2cz + 3dz^2 has negative discriminant,
## and panel 1 is the identity), so t_j(z) = sd_j * P(z) + shift_j is a
## monotone reparametrisation and
##
##   F_j(y) = Phi( t_j^{-1}(y) ),      r_j = E_Z[ G( t_j(Z) ) ].
##
## Both are evaluated on one fine z-grid: t_j is tabulated on the grid, F_l is
## obtained by linear interpolation of Phi against t_l, and the expectation is
## a trapezoidal integral against the normal density.
## ---------------------------------------------------------------------------

## Fine grid on the standard normal scale. +-9 SD covers the tail to ~1e-19.
.ETA_Z <- seq(-9, 9, length.out = 200001L)
.ETA_PHI <- stats::dnorm(.ETA_Z)
.ETA_PNORM <- stats::pnorm(.ETA_Z)

## Fleishman polynomial of a panel, evaluated on z.
fleishman_poly <- function(z, panel) {
  o <- fleishman_cases[fleishman_cases$panel == panel, , drop = FALSE]
  if (nrow(o) != 1) stop("Unknown Fleishman panel: ", panel)
  o$a + o$b * z + o$c * z^2 + o$d * z^3
}

## Exact population eta_H^2 = 12 * sum_j p_j (r_j - 1/2)^2.
population_eta_h_sq <- function(multipliers, sd_vec, shifts, panel) {
  k <- length(shifts)
  stopifnot(length(multipliers) == k, length(sd_vec) == k)
  p <- multipliers / sum(multipliers)

  base <- fleishman_poly(.ETA_Z, panel)          # P(z) on the grid
  tt <- lapply(seq_len(k), function(j) sd_vec[j] * base + shifts[j])

  ## F_l evaluated at arbitrary y, by inverting the monotone map t_l.
  Fl <- function(y, l) {
    stats::approx(x = tt[[l]], y = .ETA_PNORM, xout = y,
                  yleft = 0, yright = 1, ties = "ordered")$y
  }
  ## G = sum_l p_l F_l
  G <- function(y) {
    out <- numeric(length(y))
    for (l in seq_len(k)) out <- out + p[l] * Fl(y, l)
    out
  }
  ## r_j = E_Z[ G(t_j(Z)) ], trapezoidal against the normal density.
  trap <- function(fx) {
    w <- diff(.ETA_Z)
    sum((fx[-1] + fx[-length(fx)]) / 2 * w)
  }
  r <- vapply(seq_len(k), function(j) trap(G(tt[[j]]) * .ETA_PHI), numeric(1))

  12 * sum(p * (r - 0.5)^2)
}
