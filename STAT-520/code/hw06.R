# =============================================================================
# File Name:     hw06.R
# Author:        Evan Pete Walsh
# Contact:       epwalsh10@gmail.com
# Creation Date: 2016-10-29
# Last Modified: 2016-10-31 11:22:57
# =============================================================================


n <- 20

sim_coverage_improper <- function() {
  y <- rpois(n, 9)

  shape1 <- sum(y) + 1
  rate1 <- n
  post1_lwr <- qgamma(0.05, shape=shape1, rate=rate1)
  post1_upr <- qgamma(0.95, shape=shape1, rate=rate1)

  if ((post1_lwr <= 9) & (post1_upr >= 9)) return(1)
  return(0)
}

sim_coverage_proper <- function() {
  y <- rpois(n, 9)

  shape2 <- sum(y) + 5
  rate2 <- 0.5 + n
  post2_lwr <- qgamma(0.05, shape=shape2, rate=rate2)
  post2_upr <- qgamma(0.95, shape=shape2, rate=rate2)

  if ((post2_lwr <= 9) & (post2_upr >= 9)) return(1)
  return(0)
}

M <- 2000

mc_impr <- sapply(seq(1, M), FUN = function(x) sim_coverage_improper())
est_impr <- sum(mc_impr) / M
mc_prop <- sapply(seq(1, M), FUN = function(x) sim_coverage_proper())
est_prop <- sum(mc_prop) / M

samp_var_impr <- sum((mc_impr - est_impr)^2) / (M * (M-1))
est_impr
est_impr - qnorm(0.975) * sqrt(samp_var_impr)
est_impr + qnorm(0.975) * sqrt(samp_var_impr)
2 * qnorm(0.975) * sqrt(samp_var_impr)

samp_var_prop <- sum((mc_prop - est_prop)^2) / (M * (M-1))
est_prop
est_prop - qnorm(0.975) * sqrt(samp_var_prop)
est_prop + qnorm(0.975) * sqrt(samp_var_prop)
2 * qnorm(0.975) * sqrt(samp_var_prop)

run_mc <- function(mc_func) {
  mc <- sapply(seq(1, M), FUN = function(x) mc_func())
  Reduce('+', mc) / M
}

N <- 1000

mc_impr <- sapply(seq(1, N), FUN = function(x) run_mc(sim_coverage_improper))
mc_prop <- sapply(seq(1, N), FUN = function(x) run_mc(sim_coverage_proper))

# Improper prior
quantile(mc_impr, prob = c(0.025, 0.5, 0.975))

# Proper prior
quantile(mc_prop, prob = c(0.025, 0.5, 0.975))
