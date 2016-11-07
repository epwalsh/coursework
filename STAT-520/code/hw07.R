# =============================================================================
# File Name:     hw07.R
# Author:        Evan Pete Walsh
# Contact:       epwalsh10@gmail.com
# Creation Date: 2016-11-07
# Last Modified: 2016-11-07 11:15:27
# =============================================================================

df <- read.csv("../data/HW07.csv")
x <- df$x
y <- df$y

loglik <- function(betas) {
  b0 <- betas[1]
  b1 <- betas[2]
  l <- exp(b0 + b1 * x)
  return(-sum(y * (b0 + b1 * x) - l))
}

loglik(c(0.525,0.5))

optim(par=c(0,2), fn=loglik, hessian=T)


l <- exp(0.525 + 0.5 * x)
