# =============================================================================
# File Name:     hw03.R
# Author:        Evan Pete Walsh
# Contact:       epwalsh10@gmail.com
# Creation Date: 2016-09-18
# Last Modified: 2016-09-21 21:30:30
# =============================================================================

source("glm.R")
source("regmeanvar.R")


# Assignment 3.1

d <- read.csv("../data/HW03.csv")

xmat <- cbind(rep(1, nrow(d)), d$bm)
y <- d$oxy

j <- regmeanvar(xmat[,2], y, 5)

plot(log(j$mean), log(sqrt(j$vars)), 
  xlab="Log Group Mean", 
  ylab="Log Group Std. Deviation")
q <- matrix(c(rep(1,5), log(j$mean)), 5, 2, byrow=F)
solve(t(q)%*%q)%*%t(q)%*%log(sqrt(j$vars))
abline(-0.122,0.988)

# Suggests gamma random component

plot(xmat[,2], log(y),
  xlab="Body mass (g)",
  ylab="Log OXY concentration")


res <- basicglm(xmat, y, 2, 5)

res$estb[1,1] - qnorm(0.975) * sqrt(res$invinf[1,1])
res$estb[1,1] + qnorm(0.975) * sqrt(res$invinf[1,1])

res$estb[2,1] - qnorm(0.975) * sqrt(res$invinf[2,2])
res$estb[2,1] + qnorm(0.975) * sqrt(res$invinf[2,2])


x <- seq(0, 2100, by=5)
mu <- exp(res$estb[1,1] + x * res$estb[2,1])

plot(xmat[,2], y,
  xlab="Body mass (g)",
  ylab="OXY concentration")
curve(exp(res$estb[1,1] + x * res$estb[2,1]), from=0, to=2100, add=T)


# Assignment 3.2:

xmat <- matrix(c(rep(1, 50), 1:50), 50, 2, byrow=F)

b <- c(-5, 0.14)
phi <- 1

dat <- simbasicglm(b = b, xmat = xmat, phi = phi, link = 5, random=3)


res <- basicglm(xmat, dat, 5, 3)

res$estb[1,1] - qnorm(0.975) * sqrt(res$invinf[1,1])
res$estb[1,1] + qnorm(0.975) * sqrt(res$invinf[1,1])

res$estb[2,1] - qnorm(0.975) * sqrt(res$invinf[2,2])
res$estb[2,1] + qnorm(0.975) * sqrt(res$invinf[2,2])


x <- seq(0, 50, by=1)

estmean <- function(x, res) {
  return(1 - exp(-exp(res$estb[1,1] + x * res$estb[2,1])))
}

mu_der <- function(x, b) {
  d1 <- exp(-exp(b[1] + x * b[2])) * exp(b[1] + x * b[2])
  d2 <- exp(-exp(b[1] + x * b[2])) * exp(b[1] + x * b[2]) * x
  return(t(c(d1, d2)))
}

estvar <- function(x, b, fish) {
  der <- mu_der(x, b)
  return(as.numeric(der %*% fish %*% t(der)))
}

lowerbnd <- function(x, res) {
  return(estmean(x, res) - qnorm(0.975) * sqrt(estvar(x, res$estb[,1], res$invinf)))
}

upperbnd <- function(x, res) {
  return(estmean(x, res) + qnorm(0.975) * sqrt(estvar(x, res$estb[,1], res$invinf)))
}

estvar(30, res$estb[,1], res$invinf)

plot(xmat[,2], dat,
  xlab="Covariate", ylab="Simulated response")
curve(estmean(x, res), from=0, to=50, add=T)
curve(sapply(x, FUN = function(x) lowerbnd(x, res)), from=0, to=50, add=T, lty=2)
curve(sapply(x, FUN = function(x) upperbnd(x, res)), from=0, to=50, add=T, lty=2)

x <- seq(0, 50, by=1)

l <- sapply(x, FUN = function(x) lowerbnd(x, res))

lowerbnd(x, res)
