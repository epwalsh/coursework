# =============================================================================
# File Name:     hw03.R
# Author:        Evan Pete Walsh
# Contact:       epwalsh10@gmail.com
# Creation Date: 2016-09-18
# Last Modified: 2016-09-23 15:14:02
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

plot(xmat[,2], sqrt(y),
  xlab="Body mass (g)",
  ylab="Square root of OXY concentration")

plot(xmat[,2], y^(1/3),
  xlab="Body mass (g)",
  ylab="OXY concentration to the power of 1/3")


res <- basicglm(xmat, y, 2, 5)
res2 <- basicglm(xmat, y, 8, 5, pwr=.5)
res3 <- basicglm(xmat, y, 8, 5, pwr=0.33)

res$estb[1,1] - qnorm(0.975) * sqrt(res$invinf[1,1])
res$estb[1,1] + qnorm(0.975) * sqrt(res$invinf[1,1])
res$estb[2,1] - qnorm(0.975) * sqrt(res$invinf[2,2])
res$estb[2,1] + qnorm(0.975) * sqrt(res$invinf[2,2])

res2$estb[1,1] - qnorm(0.975) * sqrt(res2$invinf[1,1])
res2$estb[1,1] + qnorm(0.975) * sqrt(res2$invinf[1,1])
res2$estb[2,1] - qnorm(0.975) * sqrt(res2$invinf[2,2])
res2$estb[2,1] + qnorm(0.975) * sqrt(res2$invinf[2,2])

res3$estb[1,1] - qnorm(0.975) * sqrt(res3$invinf[1,1])
res3$estb[1,1] + qnorm(0.975) * sqrt(res3$invinf[1,1])
res3$estb[2,1] - qnorm(0.975) * sqrt(res3$invinf[2,2])
res3$estb[2,1] + qnorm(0.975) * sqrt(res3$invinf[2,2])


x <- seq(0, 2100, by=5)
mu <- exp(res$estb[1,1] + x * res$estb[2,1])

plot(xmat[,2], y,
  xlab="Body mass (g)",
  ylab="OXY concentration")
curve(exp(res$estb[1,1] + x * res$estb[2,1]), from=0, to=2100, add=T)
curve((res2$estb[1,1] + x * res2$estb[2,1])^2, from=0, to=2100, add=T, lty=2)
curve((res3$estb[1,1] + x * res3$estb[2,1])^3, from=0, to=2100, add=T, lty=3)


# Assignment 3.2:

xmat <- matrix(c(rep(1, 50), 1:50), 50, 2, byrow=F)

b <- c(-5, 0.14)
phi <- 1

dat <- simbasicglm(b = b, xmat = xmat, phi = phi, link = 5, random=3)
res <- basicglm(xmat, dat, 5, 3, startb=b)

res$estb[1,1] - qnorm(0.975) * sqrt(res$invinf[1,1])
res$estb[1,1] + qnorm(0.975) * sqrt(res$invinf[1,1])

res$estb[2,1] - qnorm(0.975) * sqrt(res$invinf[2,2])
res$estb[2,1] + qnorm(0.975) * sqrt(res$invinf[2,2])



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


plot(xmat[,2], dat,
  xlab="Covariate", ylab="Simulated response")
curve(estmean(x, res), from=0, to=50, add=T)
curve(sapply(x, FUN = function(x) lowerbnd(x, res)), from=0, to=50, add=T, lty=2)
curve(sapply(x, FUN = function(x) upperbnd(x, res)), from=0, to=50, add=T, lty=2)



dat <- simbasicglm(b = b, xmat = xmat, phi = phi, link = 5, random=3)
d <- data.frame(y = dat, x = xmat[,2])
res <- glm(y ~ x, data=d, family=binomial(link = cloglog))
summary(res)
res2 <- basicglm(xmat, dat, 5, 3)


estmean <- function(x, res) {
  return(1 - exp(-exp(res$coeff[1] + x * res$coeff[2])))
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
  return(estmean(x, res) - qnorm(0.975) * sqrt(estvar(x, res$coeff, vcov(res))))
}

upperbnd <- function(x, res) {
  return(estmean(x, res) + qnorm(0.975) * sqrt(estvar(x, res$coeff, vcov(res))))
}


plot(xmat[,2], dat,
  xlab="Covariate", ylab="Simulated response")
curve(estmean(x, res), from=0, to=50, add=T)
curve(sapply(x, FUN = function(x) lowerbnd(x, res)), from=0, to=50, add=T, lty=2)
curve(sapply(x, FUN = function(x) upperbnd(x, res)), from=0, to=50, add=T, lty=2)
