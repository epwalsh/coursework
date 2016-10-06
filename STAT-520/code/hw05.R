# =============================================================================
# File Name:     hw05.R
# Author:        Evan Pete Walsh
# Contact:       epwalsh10@gmail.com
# Creation Date: 2016-10-03
# Last Modified: 2016-10-06 18:29:12
# =============================================================================

source("./nonlin.R")
library(ggplot2)

df <- read.csv("../data/HW05.csv")

# Question 2 {{{
ggplot(aes(x=x, y=y), data=df) + geom_point() + theme_bw()

ps <- c(20, 3, 0.5)
res <- nonlin(df$x, df$y, ps, gompfctn, gompVmat, gompwts)

covb <- res$covb
b <- res$bs
b[1] - qnorm(0.975) * sqrt(covb[1,1])
b[1] + qnorm(0.975) * sqrt(covb[1,1])
b[2] - qnorm(0.975) * sqrt(covb[2,2])
b[2] + qnorm(0.975) * sqrt(covb[2,2])
b[3] - qnorm(0.975) * sqrt(covb[3,3])
b[3] + qnorm(0.975) * sqrt(covb[3,3])

d <- t(c(0, 1/b[3], -b[2]/(b[3]^2)))
s <- sqrt(d %*% covb %*% t(d))[1]
b[2]/b[3]
b[2]/b[3] - qnorm(0.975) * s
b[2]/b[3] + qnorm(0.975) * s

res$sshat
# }}}

# Question 2 {{{
loglikgomp <- function(dat, params) {
  # Negative of log likelihood
  x <- dat$x
  y <- dat$y
  n <- length(x)
  mu <- gompfctn(x, params[1:3])
  sig2 <- params[4]
  l1 <- n * 0.5 * log(2 * pi * sig2)
  l2 <- 0.5 * sum(log(mu))
  l3 <- sum(((y - mu)^2) / (2 * sig2 * mu))
  return(l1 + l2 + l3)
}

res2 <- optim(par=c(20, 3, 0.5, 0.2), fn=loglikgomp, dat=df, hessian=T)
invinf <- solve(res2$hessian)

res2$par[1] - qnorm(0.975) * sqrt(invinf[1,1])
res2$par[1] + qnorm(0.975) * sqrt(invinf[1,1])
res2$par[2] - qnorm(0.975) * sqrt(invinf[2,2])
res2$par[2] + qnorm(0.975) * sqrt(invinf[2,2])
res2$par[3] - qnorm(0.975) * sqrt(invinf[3,3])
res2$par[3] + qnorm(0.975) * sqrt(invinf[3,3])
res2$par[4] - qnorm(0.975) * sqrt(invinf[4,4])
res2$par[4] + qnorm(0.975) * sqrt(invinf[4,4])


d <- t(c(0, 1/res2$par[3], -res2$par[2]/(res2$par[3]^2), 0))
s <- sqrt(d %*% invinf %*% t(d))[1]
res2$par[2] / res2$par[3]
res2$par[2] / res2$par[3] - qnorm(0.975) * s
res2$par[2] / res2$par[3] + qnorm(0.975) * s
# }}}

x <- df$x

T2 <- function(b2, b3, x) {
  return(exp(b2 - b3 * x))
}

T1 <- function(b2, b3, x) {
  res <- exp(-T2(b2, b3, x))
  return(res)
}

y <- df$y
x <- df$x

b1 <- res2$par[1]
b2 <- res2$par[2]
b3 <- res2$par[3]
s2 <- res2$par[4]

b1 <- b[1]
b2 <- b[2]
b3 <- b[3]
s2 <- res$sshat

t1 <- T1(b2, b3, x)
t2 <- T2(b2, b3, x)

# dl/db1
sum((y^2) / (2 * s2 * t1 * b1^2) - t1 / (2 * s2) - 1 / (2 * b1))
# dl/db2
sum(t1 * t2 * b1 / (2 * s2) + t2 / 2 - (y^2) * t2 / (2 * s2 * t1 * b1))
# dl/db3
sum((y^2) * x * t2 / (2 * s2 * t1 * b1) - b1 * x * t1 * t2 / (2 * s2) - x * t2 / (2))
# dl/ds2
sum((y^2) / (2 * (s2^2) * b1 * t1) - y / ((s2^2)) + b1 * t1 / (2 * (s2)^2) - 1 / (2*s2))


# Question 6 {{{
loglikgomp2 <- function(dat, params) {
  # Negative of log likelihood
  x <- dat$x
  y <- dat$y
  n <- length(x)
  mu <- gompfctn(x, params[1:3])
  sig2 <- params[4]
  l1 <- n * 0.5 * log(2 * pi * sig2)
  l2 <- params[5] * sum(log(mu))
  l3 <- sum(((y - mu)^2) / (2 * sig2 * mu^(2 * params[5])))
  return(l1 + l2 + l3)
}

res3 <- optim(par=c(20, 3.25, 0.54, 0.2, 0.5), fn=loglikgomp2, dat=df, 
  hessian=T, control=list(maxit=1000))
res3

invinf <- solve(res3$hessian)

res3$par[1] - qnorm(0.975) * sqrt(invinf[1,1])
res3$par[1] + qnorm(0.975) * sqrt(invinf[1,1])
res3$par[2] - qnorm(0.975) * sqrt(invinf[2,2])
res3$par[2] + qnorm(0.975) * sqrt(invinf[2,2])
res3$par[3] - qnorm(0.975) * sqrt(invinf[3,3])
res3$par[3] + qnorm(0.975) * sqrt(invinf[3,3])
# }}}
