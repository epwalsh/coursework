# =============================================================================
# File Name:     hw04.R
# Author:        Evan Pete Walsh
# Contact:       epwalsh10@gmail.com
# Creation Date: 2016-09-24
# Last Modified: 2016-09-24 18:52:43
# =============================================================================

source("glm.R")
library(ggplot2)

dat <- read.csv("../data/HW04.csv")
dat$county <- as.factor(dat$county)

ggplot(aes(y=y, x=county, fill=county), data=dat) + geom_boxplot() + theme_bw()
ggplot(aes(y, colour=county, fill=county), data=dat) + geom_density(alpha=0.2) + 
  theme_bw() + xlim(0, 12)

# Part 1 {{{
xmat <- matrix(c(rep(1, 75), rep(1, 25), rep(0,75), rep(1, 25), rep(0, 25)),
  byrow=F, nrow=75)

y <- dat$y

mod <- basicglm(xmat, y, link=6, random=5)

df <- data.frame(y=y, x1=xmat[,2], x2=xmat[,3])
mod2 <- glm(y~x1 + x2, data=df, family=Gamma(link="inverse"))

invinf <- summary(mod2)$cov.scaled
mod2$coeff[1] - qnorm(0.975) * sqrt(invinf[1,1])
mod2$coeff[1] + qnorm(0.975) * sqrt(invinf[1,1])
mod2$coeff[2] - qnorm(0.975) * sqrt(invinf[2,2])
mod2$coeff[2] + qnorm(0.975) * sqrt(invinf[2,2])
mod2$coeff[3] - qnorm(0.975) * sqrt(invinf[3,3])
mod2$coeff[3] + qnorm(0.975) * sqrt(invinf[3,3])

# Estimates for shape parameters 
invinf <- mod$invinf
phi <- as.numeric(mod$ests[1])
b <- mod$estb

# Beta1
d1 <- matrix(c(phi, phi, 0), nrow=1)
b1 <- d1 %*% b
b1 # point estimate

v1 <- d1 %*% invinf %*% t(d1)
b1 - qnorm(0.975) * sqrt(v1)
b1 + qnorm(0.975) * sqrt(v1)

# Beta2
d2 <- matrix(c(phi, phi, phi), nrow=1)
b2 <- d2 %*% b
b2 # point estimate

v2 <- d2 %*% invinf %*% t(d2)
b2 - qnorm(0.975) * sqrt(v2)
b2 + qnorm(0.975) * sqrt(v2)

# Beta3
d3 <- matrix(c(phi, 0, 0), nrow=1)
b3 <- d3 %*% b
b3 # point estimate

v3 <- d3 %*% invinf %*% t(d3)
b3 - qnorm(0.975) * sqrt(v3)
b3 + qnorm(0.975) * sqrt(v3)

# Beta1 - Beta2
d <- matrix(c(0, phi, -phi), nrow=1)
d %*% b
v <- d %*% invinf %*% t(d)
d %*% b - qnorm(0.975) * sqrt(v)
d %*% b + qnorm(0.975) * sqrt(v)

# Beta1 - Beta3
d <- matrix(c(0, phi, 0), nrow=1)
d %*% b
v <- d %*% invinf %*% t(d)
d %*% b - qnorm(0.975) * sqrt(v)
d %*% b + qnorm(0.975) * sqrt(v)

# Beta2 - Beta3
d <- matrix(c(0, 0, phi), nrow=1)
d %*% b
v <- d %*% invinf %*% t(d)
d %*% b - qnorm(0.975) * sqrt(v)
d %*% b + qnorm(0.975) * sqrt(v)

Beta <- mean(c(b1, b2, b3))

x <- seq(0, 12, by=0.1)
y <- dgamma(x, shape=phi, rate=Beta)
df2 <- data.frame(x=x, density=y)

ggplot(aes(x=x, y=density), data=df2) + geom_line() + theme_bw()
# }}}

# Part 2 {{{
source("./newtraph.R")

county1 <- dat[dat$county == 1,2]
county2 <- dat[dat$county == 2,2]
county3 <- dat[dat$county == 3,2]

county1_res <- newtraph(derloglike, county1, c(2,0.5))
county2_res <- newtraph(derloglike, county2, c(2,0.5))
county3_res <- newtraph(derloglike, county3, c(2,0.5))

county1_res[[1]]
county2_res[[1]]
county3_res[[1]]
# }}}
