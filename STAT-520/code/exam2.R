# =============================================================================
# File Name:     exam2.R
# Author:        Evan Pete Walsh
# Contact:       epwalsh10@gmail.com
# Creation Date: 2016-11-12
# Last Modified: 2016-11-12 15:58:46
# =============================================================================

d <- read.csv("../data/tornados.csv")
d <- d[d$State != 'TX',]
y <- d$Tornados
k <- d$Area / 1000
n <- length(y)

a0 <- 2.5
b0 <- 0.5

# Question 1 {{{
# posterior a
a <- a0 + sum(y)
b <- b0 + length(y)

a / b
qgamma(0.05, a, b)
qgamma(0.95, a, b)

# posterior predictive assessment
M <- 10000

Q1 <- sapply(1:M, function(x) {
  l <- rgamma(1, a, b)
  newy <- rpois(n, l)
  return(sum(newy >= 50))
})


Q2 <- sapply(1:M, function(x) {
  l <- rgamma(1, a, b)
  newy <- rpois(n, l)
  return(diff(range(newy)))
})

sum(Q1 > sum(y >= 50)) / M
sum(Q2 > diff(range(y))) / M

# }}}

# Question 2 {{{
a <- a0 + sum(y)
b <- b0 + sum(k)
a
b
a/b
qgamma(0.05, a, b)
qgamma(0.95, a, b)

Q1 <- sapply(1:M, function(x) {
  r <- rgamma(1, a, b)
  newy <- sapply(k, function(y) rpois(1, y * r))
  return(sum(newy >= 50))
})

Q2 <- sapply(1:M, function(x) {
  r <- rgamma(1, a, b)
  newy <- sapply(k, function(y) rpois(1, y * r))
  return(diff(range(newy)))
})

sum(Q1 > sum(y >= 50)) / M
sum(Q2 > diff(range(y))) / M
# }}}

# Question 3 {{{
d1 <- d[d$State %in% c("SD", "NE", "KS", "OK", "AR", "AL", "MS", "LA"),]
d2 <- d[!(d$State %in% c("SD", "NE", "KS", "OK", "AR", "AL", "MS", "LA")),]

y1 <- d1$Tornados
y2 <- d2$Tornados
k1 <- d1$Area / 1000
k2 <- d2$Area / 1000

a0 = 0.1
b0 = 0.1

a1 <- a0 + sum(y1)
b1 <- b0 + sum(k1)
a1
b1
a1/b1
qgamma(0.05, a1, b1)
qgamma(0.95, a1, b1)

a2 <- a0 + sum(y2)
b2 <- b0 + sum(k2)
a2
b2
a2/b2
qgamma(0.05, a2, b2)
qgamma(0.95, a2, b2)

Q1 <- sapply(1:M, function(x) {
  r <- rgamma(1, a1, b1)
  newy <- sapply(k1, function(z) rpois(1, z * r))
  return(sum(newy >= 50))
})

Q2 <- sapply(1:M, function(x) {
  r <- rgamma(1, a1, b1)
  newy <- sapply(k1, function(z) rpois(1, z * r))
  return(diff(range(newy)))
})

sum(Q1 > sum(y1 >= 50)) / M
sum(Q2 > diff(range(y1))) / M

Q1 <- sapply(1:M, function(x) {
  r <- rgamma(1, a2, b2)
  newy <- sapply(k2, function(z) rpois(1, z * r))
  return(sum(newy >= 50))
})

Q2 <- sapply(1:M, function(x) {
  r <- rgamma(1, a2, b2)
  newy <- sapply(k2, function(z) rpois(1, z * r))
  return(diff(range(newy)))
})

sum(Q1 > sum(y2 >= 50)) / M
sum(Q2 > diff(range(y2))) / M

# Monte-Carlo evaluation of integral
M <- 10000
r <- rgamma(M, a1, b1)
mean(pgamma(r, shape=a2, rate=b2))


library(ggplot2)

x <- seq(0, 0.5, by=0.001)
f1 <- dgamma(x, a1, b1)
f2 <- dgamma(x, a2, b2)
df = data.frame(x=rep(x, 2), p=c(f1, f2), group=factor(c(rep(1, length(x)), rep(2, length(x)))))
ggplot(data=df, aes(x=x, y=p, colour=group, linetype=group)) + geom_line() + theme_bw()
# }}}
