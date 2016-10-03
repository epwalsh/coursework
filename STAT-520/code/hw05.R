# =============================================================================
# File Name:     hw05.R
# Author:        Evan Pete Walsh
# Contact:       epwalsh10@gmail.com
# Creation Date: 2016-10-03
# Last Modified: 2016-10-03 15:53:58
# =============================================================================

source("./nonlin.R")
library(ggplot2)

df <- read.csv("../data/HW05.csv")
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
