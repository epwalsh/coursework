# =============================================================================
# File Name:     hw03.R
# Author:        Evan Pete Walsh
# Contact:       epwalsh10@gmail.com
# Creation Date: 2016-09-18
# Last Modified: 2016-09-18 15:02:17
# =============================================================================

source("glm.R")





# Assignment 3.2:

xmat <- matrix(c(rep(1, 50), 1:50), 50, 2, byrow=F)

b <- c(5, 0.27)
phi <- 1

dat <- simbasicglm(b = b, xmat = xmat, phi = phi, link = 5, random=3)

etas <- xmat %*% b
link.info(etas, 5, 1)
