# =============================================================================
# File Name:     hw07.R
# Author:        Evan Pete Walsh
# Contact:       epwalsh10@gmail.com
# Creation Date: 2016-11-07
# Last Modified: 2016-11-08 17:26:58
# =============================================================================

df <- read.csv("../data/HW07.csv")
x <- df$x
y <- df$y

log_y = log(y)

library(ggplot2)

qplot(x = x, y = log_y, geom="point")
