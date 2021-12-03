library(here)
library(readr)
library(tidyverse)

#Part 1
dat.orig <- read_table(here("2021","Day2Part1.txt"),col_names = F)
dat <- dat.orig
x <- sum(if_else(dat$X1 == "forward", dat$X2, 0))
y <- sum(if_else(dat$X1 == "down", -dat$X2, if_else(dat$X1 == "up", dat$X2, 0)))
final <- x*y
abs(final)

#Part 2
#We can still use dat
dat$aim <- cumsum(if_else(dat$X1 == "down", -dat$X2, if_else(dat$X1 == "up", dat$X2, 0)))
dat$value = dat$X2*dat$aim
forward <- dat %>% filter(X1 == "forward")
y = sum(abs(forward$value))
final <- x*y
final
