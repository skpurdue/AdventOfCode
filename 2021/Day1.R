library(here)
library(readr)
library(tidyverse)

#Part 1
dat.orig <- read_table(here("2021","Day1Part1.txt"),col_names = F)
dat <- dat.orig
dat$X2 <- c(NA, dat$X1[1:nrow(dat)-1])
dat <- dat %>% 
  mutate(value = if_else(X1 - X2 > 0, 1, 0))
sum(dat$value, na.rm = T)

#Part 2
dat2 <- dat.orig
dat2$X2 <- c(dat2$X1[2:nrow(dat2)], NA)
dat2$X3 <- c(dat2$X1[3:nrow(dat2)], NA, NA)
dat2$sum <- dat2$X1 + dat2$X2 + dat2$X3
dat2$sumShift <-  c(NA, dat2$sum[1:nrow(dat2)-1])
dat2 <- dat2 %>% 
  mutate(value = if_else(sum - sumShift > 0, 1, 0))
sum(dat2$value, na.rm = T)
