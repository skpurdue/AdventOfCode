#Yikes!

library(here)
library(readr)
library(tidyverse)

#Part 1
fish <- as.numeric(as.character(read_csv(here("2021","Day6Part1.txt"),col_names = F)))

makeFish <- function(vec) {
  temp <- vec - 1
  if (-1 %in% temp) {
    index <- which(temp == -1, arr.ind=TRUE)
    temp <- replace(temp, index, 6)
    temp <- c(temp, rep(8, each = length(index)))
  }
  return(temp)
}

i = 1
while (i < 81) {
  if (i == 1) {
    numFish <- sapply(fish, makeFish)
  }
  else {
    numFish = unlist(sapply(numFish, makeFish))
  }
  i <- i + 1
}

#Part 2
#Can't do this with the apply functions

fishtable <- as.data.frame(table(fish)) %>% 
  mutate(fish = as.numeric(fish)) %>% 
  add_row(fish = 6, Freq = 0) %>% 
  add_row(fish = 7, Freq = 0) %>% 
  add_row(fish = 8, Freq = 0) %>% 
  add_row(fish = 0, Freq = 0) %>% 
  arrange(fish)

makeFish2 <- function(df) {
  temp <- df %>% 
    mutate(fish = fish - 1,
           fish = replace(fish, fish == -1, 8)) %>% 
    arrange(fish)
  temp$Freq[temp$fish == 6] <- temp$Freq[temp$fish == 6] + temp$Freq[temp$fish == 8]
  return(temp)
}

i = 1
while (i < 257) {
  if (i == 1) {
    fishtable2 <- makeFish2(fishtable)
  }
  else {
    fishtable2 <- makeFish2(fishtable2)
  }
  i <- i + 1
}

format(sum(fishtable2$Freq),scientific = F)
