#This one I'm pretty happy with

library(here)
library(readr)
library(tidyverse)

#Part 1
nums <- as.numeric(as.character(read_delim(here("2021","Day4BingoNumbers.txt"),col_names = F)))
boards <- read_table(here("2021","Day4BingoBoards.txt"),col_names = F)
boards <- split(boards,rep(1:100,each=5))

# nums <- as.numeric(as.character(read_delim(here("2021","Day4BingoNumbersCheck.txt"),col_names = F)))
# boards <- read_table(here("2021","Day4BingoBoardsCheck.txt"),col_names = F)
# boards <- split(boards,rep(1:3,each=5))

stop = F
for (n in nums) {
  for (i in 1:100) {
    if (any(n == boards[[i]], na.rm = T)) {
      location <- which(boards[[i]] == n, arr.ind=TRUE)
      boards[[i]][location[1],location[2]] <- NA
      colCheck <- colSums(boards[[i]], na.rm = T)
      rowCheck <- rowSums(boards[[i]], na.rm = T)
      if (0 %in% colCheck | 0 %in% rowCheck) {
        final <- sum(colCheck)
        print(n)
        stop = T
        break
      }
    }
    if (stop){break}
  }
  if (stop){break}
}

ans <- final*n
ans

#Part 2
boards <- read_table(here("2021","Day4BingoBoards.txt"),col_names = F)
boards <- split(boards,rep(1:100,each=5))

stop = F

notDone <- 1:100

for (n in nums) {
  for (i in 1:100) {
    if (i %in% notDone) {
      if (any(n == boards[[i]], na.rm = T)) {
        location <- which(boards[[i]] == n, arr.ind=TRUE)
        boards[[i]][location[1],location[2]] <- NA
        colCheck <- colSums(boards[[i]], na.rm = T)
        rowCheck <- rowSums(boards[[i]], na.rm = T)
        if (0 %in% colCheck | 0 %in% rowCheck) {
          final <- sum(colCheck)
          notDone[notDone == i] <- NA
          if (sum(is.na(notDone)) == 100) {
            print(n)
            stop = T
            break
          }
        }
      }
    }
    if (stop){break}
  }
  if (stop){break}
}

ans <- final*n
ans
