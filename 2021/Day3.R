#Well this is an atrocity

library(here)
library(readr)
library(tidyverse)

#Part 1
dat.orig <- read_table(here("2021","Day3Part1.txt"),col_names = F)
dat <- dat.orig
dat <- dat %>% 
  mutate(X1 = as.character(X1)) %>% 
  separate(X1, sep = "", into = c("Z","N2","N3","N4","N5","N6","N7","N8",
                        "N9","N10","N11","N12","N13")) %>% 
  select(-Z)

dat %>% group_by(N2) %>% tally()
dat %>% group_by(N3) %>% tally()
dat %>% group_by(N4) %>% tally()
dat %>% group_by(N5) %>% tally()
dat %>% group_by(N6) %>% tally()
dat %>% group_by(N7) %>% tally()
dat %>% group_by(N8) %>% tally()
dat %>% group_by(N9) %>% tally()
dat %>% group_by(N10) %>% tally()
dat %>% group_by(N11) %>% tally()
dat %>% group_by(N12) %>% tally()
dat %>% group_by(N13) %>% tally()

x = 010100010100
x = 1300
y = 101011101011
y = 2795
final = x*y
final

#Part 2
ox <- dat %>% 
  group_by(N2) %>% 
  mutate(N2a=n()) %>%
  ungroup() %>% 
  filter(N2a==max(N2a)) %>% 
  group_by(N3) %>% 
  mutate(N3a=n()) %>%
  ungroup() %>% 
  filter(N3a==max(N3a)) %>% 
  group_by(N4) %>% 
  mutate(N4a=n()) %>%
  ungroup() %>% 
  filter(N4a==max(N4a)) %>% 
  group_by(N5) %>% 
  mutate(N5a=n()) %>%
  ungroup() %>% 
  filter(N5a==max(N5a)) %>% 
  group_by(N6) %>% 
  mutate(N6a=n()) %>%
  ungroup() %>% 
  filter(N6a==max(N6a)) %>% 
  group_by(N7) %>% 
  mutate(N7a=n()) %>%
  ungroup() %>% 
  filter(N7a==max(N7a)) %>% 
  group_by(N8) %>% 
  mutate(N8a=n()) %>%
  ungroup() %>% 
  filter(N8=="1") %>% 
  group_by(N9) %>% 
  mutate(N9a=n()) %>%
  ungroup() %>% 
  filter(N9a==max(N9a)) %>% 
  group_by(N10) %>% 
  mutate(N10a=n()) %>%
  ungroup() %>% 
  filter(N10=="1") %>% 
  group_by(N11) %>% 
  mutate(N11a=n()) %>%
  ungroup() %>% 
  filter(N11a==max(N11a)) %>% 
  group_by(N12) %>% 
  mutate(N12a=n()) %>%
  ungroup() %>% 
  filter(N12a==max(N12a)) %>% 
  group_by(N13) %>% 
  mutate(N13a=n()) %>%
  ungroup() %>% 
  filter(N13a==max(N13a))

x = 010100101111
x = 1327

co2 <- dat %>% 
  group_by(N2) %>% 
  mutate(N2a=n()) %>%
  ungroup() %>% 
  filter(N2a==min(N2a)) %>% 
  group_by(N3) %>% 
  mutate(N3a=n()) %>%
  ungroup() %>% 
  filter(N3a==min(N3a)) %>% 
  group_by(N4) %>% 
  mutate(N4a=n()) %>%
  ungroup() %>% 
  filter(N4a==min(N4a)) %>% 
  group_by(N5) %>% 
  mutate(N5a=n()) %>%
  ungroup() %>% 
  filter(N5a==min(N5a)) %>% 
  group_by(N6) %>% 
  mutate(N6a=n()) %>%
  ungroup() %>% 
  filter(N6a==min(N6a)) %>% 
  group_by(N7) %>% 
  mutate(N7a=n()) %>%
  ungroup() %>% 
  filter(N7a==min(N7a)) %>% 
  group_by(N8) %>% 
  mutate(N8a=n()) %>%
  ungroup() %>% 
  filter(N8a==min(N8a)) %>% 
  group_by(N9) %>% 
  mutate(N9a=n()) %>%
  ungroup() %>% 
  filter(N9==0) %>% 
  group_by(N10) %>% 
  mutate(N10a=n()) %>%
  ungroup() %>% 
  filter(N10==0) %>% 
  group_by(N11) %>% 
  mutate(N11a=n()) %>%
  ungroup() %>% 
  filter(N11a==min(N11a)) %>% 
  group_by(N12) %>% 
  mutate(N12a=n()) %>%
  ungroup() %>% 
  filter(N12a==min(N12a)) %>% 
  group_by(N13) %>% 
  mutate(N13a=n()) %>%
  ungroup() %>% 
  filter(N13a==min(N13a))

y = 110101110011
y = 3429
final = x*y
final
