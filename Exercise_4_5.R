# Author: Kyle Albertyn
# Exercise 4.5
# 28 January 2020

# Import packages and csv files

library(tidyverse)
library(lubridate)
library(ggplot2)
laminaria <- read_delim("R Course/Intro_R_2020/data/laminaria.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

# sort laminaria
lam_sort <- laminaria %>% 
  select(site, total_length)

# divide total length by 2
total_length_half <- lam_sort %>% 
  mutate(total_length_half = total_length/2)

# sort by lengths <100

length_final <- total_length_half[(total_length_half[,3]<100),]


#sort different sites along with their average, minimum and maximum whilst assigning the respective variables

buffels <- length_final %>% 
  filter(site == "Buffels South") %>% 
  summarise(average_length = mean(total_length_half))

  buffels_avg <- buffels
  
buffels <- length_final %>% 
  filter(site == "Buffels South") %>%
  summarise(min_length = min(total_length_half))

  buffels_min <- buffels


buffels <- length_final %>% 
  filter(site == "Buffels South") %>%
  summarise(max_length = max(total_length_half))

  buffels_max <- buffels



millers <- length_final %>% 
  filter(site == "Miller's Point") %>% 
  summarise(average_length = mean(total_length_half))

millers_avg <- millers


millers <- length_final %>% 
  filter(site == "Miller's Point") %>% 
  summarise(min_length = min(total_length_half))

millers_min <- millers


millers <- length_final %>% 
  filter(site == "Miller's Point") %>% 
  summarise(max_length = max(total_length_half))

millers_max <- millers


baboon <- length_final %>% 
  filter(site == "Baboon Rock") %>% 
  summarise(average_length = mean(total_length_half))

baboon_avg <- baboon


baboon <- length_final %>% 
  filter(site == "Baboon Rock") %>% 
  summarise(min_length = min(total_length_half))


baboon_min <- baboon


baboon <- length_final %>% 
  filter(site == "Baboon Rock") %>% 
  summarise(max_length = max(total_length_half))


baboon_max <- baboon

roman <- length_final %>% 
  filter(site == "Roman Rock") %>% 
  summarise(average_length = mean(total_length_half))

roman_avg <- roman


roman <- length_final %>% 
  filter(site == "Roman Rock") %>% 
  summarise(min_length = min(total_length_half))

roman_min <- roman


roman <- length_final %>% 
  filter(site == "Roman Rock") %>% 
  summarise(max_length = max(total_length_half))

roman_max <- roman


frame <- length_final %>% 
  filter(site == "A-Frame") %>% 
  summarise(average_length = mean(total_length_half))

frame_avg <- frame


frame <- length_final %>% 
  filter(site == "A-Frame") %>% 
  summarise(min_length = min(total_length_half))


frame_min <- frame



frame <- length_final %>% 
  filter(site == "A-Frame") %>% 
  summarise(max_length = max(total_length_half))

frame_max <- frame



batsata <- length_final %>% 
  filter(site == "Batsata Rock") %>% 
  summarise(average_length = mean(total_length_half))

batsata_avg <- batsata


batsata <- length_final %>% 
  filter(site == "Batsata Rock") %>% 
  summarise(min_length = min(total_length_half))

batsata_min <- batsata


batsata <- length_final %>% 
  filter(site == "Batsata Rock") %>% 
  summarise(max_length = max(total_length_half))

batsata_max <- batsata













