# Day 1
# Data, Laminaria, statistical analyses
# 28 January 2020

# Loading packages && csv files

library(tidyverse)

laminaria <- read_delim("data/laminaria.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)


head(laminaria) # First 6 Rows
tail(laminaria) # Last 6 Rows
glimpse(laminaria) #Overview of Data Set
view(laminaria) # Opens Dataset
names(laminaria) # Shows names of dataset

lam_sub <- laminaria %>% # Tell R which dataframe we are using
  select(site, total_length) # Select only specific columns

lam_slice <- laminaria %>% # Tell R which dataframe we are using
  select(site, total_length) %>%  # Select only specific columns
  slice(56:78)

lam_kommetjie <- laminaria %>% 
  filter(site == "Kommetjie")

laminaria %>%
  filter(site == "Kommetjie") %>%
  nrow()


