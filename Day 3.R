# ggplot
# Mapping on Day 3
# 30 Jan 2020
# Kyle Albertyn
######################

# Load Libraries

library(tidyverse)
library(boot)

# Load data

urine <- boot::urine

# Create a quick scatterplot 

ggplot(data = urine, aes(x = osmo, y = ph))+
  geom_point(aes(colour = cond))


# Mapping in R

# Load in Packages

library(tidyverse)
library(ggpubr)

# Load data

load("data/south_africa_coast.RData")
load("data/sa_provinces.RData")
load("data/rast_annual.RData")
load("data/MUR.RData")
#load("data/MUR_low_res.RData")

# Choose which SST product you would like to use

sst <- MUR

# The colour pallette we will use for ocean temperature

cols11 <- c("#004dcd", "#0068db", "#007ddb", "#008dcf", "#009bbc",
            "#00a7a9", "#1bb298", "#6cba8f", "#9ac290", "#bec99a")














