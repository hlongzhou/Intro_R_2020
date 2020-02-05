# Kyle Albertyn       #
# Final Assessment    #
# 5th Feb 2020        #
#######################





# loading packages

library(tidyverse)
library(ggplot2)
library(raster)
library(SDMTools)


# Load in data

gebco_wide <- read.asc("data\\gebco_sa.asc") # read in as an ascii file

gebco_wide <- raster(gebco_wide) # convert to raster


# Convert to Spatial Pixel Data Frame

spdf <- as(gebco_wide, "SpatialPixelsDataFrame")


# Set as a df

gebco_tidy <- as.data.frame(spdf)


# Creating the graph
graph <- ggplot(data = gebco_tidy, aes(x = x, y = y))+ # set x and y, activating ggplot
  geom_raster(aes(x = x, y = y, fill = layer), show.legend = TRUE)+ #set to use raster
  scale_fill_gradientn("Elevation/ \nDepth (m)", values = scales::rescale(c(-6129, 0, 1, 3374)) , colours = c("blue", "darkcyan", "cyan", "white",  "darkgreen","green",  "khaki")) + # set legend title, scale and colours
  labs(x = "", y = "")+ # set labels to nothing
  scale_x_continuous(breaks = seq(10, 35, 5),
                     labels = c("10", "15", "20", "25", "30", "35"),
                     position = "bottom", expand = c(0, 0)) + # set scale numbers accordingly
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) # remove the theme lines

# display the final graph

graph
