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



ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_point()




ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "red", aes(group = group)) # The land mask


ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) # The province borders



ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) + 
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) # Force lon/lat extent


ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) + # The ocean temperatures
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)






final_map <- ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins), 
            colour = "white", size = 0.1) +
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
  scale_x_continuous(position = "top") + # Put x axis labels on top of figure
  theme(axis.title = element_blank(), # Remove the axis labels
        legend.text = element_text(size = 7), # Change text size in legend
        legend.title = element_text(size = 7), # Change legend title text size
        legend.key.height = unit(0.3, "cm"), # Change size of legend
        legend.background = element_rect(colour = "white"), # Add legend background
        legend.justification = c(1, 0), # Change position of legend
        legend.position = c(0.55, 0.4) # Fine tune position of legend
  )
final_map



## Chapter 10


# Load libraries
library(tidyverse)
library(scales)
library(ggsn)
library(maps)


# Load Africa map
load("data/africa_map.RData")


ggplot() +
  borders() + # The global shape file
  coord_equal() # Equal sizing for lon/lat 

sa_1 <- ggplot() +
  borders(fill = "grey70", colour = "black") +
  coord_equal(xlim = c(12, 36), ylim = c(-38, -22), expand = 0) # Force lon/lat extent
sa_1


sa_2 <- sa_1 +
  annotate("text", label = "Atlantic\nOcean", 
           x = 15.1, y = -32.0, 
           size = 5.0, 
           angle = 30, 
           colour = "navy") +
  annotate("text", label = "Indian\nOcean", 
           x = 33.2, y = -34.2, 
           size = 5.0, 
           angle = 330, 
           colour = "springgreen")
sa_2

sa_3 <- sa_2 +
   scalebar(x.min = 22, x.max = 26, y.min = -36, y.max = -35, # Set location of bar
            dist = 200, height = 1, st.dist = 0.8, st.size = 4, # Set particulars
            transform = TRUE, model = "WGS84") + # Set appearance
  north(x.min = 22.5, x.max = 25.5, y.min = -33, y.max = -31, # Set location of symbol
        scale = 1.2, symbol = 16)
sa_3


sa_4 <- sa_3 +
  annotation_custom(grob = ggplotGrob(africa_map),
                    xmin = 20.9, xmax = 26.9,
                    ymin = -30, ymax = -24)
sa_4

sa_final <- sa_4 +
  scale_x_continuous(breaks = seq(16, 32, 4),
                     labels = c("16°E", "20°E", "24°E", "28°E", "32°E"),
                     position = "bottom") +
  scale_y_continuous(breaks = seq(-36, -24, 4),
                     labels = c("36.0°S", "32.0°S", "28.0°S", "24.0°S"),
                     position = "right") +
  labs(x = "", y = "")
sa_final

ggsave(plot = sa_final, filename = "data/southern_africa_final.pdf", 
       height = 6, width = 8)




###################################################################################

# Homework
# Exercise 8.5
# Kyle Albertyn


# Load Data

eck <- ecklonia <- read_csv("data/ecklonia.csv")

# Load Packages

library(tidyverse)
library(ggplot2)


# Create the graphs (same code is repeated for each graph, only changing the variables and variable labels)

graph_SL <- ggplot(data = eck, aes(x = ID, y = stipe_length))+ # assign graph to variable and set data and axis data
  geom_point(aes(colour = site))+# set as point/scatter graph
  geom_smooth(method = "gam") + # Create linear model
  labs(x = "Identification", y = "Stipe Length")+ # Set axis labels
  facet_wrap(~site, ncol = 2)+ # Iniciate facet wrap
  theme_bw() # add theme


graph_SL # display graph

graph_SM <- ggplot(data = eck, aes(x = ID, y = stipe_mass))+
  geom_point(aes(colour = site))+
  geom_smooth(method = "gam") +
  labs(x = "Identification", y = "Stipe Mass")+
  facet_wrap(~site, ncol = 2)+
  theme_bw()

graph_SM




# HISTO############IGNORE


graph_yikes <- ggplot(data = eck, aes(x = stipe_mass))+
  geom_histogram(aes(colour = site, fill = site))+
  theme_bw()

graph_yikes

######################IGNORE





graph_FL <- ggplot(data = eck, aes(x = ID, y = frond_length))+
  geom_point(aes(colour = site))+
  geom_smooth(method = "gam") +
  labs(x = "Identification", y = "Frond Length")+
  facet_wrap(~site, ncol = 2)+
  theme_bw()

graph_FL

graph_FM <- ggplot(data = eck, aes(x = ID, y = frond_mass))+
  geom_point(aes(colour = site))+
  geom_smooth(method = "gam") +
  labs(x = "Identification", y = "Frond Mass")+
  facet_wrap(~site, ncol = 2)+
  theme_bw()

graph_FM

graph_PBW <- ggplot(data = eck, aes(x = ID, y = primary_blade_width))+
  geom_point(aes(colour = site))+
  geom_smooth(method = "gam") +
  labs(x = "Identification", y = "Primary Blade Width")+
  facet_wrap(~site, ncol = 2)+
  theme_bw()

graph_PBW


graph_PBL <- ggplot(data = eck, aes(x = ID, y = primary_blade_length))+
  geom_point(aes(colour = site))+
  geom_smooth(method = "gam") +
  labs(x = "Identification", y = "Primary Blade Length")+
  facet_wrap(~site, ncol = 2)+
  theme_bw()

graph_PBL


###############################################################################

# Gridding together


ggarrange(graph_SL,graph_SM,graph_FL,graph_FM,graph_PBW,graph_PBL, # Select graphs
          ncol = 2, nrow = 4,  # columns = 2, rows = 4
          labels = c("A", "B", "C", "D", "E", "F"),  # Set labels
          common.legend = TRUE)

###############################################################################

# Exercise 2, (Cutting Out a Country)
# Kyle Albertyn
# 30 Jan 2020

# Loading in libraries

library(ggplot2)
library(tidyverse)


# Create ggplot w the world

ggplot() +
  borders() + # The global shape file
  coord_equal() # Equal sizing for lon/lat 

n_1 <- ggplot()+
  borders(fill = "white", colour = "black")+# Setting borders
  coord_equal(xlim = c(2, 32), ylim = c(56, 75), expand = 0)+# setting coordinates
  labs(x = "Longitude", y = "Latitude") # setting axis labels

n_1


n_2 <- n_1 +
  annotate("text", label = "Barents\nSea", # creating text
           x = 25, y = 73, # setting placement of txt
           size = 7, # setting size
           angle = 0, # setting angle
           colour = "blue") + #setting colour and repeating for the rest of the labels
  annotate("text", label = "Norwegian\nSea", 
           x = 10, y = 70, 
           size = 7, 
           angle = 45, 
           colour = "red")+
  annotate("text", label = "Norway ^^", 
           x = 10, y = 62, 
           size = 8, 
           angle = 45, 
           colour = "Black")+
  annotate("text", label = "Oslo", 
           x = 10.4, y = 60, 
           size = 5, 
           angle = 0, 
           colour = "purple")+
  annotate("text", label = "Sweden", 
           x = 15, y = 60, 
           size = 5, 
           angle = 45, 
           colour = "orange")+
  annotate("text", label = "Finland", 
         x = 25, y = 63, 
         size = 5, 
         angle = 45, 
         colour = "grey")


n_2

n_final <- n_2 + north(x.min = 5, x.max = 10, y.min = 65, y.max = 68, # Set location of symbol
                  scale = 1.2, symbol = 16)
n_final #display the final map


kyle_palette <- scale_colour_gradientn(colours = c("#A5A94D", "#6FB16F", "#45B19B",
                                                   "#59A9BE", "#9699C4", "#CA86AD")) # creating palette




