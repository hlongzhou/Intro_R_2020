# Author: Kyle Albertyn
# Summary Stats
# 29 January 2020
################################


# Loading Packages

library(tidyverse)



# Loading in the df

lam <- read_delim("data/laminaria.csv", 
                               ";", escape_double = FALSE, trim_ws = TRUE)

lam %>% # Chose the dataframe
  summarise(avg_bld_wdt = mean(blade_length)) # Calculate mean blade length

lam %>% # Tell R that we want to use the 'laminaria' dataframe
  group_by(site) %>% 
  summarise(avg_stp_ln = mean(total_length), # Create a summary of the mean of the total lengths
            sd_stp_ln  = sd(total_length), # Create a summary of the sd of the total lengths
            max_stp_ln = max(total_length), # Create a summary of the max of the total lengths
            min_stp_ln = min(total_length), # Create a summary of the min of the total lengths
            var_stp_ln = var(total_length),# Create a summary of the variance of the total lengths
            med_stp_ln = median(total_length))# Create a summary of the median of the total lengths


## Plotting sommmm graphs

# Import package ggplot

library(ggplot2)

# Creating a plot

ggplot(data = lam, aes(x = stipe_mass, y = stipe_length)) +
  geom_point(shape = 21, colour = "cyan", fill = "white") +
  labs(x = "Stipe mass (kg)", y = "Stipe length (cm)") + 
  scale_x_discrete()
  


# Plotting

chick <- datasets::ChickWeight

ggplot(data = chick, aes(x = Time, y = weight))+ 
  geom_point(colour = "cyan") +
  geom_line(colour = "cyan", aes(group = Chick))


ggplot(data = chick, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_line(aes(group = Chick))

ggplot(data = chick, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm")+
  theme_dark()

ggplot(data = chick, aes(x = Time, y = weight, colour = Diet)) +
  geom_point(aes(size = weight)) +
  geom_smooth(method = "lm", size = 0.5)+
  theme_bw()



## Faceting

library(tidyverse)
library(ggpubr)

# Load data
chick <- datasets::ChickWeight

# Create faceted figure
ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") + # Note the `+` sign here
  facet_wrap(~Diet, ncol = 2) + # This is the line that creates the facets
  labs(x = "Days", y = "Mass (g)")

ChickLast <- chick %>% 
  filter(Time == 21)


line_1 <- ggplot(data = chick, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_line(aes(group = Chick)) +
  labs(x = "Days", y = "Mass (g)")
line_1


lm_1 <- ggplot(data = chick, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "gam") +
  labs(x = "Days", y = "Mass (g)")
lm_1


# Note that we are using 'ChickLast', not 'ChickWeight'
histogram_1 <- ggplot(data = ChickLast, aes(x = weight)) +
  geom_histogram(aes(fill = Diet), position = "dodge", binwidth = 100) +
  labs(x = "Final Mass (g)", y = "Count")
histogram_1


# Note that we are using 'ChickLast', not 'ChickWeight'
box_1 <- ggplot(data = ChickLast, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet)) +
  labs(x = "Diet", y = "Final Mass (g)")
box_1


ggarrange(line_1, lm_1, histogram_1, box_1, 
          ncol = 2, nrow = 2, # Set number of rows and columns
          labels = c("A", "B", "C", "D"), # Label each figure
          common.legend = TRUE) # Create common legend


####################################################################################

# Exercises for Day 2
# 29 Jan 2020
# Kyle Albertyn 


# Loading in packages

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(hexbin)

# Loading in a random dataset

beaver <- datasets::beaver1
beaver <- as.data.frame(beaver)


# Create graph(s) of beaver



line_bvr <- ggplot(data = beaver, x = time, y = temp, colour = day) + # Creating  graph
  geom_line(aes(x = time, y = temp, colour = day)) +                  # Setting to geom_line
  theme_bw()+                                                         # Adding theme  
    labs(x = "Time (min)", y = "Temperature (C)")+                    # Adding labels
  ggtitle("Graph showing the changes in Temperature in C          
          Over Time in min according to two different days
          ")                                                          # Adding title
  
line_bvr + theme(legend.background = element_rect(fill="Grey",
           size=0.5, linetype="solid", 
           colour ="cyan")) + scale_x_discrete(limits=c("346", "347"))# Editing the legend



point_bvr  <- ggplot(data = beaver, x = time, y = temp, colour = day)+# Creating graph 
  geom_point(aes(x = time, y = temp, colour = day))+                  # Setting geom_point
  theme_classic()+                                                    # Adding theme
  labs(x = "Time (min)", y = "Temperature (C)")+                      # Adding Labels
  ggtitle("Graph showing the changes in Temperature in C
          Over Time in min according to two different days
          ")+                                                         # Adding title
  theme_get()+ theme_bw()                                             # Adding an extra 2 themes because why not


point_bvr+ theme(legend.background = element_rect(fill="Grey",
          size=0.5, linetype="solid", 
          colour ="cyan")) + scale_x_discrete(limits=c("346", "347")) # Editing the Legend



# trying to encircle

library("ggalt")

circle.df <- beaver %>% filter(day == "347")

graph <- ggplot(beaver, aes(x = "time" , y = "temp" , colour = "day")) + 
  scale_x_continuous()+
  scale_y_continuous()+ 
  geom_point(aes(colour = day)) +
  geom_encircle(data = circle.df, linetype = 2)

graph

library("ggalt")
circle.df <- iris %>% filter(Species == "setosa")
ggplot(iris, aes(Petal.Length, Petal.Width)) +
  geom_point(aes(colour = Species)) + 
  geom_encircle(data = circle.df, linetype = 2)+
  theme_bw()


# I've been trying to encircle the day 347 and 346 like in the iris data from line 195 to 199,
# however it is not working
# I'm thinking its because the data is incorrect as it gives a discrete vs continuous error
# Which I try to rectify w the scale_x_continuous() function however this doesn't work out. 
# Do you perhaps know how to fix this by any chance?

