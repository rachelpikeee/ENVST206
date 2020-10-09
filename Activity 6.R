# Activity 6
# 10/9/2020

# Installing packages for the activity
install.packages(c("sp","rgdal","dplyr"))

# Loading in the libraries
library(sp)
library(dplyr)

# Go back and check on this one
library(rgdal)

# Reading in the shape files
g1966 <- readOGR("/Users/rachelpike/ENVST Data/a06/GNPglaciers/GNPglaciers_1966.shp")
g2015 <- readOGR("/Users/rachelpike/ENVST Data/a06/GNPglaciers/GNPglaciers_2015.shp")

# Getting an initial map of the glaciers
plot(g1966, col="skyblue")

# To refer a data frame spatial data we use "@"
g1966@data

# To refer to specific column, we first refer to data frame
g1966@data$GLACNAME

# Looking the glacier area for both years
g1966@data$Area1966
g2015@data$Area2015

# EXAMPLE creating a new data frame to match up data 
exp1 <- data.frame(NAME = as.factor(c("a", "b", "c")),
                   per.change = c(50,40,65))
exp2 <- data.frame(NAME = as.factor(c("a", "b", "c", "d")),
                   mass.gt = c(70,8,10,30))
glac <- full_join(exp1, exp2, by="NAME")
