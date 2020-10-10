# Activity 6
# 10/9/2020

# Installing packages for the activity
# install.packages(c("sp","rgdal","dplyr"))

# Loading in the libraries
library(sp)
library(dplyr)

# Go back and check on this one
library(rgdal)

# Reading in the shape files
g1966 <- readOGR("/Users/rachelpike/ENVST Data/a06/GNPglaciers/GNPglaciers_1966.shp")
g2015 <- readOGR("/Users/rachelpike/ENVST Data/a06/GNPglaciers/GNPglaciers_2015.shp")


# Getting an initial map of the glaciers
plot(g1966, col="skyblue", border = "grey50")

# To refer a data frame spatial data we use "@"
# g1966@data

# Previewing first few lines and columns of data table
head(g2015@data)

# Looking at projection information
g1966@proj4string

# To refer to specific column, we first refer to data frame
g1966@data$GLACNAME
g2015@data$GLACNAME

# Fixing hte glacier names to make sure they all match
#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))
