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

# Combining area data - first work with a smaller data frame
gdf66 <- data.frame(GLACNAME = g1966@data$GLACNAME,
                    area66 = g1966@data$Area1966)

gdf15 <- data.frame(GLACNAME = g2015@data$GLACNAME,
                    area15 = g2015@data$Area2015)

# Join all data tables by glacier name
gAll <- full_join(gdf66,gdf15, by="GLACNAME")

# Calculating the % change in area from 1966 to 2015
gAll$gdiff <- ((gAll$area66-gAll$area15)/gAll$area66)*100

# Making a scatter plot of % change vs. 1966 area
plot(gAll$area66, gAll$gdiff,
     pch = 19,
     ylab = "% Change in Glacier Area",
     xlab = "Glacier Area in 1966 (m^2)",
     main = "Percent Change in Glacier Area from 1966 to 2015 vs. Glacier Area in 1966")

# Joining data with the spatial data table and overwrite into spatial data table 
g1966@data <- left_join(g1966@data, gAll, by="GLACNAME")
# Use spplot to shade polygons based on the % change of labels
# First argument is the spatial object
# Second is the column in of data to display with the different colors
# Add a title using main
# Col changes the color of the borders. This argument sets them to transparent
spplot(g1966, "gdiff", main="% change in area", col="transparent")


# Calculating the summary statistics for percent change
mean(gAll$gdiff)
sd(gAll$gdiff)
which.min(gAll$gdiff)
gAll$GLACNAME[25]
which.max(gAll$gdiff)
gAll$GLACNAME[5]
gAll$gdiff[5]
which.min(gdf66$area66)
gdf66$GLACNAME[9]
gdf66$area66[13]
which.max(gdf66$area66)
gdf66$GLACNAME[13]
gAll$gdiff[9]
gAll$gdiff[13]

# Making a data for the glacier with the largest percent loss
Boulder66 <- g1966[g1966@data$GLACNAME == "Boulder Glacier",]
Boulder15 <- g2015[g2015@data$GLACNAME == "Boulder Glacier",]

# Plotting the map of boulder glacier
plot(Boulder66, col = "deepskyblue4", border = "gray35", 
     main = "Boulder Glacier Area Loss from 1966 to 2015")
plot(Boulder15, add = TRUE, col = "lightcyan1", border = "gray35")
legend("bottomleft", legend=c("1966 Area (meters squared)", "2015 Area (meters squared)"),
       fill=c( "deepskyblue4", "lightcyan1"), bty="n", cex = 1)

# Making a data for the glacier with the smallest percent loss
Pumpelly66 <- g1966[g1966@data$GLACNAME == "Pumpelly Glacier",]
Pumpelly15 <- g2015[g2015@data$GLACNAME == "Pumpelly Glacier",]

# Plotting the map of boulder glacier
plot(Pumpelly66, col = "deepskyblue4", border = "gray35", 
     main = "Pumpelly Glacier Area Loss from 1966 to 2015")
plot(Pumpelly15, add = TRUE, col = "lightcyan", border = "gray35")
legend("bottomright", legend=c("1966 Area (meters squared)", "2015 Area (meters squared)"),
       fill=c("lightcyan1", "deepskyblue4"), bty="n", cex = 1)

