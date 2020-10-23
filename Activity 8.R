# Activity 8
# 10/23/2020

#install.packages(c("raster"))

library(raster)
library(ggplot2)
library(rgdal)

# Reading in all data in the Oneida folder
dirR <- "/Users/rachelpike/ENVST Data/a08/oneida"

# Read in Sentinel data
rdatB2 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B02_20m.tif"))
rdatB3 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B03_20m.tif"))
rdatB4 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B04_20m.tif"))
rdatB8 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B08_20m.tif"))

# Look at an inital plot
plot(rdatB2/10000)

# Stack raster data to see what the true color looks like
# Stack red green blue
rgbS <- stack(rdatB4,rdatB3,rdatB2)/10000
# View raster and use stretch argument to add contrast to the image
plotRGB(rgbS,  stretch="lin")

# Plotting full spatial resolution by changing max pixel argument
plotRGB(rgbS,  stretch="lin", maxpixels=rgbS@nrows*rgbS@ncols)

ndvi <- (rdatB8 - rdatB4)/(rdatB8 + rdatB4)
plot(ndvi)

# Creating a false color map
gbrS <- stack(rdatB3,rdatB2,rdatB4)/10000
plotRGB(gbrS,  stretch="lin", maxpixels=gbrS@nrows*gbrS@ncols)
