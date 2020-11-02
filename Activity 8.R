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

# Read in landcover points data
# I've also turned off the info print out here when you read in the file
algae <- readOGR(paste0(dirR,"/Oneida/algae.shp"), verbose=FALSE)
agri <- readOGR(paste0(dirR,"/Oneida/agriculture.shp"), verbose=FALSE)
forest <- readOGR(paste0(dirR,"/Oneida/forest.shp"), verbose=FALSE)
water <- readOGR(paste0(dirR,"/Oneida/water.shp"), verbose=FALSE)
wetlands <- readOGR(paste0(dirR,"/Oneida/wetlands.shp"), verbose=FALSE)

# Plot points and true color
plotRGB(rgbS, stretch="lin",maxpixels=2297430)
plot(algae, add=TRUE, col=rgb(0.5,0.5,0.5,0.5), pch=19)
plot(agri, add=TRUE, col=rgb(0.75,0.5,0.5,0.5), pch=19)
plot(forest, add=TRUE, col=rgb(0.75,0.75,0.25,0.5), pch=19)
plot(water, add=TRUE, col=rgb(0.33,0.75,0.75,0.5), pch=19)
plot(wetlands, add=TRUE, col=rgb(0.33,0.33,0.65,0.5), pch=19)
legend("bottomleft", c("algae","agri","forest","water","wetlands"),
       pch=19, col=c(rgb(0.5,0.5,0.5,0.5),rgb(0.75,0.5,0.5,0.5),rgb(0.75,0.75,0.25,0.5),rgb(0.33,0.75,0.75,0.5),rgb(0.33,0.33,0.65,0.5)),
       bty="n", cex=0.75)

# Set up a dataframe with all of the point coordinates
landExtract <-  data.frame(landcID = as.factor(rep(c("algae","water","agri","forest","wetland"),each=120)),
                           x=c(algae@coords[,1],water@coords[,1],agri@coords[,1],forest@coords[,1],wetlands@coords[,1]),
                           y=c(algae@coords[,2],water@coords[,2],agri@coords[,2],forest@coords[,2],wetlands@coords[,2]))

# Stack all bands
allbands <-  stack(rdatB2, rdatB3, rdatB4,rdatB8)/10000
# Add the raster reflectance values to the point coordinates and classes
# Extract(raster, matrix of coordinates)
# Raster:: helps ensure that extract comes from the raster package
ExtractOut <- raster::extract(allbands,landExtract[,2:3])
# Name the bands
colnames(ExtractOut) <- c("B02","B03","B04","B08")
# Combine the original data with the coordinates with the raster data
rasterEx <- cbind(landExtract,ExtractOut)
# Look at data
head(rasterEx)

# Plotting land cover classes across different bands
ggplot(data=rasterEx, aes(x=B08, y=B04, color=landcID))+
  geom_point(alpha=0.6)+
  theme_classic()+
  labs(title = "Red Light vs. Near Infrared Light",
       x = "Near Infrared (Band 8)",
       y = "Red Light (Band 4)")

# Extracting out NDVI for each band
ExtractNDVI <- raster::extract(ndvi,landExtract[,2:3])
rasterNDVI <- cbind(landExtract, Extract)
colnames(rasterNDVI) = c("landcID","x", "y", "NDVI") 
head(rasterNDVI)

ggplot(data = rasterNDVI[rasterNDVI$landcID == c("agri", "forest", "wetland"),], aes(x=landcID, y=NDVI, fill=landcID))+
  geom_violin(width = .9, alpha = 0.5)+
  geom_boxplot(width = 0.05, fill="grey90")+ 
  labs(title = "Distribution of Landcover NDVI Values",
       x = "Landcover Type",
       y = "NDVI") +
  theme_classic()




