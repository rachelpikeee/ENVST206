# Activity 5

# Loading in the ggplot package (need to do everytime)
library(ggplot2)

# Reading in data set
datW <- read.csv("/Users/rachelpike/ENVST Data/noaa2011124.csv")

# Specify that the name column should be a factor
datW$NAME<- as.factor(datW$NAME)

# Set up a vector of all names for each level
nameS <- levels(datW$NAME)
nameS

# Make a dataframe with just precipitation, year, and site name
# Remove NA using na.omit
datP <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           PRCP=datW$PRCP))

# Getting total annual precipitation (mm)
pr <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="sum", na.rm=TRUE)

# Changing column sames
colnames(pr) <- c("NAME", "YEAR", "totalP")

# Add the x column from aggregate looking at the length of observations in each year
# This is to see how many observations we have per year
pr$ncount <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="length")$x

# Subsetting data by how many observations we think is acceptable
# No convention, just what we think is appropriate for the data
pr <- pr[pr$ncount >= 364,]

# Looking at only livermore california and morrisville new york preciptiation
ca <- pr[pr$NAME == nameS[2], ]
ny <- pr[pr$NAME == nameS[5], ]

# Making a plot comparing New York and California precip
plot(ca$YEAR, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n",
     ylim =c(0, 1600))
#add y axis
axis(2, seq(0,1600, by=400), las=2 )
#add arizona
points(ny$YEAR, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")
#add legend

legend("topleft", #position
       c("California", "New York"), #labels
       col= c("black", "tomato3"), #colors
       pch=19, #point shape
       lwd=1, #line thickness 1, anytime both point & line arguments are given both will be drawn
       bty="n") #always use this argument otherwise an ugly box is drawn

# Adding a mean annual temp to datW
datW$MAT <- (datW$TMAX + datW$TMIN)/2

# Make a dataframe with just mean annual temp, year, and site name
# Remove NA using na.omit
datT <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           MAT=datW$MAT))

# Getting average annual mean temperatures (mm)
mat <- aggregate(datT$MAT, by=list(datT$NAME,datT$year), FUN="mean", na.rm=TRUE)

# Changing column sames
colnames(mat) <- c("NAME", "YEAR", "MAT")

# Add the x column from aggregate looking at the length of observations in each year
# This is to see how many observations we have per year
mat$ncount <- aggregate(datT$MAT, by=list(datT$NAME,datT$year), FUN="length")$x

# Subsetting data by how many observations we think is acceptable
# No convention, just what we think is appropriate for the data
mat <- mat[mat$ncount >= 364,]

# Looking at only livermore california and morrisville new york preciptiation
ND <- mat[mat$NAME == nameS[3], ]
NY <- mat[mat$NAME == nameS[5], ]

# Making a plot comparing New York and North Dakota mat
plot(ND$YEAR, ND$MAT,
     type = "b",
     pch = 19,
     col = "deepskyblue2",
     ylab = "Mean Annual Temperature (˚C)",
     xlab = "Year") 
     #yaxt = "n",)
     #ylim =c(0, 1600))
#add y axis
#axis(2, seq(0,1600, by=400), las=2 )
#add arizona
points(NY$YEAR, NY$MAT,
       type = "b",
       pch = 19,
       col="goldenrod2")
#add legend

legend("topleft", #position
       c("California", "New York"), #labels
       col= c("black", "tomato3"), #colors
       pch=19, #point shape
       lwd=1, #line thickness 1, anytime both point & line arguments are given both will be drawn
       bty="n") #always use this argument otherwise an ugly box is drawn



# Installing the ggplot package (only need to do once)
install.packages("ggplot2")

ggplot(data = pr,
       aes(x = YEAR,
           y = totalP,
           color = NAME)) +
  geom_point() +
  geom_path() +
  labs(x = "Year", y = "Total Annual Precepitation (mm)") +
  theme_classic() +
  scale_color_manual(values = c("#7FB3D5","#34495E", "#E7B800", "#FC4E07","#26A69A"))
