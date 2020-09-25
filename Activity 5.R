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

# add normal r plots here to keep the same order of activity

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
