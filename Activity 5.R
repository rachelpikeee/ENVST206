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
     type = "o",
     pch = 19,
     col = "deepskyblue2",
     ylab = "Mean Annual Temperature (˚C)",
     xlab = "Year",
     xlim = c(1930, 2016),
     yaxt = "n",
     ylim =c(2, 9))
#add y axis
axis(2, seq(2,9, by=1), las=2 )
#add new york
points(NY$YEAR, NY$MAT,
       type = "o",
       pch = 19,
       col="goldenrod1")
#add legend

legend("topleft", #position
       c("New York", "North Dakota"), #labels
       col= c("goldenrod1", "deepskyblue2"), #colors
       pch=19, #point shape
       lwd=1, #line thickness 1, anytime both point & line arguments are given both will be drawn
       bty="n") #always use this argument otherwise an ugly box is drawn



# Installing the ggplot package (only need to do once)
install.packages("ggplot2")

# Precip plot
ggplot(data = pr,
       aes(x = YEAR,
           y = totalP,
           color = NAME)) +
  geom_point() +
  geom_path() +
  labs(x = "Year", y = "Total Annual Precepitation (mm)") +
  theme_classic() +
  scale_color_manual(values = c("#003B92","#FFDF00", "#FF8C00", "#921800","#01A4C3"))

# Making violin plots
ggplot(data = datW, aes(x=NAME, y=TMIN))+ #look at daily tmin
  geom_violin(fill=rgb(0.933,0.953,0.98))+ #add a violin plot with blue color
  geom_boxplot(width=0.2,size=0.25, fill="grey90")+ #add grey boxplots and make them about 20% smaller than normal with 25% thinner lines than normal
  theme_classic() #git rid of ugly gridlines

# Looking at daily patterns in AZ site in 1974
sub <- datW[datW$NAME == nameS[4] & datW$ year == 1974,]

# Specify date format
# %Y means a four number year 
# - indicates that the date uses dashes to seperate
# %m means month
# %d means day
sub$DATE <- as.Date(sub$DATE,"%Y-%m-%d")

# Making scatter plot for 1974 AZ data
ggplot(data=sub, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximimum temperature (C)")

# Making bar plot for precip data
ggplot(data=sub, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Daily precipitation (mm)")

