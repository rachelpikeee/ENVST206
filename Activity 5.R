# Activity 5

# Loading in the ggplot package (need to do everytime)
library(ggplot2)

library(lubridate)

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

# Make a dataframe with just mean annual temp, year, and site name
# Remove NA using na.omit
datT <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           TMAX=datW$TMAX))

# Getting average annual mean max temperatures (mm)
tmax <- aggregate(datT$TMAX, by=list(datT$NAME,datT$year), FUN="mean", na.rm=TRUE)

# Changing column sames
colnames(tmax) <- c("NAME", "YEAR", "TMAX")

# Add the x column from aggregate looking at the length of observations in each year
# This is to see how many observations we have per year
tmax$ncount <- aggregate(datT$TMAX, by=list(datT$NAME,datT$year), FUN="length")$x

# Subsetting data by how many observations we think is acceptable
# No convention, just what we think is appropriate for the data
tmax <- tmax[tmax$ncount >= 364,]

# Looking at only livermore california and morrisville new york preciptiation
ND <- tmax[tmax$NAME == nameS[3], ]
NY <- tmax[tmax$NAME == nameS[5], ]

# Making a plot comparing New York and North Dakota mat
plot(ND$YEAR, ND$TMAX,
     type = "o",
     pch = 19,
     col = "deepskyblue2",
     ylab = "Mean Annual Temperature (˚C)",
     xlab = "Year",
     xlim = c(1930, 2016),
     yaxt = "n",
     ylim =c(8,16))
#add y axis
axis(2, seq(8,16, by=2), las=2 )
#add new york
points(NY$YEAR, NY$TMAX,
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
  labs(x="year", y="Maximimum temperature (˚C)")

# Making bar plot for AZ precip data
ggplot(data=sub, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Daily precipitation (mm)")

# Looking at daily patterns in CA site in 1974
CA <- datW[datW$NAME == nameS[2] & datW$ year == 1974,]

# Specify date format
# %Y means a four number year 
# - indicates that the date uses dashes to seperate
# %m means month
# %d means day
CA$DATE <- as.Date(CA$DATE,"%Y-%m-%d")

# Making scatter plot for 1974 CA data
ggplot(data=CA, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(title = "Maximum Temperature at Livermore CA Site in 1974", 
       x="year", y="Maximimum temperature (˚C)")

# Making bar plot for CA precip data
ggplot(data=CA, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Daily precipitation (mm)",
       title = "Daily Precipitation at Livermore CA Site in 1974")

# Installing lubridate package to look at the data by month
install.packages("lubridate")

# Looking just at the minimum temperatures at the Washington site for the past
#   two decades
WA <- datW[datW$NAME == nameS[1] & datW$ year >= 2000,]

# Making the date column stored as a date
WA$DATE <- as.Date(WA$DATE,"%Y-%m-%d")

# Isolating just the month data
WA$Month <- month(WA$DATE)

# Trying to get the average per month per year --> something is not working here
WAtmin <- aggregate(WA$TMIN, by=list(WA$year,WA$Month), FUN="mean", na.rm=TRUE)

colnames(WAtmin) <- c("YEAR", "MONTH", "TMIN")

# Getting the number of data per month
WAtmin$ncount <- aggregate(WA$TMIN, by=list(WA$year,WA$Month), FUN="length")$x

# Saying there must be at least 27 data points per month
WAtmin <- WAtmin[WAtmin$ncount >= 27,]

WAtmin$season <- ifelse((WAtmin$MONTH <=2 | WAtmin$MONTH >=12), "winter", 
                        ifelse((WAtmin$MONTH > 5 & WAtmin$MONTH < 9), "summer", 
                               ifelse((WAtmin$MONTH > 2 & WAtmin$MONTH < 6), "spring","fall")))


WAtminszn <- aggregate(WAtmin$TMIN, by=list(WAtmin$YEAR,WAtmin$season), FUN="mean", na.rm=TRUE)

colnames(WAtminszn) <- c("YEAR", "SEASON", "TMIN")


# Making plot for min temps

ggplot()+
  geom_line(data = WAtminszn, aes(x=YEAR, y=TMIN, color = SEASON))+
  theme_classic()+
  labs(title = "Annual Average Minimum Temperature in Aberden, WA", 
       x="Year", y="Minimum temperature (˚C)")+
  scale_colour_manual(labels = c("Fall (SON)", "Spring (MAM)", "Summer (JJA)", "Winter (DJF)"), values = c("darkorange2", "forestgreen", "goldenrod3", "deepskyblue3"))
  
  