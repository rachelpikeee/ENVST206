# Rachel Pike
# Final Project Script
# Started 10/19/2020
# Finished 11/21/2020

# Installing libraries
library(tidyr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(cowplot)
library(rstatix)
library(moments)
library(formattable)

# ORGANIZING DATA AND INITIAL FIGURES ------------------------------------------

# Reading in the wildfire and drought data sets
FireData <- read.csv("/Users/rachelpike/ENVST Data/final project/CAfiredata.csv")
DroughtData <- read.csv("/Users/rachelpike/ENVST Data/final project/droughtdata.csv")
CountyDroughtData <- read.csv("/Users/rachelpike/ENVST Data/final project/CAdroughtdata.csv")

# Making sure all data columns are stored as dates
FireData$incident_date_created <- as.Date(FireData$incident_date_created, "%Y-%m-%d")
FireData$incident_date_last_update <- as.Date(FireData$incident_date_last_update, "%Y-%m-%d")
DroughtData$ValidStart <- as.Date(DroughtData$ValidStart, "%Y-%m-%d")
DroughtData$ValidEnd <- as.Date(DroughtData$ValidEnd, "%Y-%m-%d")
CountyDroughtData$ValidStart <- as.Date(CountyDroughtData$ValidStart, "%Y-%m-%d")
CountyDroughtData$ValidEnd <- as.Date(CountyDroughtData$ValidEnd, "%Y-%m-%d")

# Adding a year column to help subset the both data sets
DroughtData$Year <- year(DroughtData$ValidStart)
CountyDroughtData$Year <- year(CountyDroughtData$ValidStart)
FireData$Year <- year(FireData$incident_date_created)

# Getting rid of outlier data point in fire data from 1969
FireData <- subset(FireData, FireData$Year >= 2000)
FireData <- subset(FireData, FireData$Year < 2020)
FireData <- subset(FireData, FireData$incident_acres_burned > 0)

# Subsetting drought data to match fire data --> earliest date is 2013 for fire
DroughtData <- subset(DroughtData, DroughtData$Year >= 2012)
CountyDroughtData <- subset(CountyDroughtData, CountyDroughtData$Year >= 2012)

# Getting summary statistics for the Fire data area burned
summary(FireData$incident_acres_burned)
sd(FireData$incident_acres_burned)

# Creating a plot of area burned for each year (COULD BE INCLUDED)
ggplot(data = FireData, aes(x=Year, y=incident_acres_burned))+
  geom_count(color="chocolate")+
  theme_classic()+ # git rid of ugly gridlines
  labs(title = "Distribution of Fire Sizes Since 2013",
       x = "Year",
       y = "Area Burned (Acres)")

# Create data all data frame that will store year, mean area burned, min/max area burned, 
# average drought conditions, min/max drought conditions
DataAll <- matrix(0, nrow = 7, ncol = 8)

# Creating columns of years
for (i in 1:7){
  DataAll[i, 1] <- 2012 + i
  current_year_fire_data <- subset(FireData, FireData$Year == DataAll[i,1])
  current_year_drought_data <- subset(DroughtData, DroughtData$Year == DataAll[i,1])
  DataAll[i,2] <- mean(current_year_fire_data$incident_acres_burned)
  DataAll[i,3] <- max(current_year_fire_data$incident_acres_burned)
  DataAll[i,4] <- mean(current_year_drought_data$D0)
  DataAll[i,5] <- mean(current_year_drought_data$D1)
  DataAll[i,6] <- mean(current_year_drought_data$D2)
  DataAll[i,7] <- mean(current_year_drought_data$D3)
  DataAll[i,8] <- mean(current_year_drought_data$D4)
}

# Making DataAll a data frame with appropriate columns
DataAll <- as.data.frame(DataAll)
colnames(DataAll) = c("Year", "Mean_Acres_Burned", "Max_Acres_Burned", "Mean_D0", "Mean_D1", "Mean_D2", "Mean_D3", "Mean_D4")

# Creating plots for acres burned and area in D3
acres_burned_plot <- ggplot(data = DataAll, mapping = aes(x = Year, y = Mean_Acres_Burned))+
  geom_col(fill = "darkred")+
  theme_classic()+
  labs(title = "Mean Acres Burned in California", x = "Year", y = "Acres Burned (acres)")
D3plot <- ggplot(data = DataAll, mapping = aes(x = Year, y = Mean_D3))+
  geom_col(fill = "darkorange")+
  theme_classic()+
  labs(title = "% Area in D3 drought in California", x = "Year", y = "% Area")
plot_grid(acres_burned_plot, D3plot) # (COULD BE INCLUDED)

# COUNTY ANALYSIS --------------------------------------------------------------

# Creating datasets for three counties
Shasta <- subset(FireData, FireData$incident_county == "Shasta")
LA <- subset(FireData, FireData$incident_county == "Los Angeles")
Fresno <- subset(FireData, FireData$incident_county == "Fresno")

# Getting rid of zero values
LA <- subset(LA, LA$incident_acres_burned > 0)
Fresno <- subset(Fresno, Fresno$incident_acres_burned > 0)

# Looking at means
mean(Shasta$incident_acres_burned)
mean(LA$incident_acres_burned)
mean(Fresno$incident_acres_burned)

# Histogram of normal data
ShastaPlot <- ggplot(data = Shasta, aes(x = incident_acres_burned))+
  geom_histogram(binwidth = 5000, color = "black", fill = "goldenrod2")+
  labs(title = "Frequency of Fire Area in Shasta, CA 2013-2019", x = "Acres Burned (acres)",
       y = "Frequency")
FresnoPlot <- ggplot(data = Fresno, aes(x = incident_acres_burned))+
  geom_histogram(binwidth = 12000, color = "black", fill = "goldenrod2")+
  labs(title = "Frequency of Fire Area in Fresno, CA 2013-2019", x = "Acres Burned (acres)",
       y = "Frequency")
LAPlot <- ggplot(data = LA, aes(x = incident_acres_burned))+
  geom_histogram(binwidth = 3000, color = "black", fill = "goldenrod2")+
  labs(title = "Frequency of Fire Area in Los Angeles, CA 2013-2019", x = "Acres Burned (acres)",
       y = "Frequency")
plot_grid(ShastaPlot, LAPlot, FresnoPlot) # (COULD BE INCLUDED)

# Checking normality --> NOT normal, can't use ANOVA
shapiro.test(Shasta$incident_acres_burned)
shapiro.test(LA$incident_acres_burned)
shapiro.test(Fresno$incident_acres_burned)

# Checking skewness of data and trying data transformations
skewness(Shasta$incident_acres_burned, na.rm = TRUE)
skewness(LA$incident_acres_burned, na.rm = TRUE)
skewness(Fresno$incident_acres_burned, na.rm = TRUE)

# Using inverse for severe skewness
Shasta$acres_burned_trans <- 1/(Shasta$incident_acres_burned)^.3
skewness(Shasta$acres_burned_trans, na.rm = TRUE)

# Using log for moderate skewness
LA$acres_burned_trans <- 1/(LA$incident_acres_burned)^.3
skewness(LA$acres_burned_trans, na.rm = TRUE)

# Using inverse for severe skewness
Fresno$acres_burned_trans <-  1/(Fresno$incident_acres_burned)^.3
skewness(Fresno$acres_burned_trans, na.rm = TRUE)

# Trying shaprio tests again -> now they seem to all fit this test
shapiro.test(Shasta$acres_burned_trans)
shapiro.test(LA$acres_burned_trans)
shapiro.test(Fresno$acres_burned_trans)

# Combining all county data into one dataframe
countydata <- rbind(Shasta, LA)
countydata <- rbind(countydata, Fresno)

# Checking for equal variance --> NOT equal, can't use ANOVA
bartlett.test(countydata$acres_burned_trans ~ countydata$incident_county)

# Trying Anova Test
my.aov <- aov(countydata$incident_acres_burned ~ countydata$incident_county, data = countydata)
summary(my.aov)

# Run Tukey HSD
tukeyT <- TukeyHSD(my.aov)
# View results
tukeyT

# Trying the Kruskal-Wallis Test
kruskal.test(incident_acres_burned ~ incident_county, data = countydata)

# Using the Wilcox test for post hoc
pairwise.wilcox.test(countydata$incident_acres_burned, countydata$incident_county)

# pair with box or violin plot
ggplot(data = countydata, aes(x = incident_county, y = incident_acres_burned))+
  geom_violin(scale = "area", color = "black", fill = "goldenrod2")+
  theme_classic()

# LARGE FIRE ANALYSIS ----------------------------------------------------------

# Finding top 3 fires
# Creating data frames for each county drought data
RanchDrought <- subset(CountyDroughtData, CountyDroughtData$County == c("Colusa County", "Glenn County", "Lake County", "Mendocino County"))
ThomasDrought <- subset(CountyDroughtData, CountyDroughtData$County == c("Santa Barbara County", "Ventura County"))
RimDrought <- subset(CountyDroughtData, CountyDroughtData$County == "Tuolumne County")

# Isolating years before and after each major fire
RanchDrought <- subset(RanchDrought, RanchDrought$Year == c(2017,2018,2019))
ThomasDrought <- subset(ThomasDrought, ThomasDrought$Year == c(2016,2017,2018))
RimDrought <- subset(RimDrought, RimDrought$Year == c(2012,2013,2014))

RanchD3Before <- mean(RanchDrought$D3[RanchDrought$Year == 2017])
RanchD3During <- mean(RanchDrought$D3[RanchDrought$Year == 2018])
RanchD3After <- mean(RanchDrought$D3[RanchDrought$Year == 2019])

ThomasD3Before <- mean(ThomasDrought$D3[ThomasDrought$Year == 2016])
ThomasD3During <- mean(ThomasDrought$D3[ThomasDrought$Year == 2017])
ThomasD3After <- mean(ThomasDrought$D3[ThomasDrought$Year == 2018])

RimD3Before <- mean(RimDrought$D3[RimDrought$Year == 2012])
RimD3During <- mean(RimDrought$D3[RimDrought$Year == 2013])
RimD3After <- mean(RimDrought$D3[RimDrought$Year == 2014])

# Creating vectors to become columns for a data frame
County <- c("Ranch (Colusa, Glenn, Lake, and Mendocino Counties)","Ranch (Colusa, Glenn, Lake, and Mendocino Counties)","Ranch (Colusa, Glenn, Lake, and Mendocino Counties)", "Thomas (Santa Barbara and Ventura Counties)","Thomas (Santa Barbara and Ventura Counties)","Thomas (Santa Barbara and Ventura Counties)", "Rim (Tuolumne County)","Rim (Tuolumne County)","Rim (Tuolumne County)")
Time <- c("Before Fire", "During Fire", "Post Fire", "Before Fire", "During Fire", "Post Fire", "Before Fire", "During Fire", "Post Fire")
Values <- c(RanchD3Before, RanchD3During, RanchD3After, ThomasD3Before, ThomasD3During, ThomasD3After, RimD3Before, RimD3During, RimD3After)

# Combining columns into one table
TopFireDrought <- tibble(County, Time, Values)
colnames(TopFireDrought) <- c("County", "Time", "% D3")

# Plotting the conditions before, during, and after each fire
ggplot(data = TopFireDrought, aes(x = Time, y = Values, fill = County))+
  geom_col(position = "dodge")+
  labs(title = "% of County in D3 Drought Before, During and After Major Fire", x = "Time",
       y = "% of County", fill = "Fire Name")+
  theme_classic()


