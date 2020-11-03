# Final Project Script
# Started 10/19/2020

library(tidyr)
library(lubridate)
library(dplyr)
library(ggplot2)

# Reading in the wildfire and drought datasets
FireData <- read.csv("/Users/rachelpike/ENVST Data/final project/CAfiredata.csv")
DroughtData <- read.csv("/Users/rachelpike/ENVST Data/final project/droughtdata.csv")

# Making sure all data columns are stored as dates
FireData$incident_date_created <- as.Date(FireData$incident_date_created, "%Y-%m-%d")
FireData$incident_date_last_update <- as.Date(FireData$incident_date_last_update, "%Y-%m-%d")
DroughtData$ValidStart <- as.Date(DroughtData$ValidStart, "%Y-%m-%d")
DroughtData$ValidEnd <- as.Date(DroughtData$ValidEnd, "%Y-%m-%d")

# Adding a year column to help subset the both data sets
DroughtData$Year <- year(DroughtData$ValidStart)
FireData$Year <- year(FireData$incident_date_created)

# Getting rid of outlier data point in fire data from 1969
FireData <- subset(FireData, FireData$Year >= 2000)

# Subsetting drought data to match fire data --> earliest date is 2013 for fire
 DroughtData <- subset(DroughtData, DroughtData$Year >= 2013)

# Getting summary statistics for the Fire data area burned
summary(FireData$incident_acres_burned)
sd(FireData$incident_acres_burned)

# # Plotting a histogram of the area burned for each fire in 2019
# ggplot(data = Fire2019, aes(x=incident_acres_burned))+
#   geom_histogram(binwidth = 2000, fill = "chocolate", color = "black")+
#   labs(title = "Distribution of Area Burned in 2019",
#        x = "Area Burned (Acres)",
#        y = "Frequency")+
#   theme_bw()
# 
# # Creating a violin plot for distributions of area burned for each year
# ggplot(data = FireData, aes(x=Year, y=incident_acres_burned))+
#   geom_count(color="chocolate")+
#   theme_classic()+ # git rid of ugly gridlines
#   labs(title = "Distribution of Fire Sizes Since 2013",
#        x = "Year",
#        y = "Area Burned (Acres)")

# New idea = look at each year (maybe just fire season) and compare the average
# area burned with average drought conditions for the state that year

# second analysis --> pick 3 largest fires for each year and compare drought conditions
# before, during, and after each and compare

# Creating data frames for each year of fire data
Fire2019 <- subset(FireData, FireData$Year == 2019)
Fire2018 <- subset(FireData, FireData$Year == 2018)
Fire2017 <- subset(FireData, FireData$Year == 2017)
Fire2016 <- subset(FireData, FireData$Year == 2016)
Fire2015 <- subset(FireData, FireData$Year == 2015)
Fire2014 <- subset(FireData, FireData$Year == 2014)
Fire2013 <- subset(FireData, FireData$Year == 2013)

# Creating data frames for each year of drought data
Drought2019 <- subset(DroughtData, DroughtData$Year == 2019)
Drought2018 <- subset(DroughtData, DroughtData$Year == 2018)
Drought2017 <- subset(DroughtData, DroughtData$Year == 2017)
Drought2016 <- subset(DroughtData, DroughtData$Year == 2016)
Drought2015 <- subset(DroughtData, DroughtData$Year == 2015)
Drought2014 <- subset(DroughtData, DroughtData$Year == 2014)
Drought2013 <- subset(DroughtData, DroughtData$Year == 2013)

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

DataAll <- as.data.frame(DataAll)
colnames(DataAll) = c("Year", "Mean_Acres_Burned", "Max_Acres_Burned", "Mean_D0", "Mean_D1", "Mean_D2", "Mean_D3", "Mean_D4")

# Next Steps = figure out what test to use / if we need to use a test here,
# do some reading on typical drought conditions that lead to a fire
# look at top 3 biggest fires to try and isolate drought conditions before, during, and after
 


