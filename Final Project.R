# Final Project Script
# Started 10/19/2020

library(tidyr)
library(lubridate)
library(dplyr)
library(ggplot2)

# Reading in the wildfire and drought datasets
FireData <- read.csv("/Users/rachelpike/ENVST Data/final project/CAfiredata.csv")
DroughtData <- read.csv("/Users/rachelpike/ENVST Data/final project/CAdroughtdata.csv")

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

# Getting summary statistics for 2019
Fire2019 <- subset(FireData, FireData$Year == 2019)
summary(Fire2019$incident_acres_burned)
sd(Fire2019$incident_acres_burned)

# Plotting a histogram of the area burned for each fire in 2019
ggplot(data = Fire2019, aes(x=incident_acres_burned))+
  geom_histogram(binwidth = 2000, fill = "chocolate", color = "black")+
  labs(title = "Distribution of Area Burned in 2019",
       x = "Area Burned (Acres)",
       y = "Frequency")+
  theme_bw()

# Creating a violin plot for distributions of area burned for each year
ggplot(data = FireData, aes(x=Year, y=incident_acres_burned))+
  geom_count(color="chocolate")+
  theme_classic()+ # git rid of ugly gridlines
  labs(title = "Distribution of Fire Sizes Since 2013",
       x = "Year",
       y = "Area Burned (Acres)")

