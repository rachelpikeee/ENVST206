# Final Project Script
# Started 10/19/2020

library(tidyr)
library(lubridate)
library(dplyr)

# Reading in the wildfire and drought datasets
FireData <- read.csv("/Users/rachelpike/ENVST Data/final project/CAfiredata.csv")
DroughtData <- read.csv("/Users/rachelpike/ENVST Data/final project/CAdroughtdata.csv")

# Making sure all data columns are stored as dates
FireData$incident_date_created <- as.Date(FireData$incident_date_created, "%Y-%m-%d")
FireData$incident_date_last_update <- as.Date(FireData$incident_date_last_update, "%Y-%m-%d")
DroughtData$ValidStart <- as.Date(DroughtData$ValidStart, "%Y-%m-%d")
DroughtData$ValidEnd <- as.Date(DroughtData$ValidEnd, "%Y-%m-%d")

# Adding a year column to help subset the drought data
DroughtData$Year <- year(DroughtData$ValidStart)

# Subsetting drought data to match fire data --> earliest date is 2013 for fire
DroughtData <- subset(DroughtData, DroughtData$Year >= 2013)

# NEXT STEPS: get some initial summary statistics and plots