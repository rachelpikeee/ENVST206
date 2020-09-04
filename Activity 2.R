# Activity 2

# Learning about vectors
heights <- c(3,2,3)

# Importing data set and learning basic functions
datW <- read.csv("/Users/rachelpike/Desktop/2020-2021/Semester 1/Data Science/a02/noaa2011124.csv")

datW$PRCP_cm <- datW$PRCP/10

mean(datW$PRCP_cm, na.rm=TRUE)

# Converting into a factor so it is easier to refer back to the different sites
datW$NAME <- as.factor(datW$NAME)

# Creating vectors of different types of data
CharEx <- c("hi", "@", "45", "data science", "is fun!")

IntEx <- c(1, 2, 3, 4, 5)

NumEx <- c(1.3, 3.0002, 4.0, 934.56, 2.5)

FactorEx <- factor(IntEx)

# Seeing the unique site names
levels(datW$NAME)

# Looking at the mean and SD of the aberdeen site
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)
sd(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

# Create a average temperature column in the data (halfway between TMAX and TMIN)
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

# Get the mean across all sites using aggregate function
# The "by" function is a list of one or more variables to index over.
# FUN indicates the function we want to use
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

# Change the automatic output of column names to be more meaningful
# MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

# Convert level to number for factor data type
# You will have to reference the level output or look at the row of data to see the character designation.
datW$siteN <- as.numeric(datW$NAME)

# Making a histogram for the first site in our levels
# main= is the title name argument, so we will paste the actual name of the 
#   factor not the numeric index since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")
help(hist)
