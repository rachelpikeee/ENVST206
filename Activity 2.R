# Activity 2

heights <- c(3,2,3)

datW <- read.csv("/Users/rachelpike/Desktop/2020-2021/Semester 1/Data Science/a02/noaa2011124.csv")

datW$PRCP_cm <- datW$PRCP/10

mean(datW$PRCP_cm, na.rm=TRUE)
