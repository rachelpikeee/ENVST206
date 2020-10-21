#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(dplyr)

#you may work through each section or choose one challenge to focus on
#and work through as many examples as possible


#####################################
##### Review: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
					Height.cm = c(60,100,11.8))

iris <- iris
iris$Sepal.Length <- as.numeric(iris$Sepal.Length)
sepal.length <- iris$Sepal.Length
iris$Sepal.Width <- as.numeric(iris$Sepal.Width)
iris$Petal.Length <- as.numeric(iris$Petal.Length)
iris$Petal.Width <- as.numeric(iris$Petal.Width)

IrisHeight <- left_join(iris, height, by="Species")

#####################################
##### Challenge: learn apply    #####
#####################################
#take the average of all sepal and petal widths & lengths 
#for iris setosa
#use the apply function to take this average

apply(iris[iris$Species == "setosa", 1:4], 2, "mean")

Setosa <- subset(iris, iris$Species == "setosa")
averages <- apply(Setosa[1:4], 2, "mean")

#####################################
##### Greater Challenge:        #####
##### learn for loops           #####
#####################################
#take the average of all sepal and petal widths & lengths 
#for iris setosa
#using a for loop
#read writing a for loop in R  stop at the next portion)
#https://www.r-bloggers.com/2015/12/how-to-write-the-first-for-loop-in-r/

LoopAverages <- numeric()
for (i in 1:4){
  LoopAverages[i] <- mean(Setosa[1:length(Setosa[i]), i])
}

LoopAverages
averages
#####################################
#####################################
##### Extra challenge: for loops ####
##### and regression             ####
#####################################
#read writing a for loop in R  stop at the next portion)
#https://www.r-bloggers.com/2015/12/how-to-write-the-first-for-loop-in-r/

#Using only data for iris versicolor
#write a for loop
#that produces 3 regression tables
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length
#make a versicolor dataframe
versicolor <- iris[iris$Species == "versicolor",]
#variables in each regression
x <- c("Sepal.Length", "Petal.Length", "Sepal.Length")
y <- c("Sepal.Width", "Petal.Width", "Petal.Length")


		