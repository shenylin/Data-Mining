#IS 577 Data Mining HW3
#Student: Cheng-Hsuan Lin
#Due Date: Dec. 5, 2021
#Using wine.csv

#Set Working Directory
setwd("~/Downloads/01_IS 577/Assignment 3")

#Read in Dataset
library(readr)
wine <- read.csv(file="wine.csv", header=TRUE, sep=",")


##Show the column name
names(wine)
View(wine)
#178 observations and 13 variables

#Check data for null/missing values, and address them (if any exist) either by dropping the rows, or replacing them with column mean. 
#Check Missing Data
sum(is.na(wine))
#No Missing Data

#Listwise Deletion
wine2 <- na.omit(wine)
sum(is.na(wine2))


#dbscan package
library(fpc)
library(dbscan)
library(factoextra)
library(ggplot2)

#Perform DBSCAN on your cleaned/processed data set using min pts = 3, and epsilon = 25.
set.seed(220) # Setting seed
Dbscan_cl <- dbscan(wine2, eps = 25, minPts = 3, weights = NULL, borderPoints = TRUE)
Dbscan_cl

# Plot DBSCAN results
Dbscan_cl_1 <- dbscan::dbscan(wine2, 25, 3)
fviz_cluster(Dbscan_cl, wine2, geom = "point")



