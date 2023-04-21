#IS 577 Data Mining HW 1
#Student: Cheng-Hsuan Lin
#Due Date: Oct 4, 2021
#Using TMDB 5000 Movie Dataset from the Kaggle

#Set Working Directory
setwd("~/Downloads/01_IS 577/Assignment 1")

##Read the tmdb 5000 movies.csv and tmdb 5000 credits.csv files. 
#Read in Datasets
tmdb_5000_movies <- read.csv(file="tmdb_5000_movies.csv", header=TRUE, sep=",")
tmdb_5000_credits <- read.csv(file="tmdb_5000_credits.csv", header=TRUE, sep=",")

##Join the two data sets using the "movie_id" column of tmdb_5000_credits.csv and "id" column of tmdb 5000 movies.csv. 
#Merge Datasets
TMDB_5000 <- merge(tmdb_5000_movies, tmdb_5000_credits, 
                   by.tmdb_5000_movies="id", by.tmdb_5000_credits="movie_id")

##Show the columns' name of the joint table. 
names(TMDB_5000)
View(TMDB_5000)
#4,809 observations and 23 variables
 
##There is a column names popularity. 
##Output the title of the movie with the maximal popularity value.
TMDB_5000[which.max(TMDB_5000$popularity),]


##Data Cleaning 1: drop rows with Nan value
#drop movies without "release date"
#Output the number of movies before and after this step.
sum(is.na(TMDB_5000$release_date))
TMDB_5000_1 <- na.omit(TMDB_5000$release_date)
str(TMDB_5000)
str(TMDB_5000_1)


##Data Cleaning 2: drop rows with empty value
#drop movies without "keywords"
#Output the number of movies before and after this step
sum(is.na(TMDB_5000[["keywords"]] ))

#412 movies without 'keywords'
TMDB_5000_2 <- na.omit(TMDB_5000$keywords)
str(TMDB_5000)
str(TMDB_5000_2)

##Data Cleaning 3: replacing invalid value with mean value
##replace 0 runtime with average runtime
TMDB_5000$runtime_new<-replace(TMDB_5000$runtime, TMDB_5000$runtime==0, mean(TMDB_5000$runtime)) 

#Output the average runtime before and after this step
#before
mean(TMDB_5000$runtime)
#after
mean(TMDB_5000$runtime_new)



##Handling outlier: normalize popularity with z-score
##Remove movies if normalized popularity is larger than 3 or less than -3
mean(TMDB_5000$popularity)
sd(TMDB_5000$popularity)

zscore_popularity = ((TMDB_5000$popularity - mean(TMDB_5000$popularity))/sd(TMDB_5000$popularity))
zscore_popularity

which(zscore_popularity > 3)
which(zscore_popularity < -3)

TMDB_5000_new <- na.omit(TMDB_5000$zscore_popularity, which(zscore_popularity > 3), which(zscore_popularity < -3))
sum(TMDB_5000_new)

##Output the number of movies before and after this step



       
##Using "keywords" creates multi-hot features for movies: 
##A common way to represent movie data is using multi-hot encoding. 
##In multi-hot movie encoding, each movie is represented by a vector with length equal to the number of keywords. 
##If the movie has a keyword, the corresponding value in the vector is 1. Then write a function l2distance that takes two vectors and returns their L2-distance. 
##Your function should accept two keywords vectors of equal length. 
##Output the L2-distance between "Iron Man" and "Titanic", and the distance between "Iron Man" and "Titanic".
