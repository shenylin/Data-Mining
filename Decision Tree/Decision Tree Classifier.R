#IS 577 Data Mining HW2
#Student: Cheng-Hsuan Lin
#Due Date: Nov. 8, 2021
#Using titanic.csv

#install packages
install.packages("FSelector")
install.packages("rpart")
install.packages("caret", dependencies = TRUE)
install.packages("dplyr")
install.packages("rpart.plot")
install.packages("xlsx")
install.packages("data.tree")
install.packages("caTools")

library(FSelector)
library(rpart)
library(caret)
library(dplyr)
library(rpart.plot)
library(xlsx)
library(data.tree)
library(caTools)



#Set Working Directory
setwd("~/Downloads/01_IS 577/Assignment 2")

#Read in Dataset
library(readr)
titanic <- read.csv(file="titanic.csv", header=TRUE, sep=",")

##Show the column name
names(titanic)
View(titanic)
#1,045 observations and 4 variables

#Suffle the dataset
set.seed(42)
rows <- sample(nrow(titanic))
titanic2 <- titanic[rows, ]
View(titanic2)


#Split the data into training and test sets with an 80-20 split
library(dplyr)
train <- sample_frac(titanic2, 0.8)  
sid <-as.numeric(rownames(train)) 
test <- titanic2[-sid,] 


#Train a decision tree classifier on the training set
#and predict the class labels in the test set
library(rpart)
library(rpart.plot)
#train the decision tree classifier
fit <- rpart(survived~., data = train, method = 'class')

#prediction
predict <-predict(fit, test, type = 'class')
table_mat <- table(test$survived, predict)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

############################################################

#Set Working Directory
setwd("~/Downloads/01_IS 577/Assignment 2")

#Read in Dataset
library(readr)
SPAM <- read.csv(file="spamdata.csv", header=TRUE, sep=",")

##Show the column name
names(SPAM)
View(SPAM)
#4,601 observations and 58 variables

#Suffle the dataset
set.seed(3456)
rows <- sample(nrow(SPAM))
SPAM2 <- SPAM[rows, ]
View(SPAM2)

#Split the data into training and test sets with an 80-20 split
library(dplyr)
train <- sample_frac(SPAM2, 0.8)  
sid <-as.numeric(rownames(train)) 
test <- SPAM2[-sid,] 

#logistic regression
model <- glm(isSPAM ~.,data=train, family ="binomial")
summary(model) 

#train accuracy
train$model_prob <- predict(model, train, type = "response")
train <- train  %>% mutate(model_pred = 1*(model_prob > .53) + 0,
                         visit_binary = 1*(isSPAM == "Yes") + 0)
train <- train %>% mutate(accurate = 1*(model_pred == visit_binary))
sum(train$accurate)/nrow(train)

#test accuracy
test$model_prob <- predict(model, test, type = "response")
test <- test  %>% mutate(model_pred = 1*(model_prob > .53) + 0,
                         visit_binary = 1*(isSPAM == "Yes") + 0)
test <- test %>% mutate(accurate = 1*(model_pred == visit_binary))
sum(test$accurate)/nrow(test)

#cross validation to evaluate
library(tidyverse) 
library(caret)

prop.table(table(SPAM2$isSPAM))

# define training control
set.seed(100)
train.control <- trainControl(method = "cv", number = 10)

#average training accuracy
model_train <- train(factor(isSPAM) ~., data = train, method = "glm", trControl=train.control, 
                     tuneLength = 0)
model_train

#average test accuracy
model_test <- train(factor(isSPAM) ~., data = test, method = "glm", trControl=train.control, 
                    tuneLength = 0)
model_test

