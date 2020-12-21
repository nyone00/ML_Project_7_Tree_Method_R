# Get the Data
library(ISLR)
head(College)

df <- as.data.frame(College)

# EDA
library(ggplot2)
library(dplyr)

# create a scatterplot of Grad.Rate versus Room.Board, colored by the Private column
ggplot(df,aes(Grad.Rate,Room.Board)) + geom_point(aes(color=Private))

# create a histogram of full time undergrad students, color by Private
ggplot(df,aes(F.Undergrad)) + geom_histogram(aes(fill=Private),color='black',bins=50) + theme_bw()

# create a histogram of Grad.Rate colored by Private.
ggplot(df,aes(Grad.Rate)) + geom_histogram(aes(color=Private))

# found a Graduation Rate of abpbe 100%
odd <- subset(df,Grad.Rate > 100)
odd
df['Cazenovia College',]['Grad.Rate'] <- 100
# or df['Cazenovia','Grad.Rate'] <- 100


# TRAIN TEST SPLIT
library(caTools)
library(rpart)
library(rpart.plot)
set.seed(101)

sample <- sample.split(df$Private, SplitRatio = 0.7)

train <- subset(df, sample == T)
test <- subset(df, sample == F)

# Train model
# Decision Tree
# use the rpart library to build a decision tree to predict whetger or not 
# a school is Private
tree <- rpart(Private ~. , method = 'class', data = train)

# use predict() to predict the Private label on the test data
predicted.private <- predict(tree, newdata = test)

predicted.private <- as.data.frame(predicted.private)

merging <- function(x){
  if (x>=0.5){
    return('Yes')
  }else{
    return('No')
  }
}
  
# turn these two columns into one column to match the original Yes/No Label for a 
# Private column
predicted.private$Private <- sapply(predicted.private$Yes,merging)

print(head(predicted.private))

# use table() to create a onfusion matrix of tree model
table(test$Private, predicted.private$Private)

# plot out the tree model using prp()
prp(tree)


# Build Random Forest
library(randomForest)
set.seed(101)

# use randomeForest() to build out a model to predict Private class. 
# add importnace= TRUE as a parameter in the model. 
rf.model <- randomForest(Private ~ . , data = train, importance = TRUE)

rf.model$confusion # confusion of training data not predicted. 

# grab the feature importance with model$importance. 
rf.model$importance

# Predictions
pre.random.Forest <- predict(rf.model,test)

table(test$Private,pre.random.Forest)
# perform better than single tree

#   P      No     Yes
#  No      57      5
#  Yes      7     164