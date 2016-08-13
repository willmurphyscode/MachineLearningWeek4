require(data.table)

## download data
testFile <- ".\\data\\test.csv"
trainFile <- ".\\data\\training.csv"

setwd("D:\\Projects\\MachineLearningWeek4")

downloadTrainingData <- function (){ 
  url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
  download.file(url, trainFile)
}

downloadTestData <- function () {
  url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
  
  download.file(url, testFile)
}

if(!file.exists(trainFile)) {
  downloadTrainingData()
}

if(!file.exists(testFile)) {
  downloadTestData()
}

test <- fread(testFile)
train <- fread(trainFile)

## TODO build validation set

## exploratory analysis
tab <- tapply(train$user_name, train$classe, length)


## just random models for now
require(caret)
require(dplyr)
tmp <- apply(train, 2, function(x) {sum(is.na(x))})
# TODO this call is failing because with an error because of some feature of the data 
# that I don't understand yet. 
mod <- train(train[,-c(160, which(tmp > 0))], train[,160], method = "rpart") 

