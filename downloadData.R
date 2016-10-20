
testFile <- ".\\data\\test.csv"
trainFile <- ".\\data\\training.csv"

downloadDataIfNecessary <- function() {

  
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
  
  #source("makeModels.R")
  
  testSet <- fread(testFile)
  test <- testSet[,-1, with=FALSE]
  test$cvtd_timestamp <- as.POSIXct(test$cvtd_timestamp)


}

downloadDataIfNecessary()