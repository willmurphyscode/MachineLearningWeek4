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
tmp <- apply(train, 2, function(x) {!any(is.na(x)) & !any(x == "")})
cnams <- names(tmp[which(tmp)])


#I'm running out of RAM trying to do an indiscriminant regression here. 
complete <- train[, cnams, with = FALSE]

#running time is too high: 
#mod <- train(classe ~ ., data = complete,  method = "glm") 

## ok, so the easy "just predict by all copmlete vectors" is not working 
## partly because I don't have the RAM and partly because there is 
## something going on in the GLM calls that I don't understand yet. 

## instead, plot some stuff to look for good predictors
tmp2 <- apply(train, 1, function(x) {class(x[2])})
cnams2 <- names(tmp2[which(tmp2)])

complete_numeric <- train[, cnams2, with = FALSE]


g <- ggplot(data = train, aes(x = gyros_arm_x, y = magnet_arm_x, color = classe)) + geom_point() 
g



xyColor <- function(cname1, cname2) {
  df <- data.frame(
    classe = train$classe,
    x =  train[,cname1,with=FALSE],
    y = train[,cname2,with=FALSE]
  )
  
  #g <- ggplot(data = train, aes(x = train[,.SD, .SDcols=c(cname1),], y = train[,.SD,.SDcols=c(cname2),], color = classe)) + geom_point() 
  g <- ggplot(data = df, aes(x = df[,cname1], y = df[,cname2], color = classe)) + geom_point() 
  print(g)
  print(head(df))
  mod1 <- train(classe ~ ., data = df, method = "rpart")
  print(class(mod1))

  mod1
}

cname1 <- "gyros_arm_x"
cname2 <- "magnet_arm_x"
xyColor("gyros_arm_x", "magnet_arm_x")

m1 <- xyColor("magnet_arm_x","gyros_arm_x" )

require(rattle)

fancyRpartPlot(m1$finalModel)





