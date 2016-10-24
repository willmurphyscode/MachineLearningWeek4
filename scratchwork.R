.libPaths("D:/rlibs")

require(data.table)
require(xts)
require(caret)
require(lubridate)

setwd("D:\\Projects\\MachineLearningWeek4")
#setwd("C:\\Users\\william.murphy\\MachineLearningWeek4")

source(".\\downloadData.R")

trainSet <- fread(trainFile)
testSet <- fread(testFile)
trainData <- trainSet[,-1,with=FALSE]
trainData$classe = factor(trainData$classe)
classe <- trainSet[,160,with=FALSE]
classe <- unlist(classe)
classe <- factor(classe)
trainData$cvtd_timestamp <- as.POSIXct(trainData$cvtd_timestamp)

ixNaCols <- which(apply(trainData, 2, function(x){ any(is.na(x)) | any(x == "") }))

noNas <- trainData[,-ixNaCols, with=FALSE]

#ixNaCols2 <- which(apply(noNas, 2, function(x){ any(is.na(x)) }))

myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 75,
                              horizon = 25,
                              fixedWindow = TRUE)


attempt <- train(classe ~ .,
                    data = noNas[1:2000,],
                    method = "rf",
                    trControl = myTimeControl)



trainMods <- buildModels(train)
testMods <- buildModels(test)

#as.xts requires the date column to be first.
#the last column of train is "classe" and the last column of test is "problem_id" 
timestampName <- "cvtd_timestamp"
currentCnames <- colnames(test)
ixDate <- match(timestampName, currentCnames)
desiredCnams <- c(timestampName, currentCnames[1:ixDate -1], currentCnames[ixDate + 1 : (length(currentCnames) - ixDate)])

setcolorder(test, desiredCnams)
setcolorder(train, desiredCnams)

test_ts <- xts::xts(test)

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
mod <- train(x = train, y = classe, method ="rpart")


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



vecvecvec <- sapply(colnames(train), function(x) { xyColor("gyros_arm_x", x)})

