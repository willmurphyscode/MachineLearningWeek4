#transformRow.R
require(dplyr)
set.seed(8675309)
#I can just use the mean(column, na-rm = TRUE) to remove things
#the columns that are maxes will be fine, since they have only one 
#observations per window, and the data that is observed continuously
#will be transformed.
stopifnot( require(wavelets))


meanNumerics <- function(input) {
  result <- NULL
  if(is.numeric(input) && !any(is.na(input))) {
    print("entered wavelets")
    print(length(input))
    names(input) <- NULL
    result <- wavelets::dwt(as.double(input), n.levels=1)

    result
  } else {
    result <- input[1]
  }
  if(is.integer(result)) {
    result <- as.double(result)
  }
  
  result
  
}


minNumerics <- function(input) {
  result <- NULL
  if(is.numeric(input) ) {
    result <- min(input, na.rm = TRUE)
    as.double(result)
  } else {
    result <- input[1]
  }
  
  if(is.integer(result)) {
    result <- as.double(result)
  }
  
  result
  
}

first <- function(v) {
  v[1]
}

medianNumerics <- function(input) {
  result <- NULL
  if(is.numeric(input) && !any(is.na(input))) {
    result <- median(input, na.rm = TRUE)
    as.double(result)
  } else {
    result <- input[1]
  }
  
  if(is.integer(result)) {
    result <- as.double(result)
  }
  
  result
  
}

countSwitches <- function(v) {
    if(!is.numeric(v)) {
      #print(class(vec))
      return(NA)
    }
  
  
  vec <- as.numeric(v)
  
  if(any(is.na(vec))) {
    return(NaN)
  }
  
  offsetVec <- c(0, vec)
  otherVec <- c(vec, 0)
  deltas <- offsetVec - otherVec
  
  signs <- deltas >= 0
  
  on <- signs[1]
  
  switches <- 0
  for(i in 1:(length(deltas) - 1)) {
    
    if(on != signs[i]) {
      on <- ! on
      switches <- switches + 1
    }
  }
  as.numeric(switches)
  
}

makeIgnorableColumns <- function(dt) {
  ignoreMe <- NULL
  sadfaces <- apply(dt, 2 , function(x) { length(unique(x)) == 1})
  
  ignoreMe <- colnames(dt)[which(sadfaces)]
  ignoreMe
}

transformData <- function(dt, vecColNamesToIgnore, fnSummarizeNumeric = function(x) {mean(x, na.rm = TRUE)}) {
  
    tmpDt <- dt#[, which(colnames(dt) %in% vecColNamesToIgnore) := NULL, with = FALSE]
    #print(str(dt))
    classByWindow <- data.frame(window = tmpDt$num_window, classe = tmpDt$classe)
    tmpDt <- group_by(tmpDt, num_window)

    classByWindow <- group_by(classByWindow, window) %>%
        summarise(classe = first(classe))

    tmpDt <- summarise_each(tmpDt,funs(meanNumerics(.)), -classe)
    print("passed summarise each")
    print(tmpDt$classe)
   
    tmpDt[, classe := classByWindow$classe]
    print(length(unique(tmpDt$classe)))
    vecColNamesToIgnore <- makeIgnorableColumns(tmpDt)
    
    tmpDt <- tmpDt[, which(colnames(tmpDt) %in% vecColNamesToIgnore) := NULL, with = FALSE]
    
    sadFaces <- makeIgnorableColumns(tmpDt)
    print(sadFaces)
    
    tmpDt
    
}


doTransform <- function() {
  colNamesToIgnore <- makeIgnorableColumns(trainSet)
  result <- transformData(trainSet, colNamesToIgnore)
  
  result
}

doTestTransform <- function() {
  colNamesToIgnore <- makeIgnorableColumns(trainSet)
  result <- transformData(testSet, colNamesToIgnore)
  
  result
}
#testTemp <- doTestTransform()
trainTemp <- doTransform()

completeTrain <- trainTemp[complete.cases(trainTemp),,]
allNumbers <- apply(completeTrain, 2, function(x) { all(is.numeric(x))})
#print(allNumbers)

trainDat <- caret::createDataPartition(completeTrain$classe, p = 0.25)
crossValidateSet <- completeTrain[trainDat[[1]],,]
modelTrainerSet <- completeTrain[-trainDat[[1]],,]

windowIds <- completeTrain$num_window
completeTrain[, num_window := NULL]

mod <- caret::train(classe ~ ., data = completeTrain, method="rpart")
rattle::fancyRpartPlot(mod$finalModel)
#rpart::printcp(mod$finalModel)
#mod$finalModel <- randomForest::randomForest(classe ~ ., data = modelTrainerSet)

predictions <- predict(mod$finalModel, newdata = modelTrainerSet, type = "class")
perf <- ROCR::performance(predictions, "tpr", "fpr")
plot(perf,col="black",lty=3, lwd=3)
#rattle::fancyRpartPlot(mod$finalModel)
beepr::beep()


