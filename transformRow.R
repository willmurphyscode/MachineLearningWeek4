#transformRow.R
require(dplyr)
set.seed(8675309)
#I can just use the mean(column, na-rm = TRUE) to remove things
#the columns that are maxes will be fine, since they have only one 
#observations per window, and the data that is observed continuously
#will be transformed.



meanNumerics <- function(input) {
  result <- NULL
  if(is.numeric(input)) {
    result <- mean(input, na.rm = TRUE)
  } else {
    result <- input[1]
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
    return(NA)
  }
  
  offsetVec <- c(0, vec)
  otherVec <- c(vec, 0)
  deltas <- offsetVec - otherVec
  
  signs <- deltas >= 0
  
  on <- signs[1]
  
  #   print(deltas)
  #   print(signs)
  
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
  sadfaces <- apply(dt, 2 , function(x) { all(is.na(x))})
  
  ignoreMe <- colnames(dt)[which(sadfaces)]
  ignoreMe
}

transformData <- function(dt, vecColNamesToIgnore, fnSummarizeNumeric = function(x) {mean(x, na.rm = TRUE)}) {
    tmpDt <- dt#[, which(colnames(dt) %in% vecColNamesToIgnore) := NULL, with = FALSE]
    #print(str(dt))

    tmpDt <-tmpDt %>% 
        group_by(num_window) %>%
          summarise_each(funs(meanNumerics(.), countSwitches(.)))

    tmpDt <- tmpDt[, which(colnames(tmpDt) %in% vecColNamesToIgnore) := NULL, with = FALSE]
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
testTemp <- doTestTransform()
trainTemp <- doTransform()

completeTrain <- trainTemp[complete.cases(trainTemp),,]

trainDat <- caret::createDataPartition(completeTrain$classe, p = 0.25)
crossValidateSet <- completeTrain[trainDat[[1]],,]
modelTrainerSet <- completeTrain[-trainDat[[1]],,]

mod <- caret::train(classe ~ ., data = modelTrainerSet, method="rpart")
rattle::fancyRpartPlot(mod$finalModel)
rpart::printcp(mod$finalModel)
predictions <- predict(mod$finalModel, crossValidateSet, type = "class")
perf <- ROCR::performance(predictions, "tpr", "fpr")
plot(perf,col="black",lty=3, lwd=3)
#rattle::fancyRpartPlot(mod$finalModel)
beepr::beep()


