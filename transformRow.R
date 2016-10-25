#transformRow.R
require(dplyr)

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
          summarise_each(funs(meanNumerics(.)))

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

mod <- caret::train(classe ~ ., data = modelTrainerSet, method="rf")

predictions <- predict(mod$finalModel, crossValidateSet)
perf <- ROCR::performance(predictions, "tpr", "fpr")
plot(perf,col="black",lty=3, lwd=3)
#rattle::fancyRpartPlot(mod$finalModel)
beepr::beep()


