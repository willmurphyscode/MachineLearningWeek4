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



trainTemp <- doTransform()

completeTrain <- trainTemp[complete.cases(trainTemp),,]

mod <- caret::train(classe ~ ., data = completeTrain, method="rpart")

rattle::fancyRpartPlot(mod$finalModel)

