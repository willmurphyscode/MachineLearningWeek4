#transformRow.R
require(dplyr)
set.seed(8675309)
#I can just use the mean(column, na-rm = TRUE) to remove things
#the columns that are maxes will be fine, since they have only one 
#observations per window, and the data that is observed continuously
#will be transformed.
stopifnot( require(wavelets))


meanWaveletCoefs <- function(in_put) {
  input <- as.numeric(in_put)
  result <- NULL
  if(is.numeric(input) && !any(is.na(input)) && length(input) > 1) {
    names(input) <- NULL
    result <- tryCatch({
      result <- wavelets::dwt(as.double(input), n.levels=1)
      result <- mean(result@filter@h ^ 2)
      if(is.na(result)) {
        stop("Would return NA")
      }
      result  
    }, error=function(cond) {
      
      print(input)
      print(cond)
      stop("would make NA")
      return(NA)
    }, warning=function(cond) {
       
      print(input)
      print(cond)
      stop("would make NA")
      return(NA)
    })
    
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
  sadfaces2 <- apply(dt, 2, function(x) { all(is.na(x)) })
  sadfaces3 <- apply(dt, 2, function(x) { any(x == "")})
  sadfaces <- unique(c(sadfaces, sadfaces2, sadfaces3))
  ignoreMe <- colnames(dt)[which(sadfaces)]
  print(paste("Ignoring: ", ignoreMe))
  ignoreMe
}

transformData <- function(dt, vecColNamesToIgnore, fnSummarizeNumeric = function(x) {mean(x, na.rm = TRUE)}) {
  
    tmpDt <- dt#[, which(colnames(dt) %in% vecColNamesToIgnore) := NULL, with = FALSE]
    #print(str(dt))
    classByWindow <- data.frame(window = tmpDt$num_window, classe = tmpDt$classe)
    tmpDt <- group_by(tmpDt, num_window)

    classByWindow <- group_by(classByWindow, window) %>%
        summarise(classe = first(classe))

    tmpDt <- summarise_each(tmpDt,funs(meanWaveletCoefs(.)), c(roll_belt, yaw_belt, pitch_belt,
                                    roll_dumbbell, pitch_dumbbell, yaw_dumbbell,
                                    gyros_arm_x, gyros_arm_y, gyros_arm_z, 
                                    accel_arm_x, accel_arm_y, accel_arm_z,
                                    magnet_arm_x, magnet_arm_y, magnet_arm_z,
                                    roll_arm, pitch_arm, yaw_arm,
                                    accel_belt_x, accel_belt_y, accel_belt_z,
                                    gyros_belt_x, gyros_belt_y, gyros_belt_z,
                                    magnet_belt_x, magnet_belt_y, magnet_belt_z,
                                    accel_dumbbell_x, accel_dumbbell_y, accel_dumbbell_z,
                                    gyros_dumbbell_x, gyros_dumbbell_y, gyros_dumbbell_z,
                                    magnet_dumbbell_x, magnet_dumbbell_y, magnet_dumbbell_z,
                                    gyros_forearm_x, gyros_forearm_y, gyros_forearm_z,
                                    accel_forearm_x, accel_forearm_y, accel_forearm_z,
                                    magnet_forearm_x, magnet_forearm_y, magnet_forearm_z
                                    ))
    print("passed summarise each")
    print(tmpDt$classe)
    print(str(tmpDt))
    
    tmpDt[, classe := classByWindow$classe]
    
    print("passed re-add classe")
   # print(length(unique(tmpDt$classe)))
  #  vecColNamesToIgnore <- makeIgnorableColumns(tmpDt)
  #  print(vecColNamesToIgnore)
   # tmpDt <- tmpDt[, which(colnames(tmpDt) %in% vecColNamesToIgnore) := NULL, with = FALSE]
    
    #sadFaces <- makeIgnorableColumns(tmpDt)
   # print(sadFaces)
    
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
print('passed all numbers')
#print(allNumbers)

trainDat <- caret::createDataPartition(completeTrain$classe, p = 0.25)
crossValidateSet <- completeTrain[trainDat[[1]],,]
modelTrainerSet <- completeTrain[-trainDat[[1]],,]



windowIds <- completeTrain$num_window
completeTrain[, num_window := NULL]

near_zero <- apply(completeTrain, 2, function(x) { var(x) < 0.01 })

percent_nzv <- mean(near_zero, na.rm = TRUE)

if(percent_nzv > 0.1) {
  print(percent_nzv)
  stop('too many variables with near zero variation')
}


mod <- caret::train(classe ~ ., data = completeTrain, method="rpart", cp = 0.00001)

print(paste("number of classes was: ", length(unique(completeTrain$classe))))

print(summary(mod$finalModel))

rattle::fancyRpartPlot(mod$finalModel)
#rpart::printcp(mod$finalModel)
#mod$finalModel <- randomForest::randomForest(classe ~ ., data = modelTrainerSet)

print('passed fancyplot')

predictions <- predict(mod$finalModel, newdata = modelTrainerSet, type = "class")
perf <- ROCR::performance(predictions, "tpr", "fpr")
plot(perf,col="black",lty=3, lwd=3)
#rattle::fancyRpartPlot(mod$finalModel)
beepr::beep()


