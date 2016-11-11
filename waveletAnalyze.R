set.seed(12345)

.libPaths(c("D:/rlibs", .libPaths()))

require(data.table)
require(xts)
require(caret)
require(lubridate)

setwd("D:\\Projects\\MachineLearningWeek4")
#setwd("C:\\Users\\william.murphy\\MachineLearningWeek4")

source(".\\downloadData.R")

trainSet <- fread(trainFile)
testSet <- fread(testFile)


stopifnot(require(wavelets));

#the problem is now very specific:
#based on invoking wavelets::dwt() on a window
#create a new datatable with one row per window
#that rpart or randomForest can interpret

##TODO write a function that takes a reference to the trainSet
##and a num_window, and returns the dwt object for that function

##TODO write a function that transforms a vector of dwt results
##into a vector of doubles

##TODO dwt accepts a a vector or a dataframe. Should I just yank 
# the columns I want and dwt the whole thing? 

transformVec <- function(input) {
      result <- wavelets::dwt(as.double(input), n.levels=1)
      result <- mean(result@W$W1)
      result
}

transormation <- function(testOrTrain) {
      
      yb <- tapply(testOrTrain)

      result <- data.frame(
         

      )


}
tmpSet <- trainSet[ ,which( colnames(trainSet) %in% c("roll_belt", "yaw_belt", "pitch_belt",
                                    "roll_dumbbell", "pitch_dumbbell", "yaw_dumbbell",
                                    "gyros_arm_x", "gyros_arm_y", "gyros_arm_z", 
                                    "accel_arm_x", "accel_arm_y", "accel_arm_z",
                                    "magnet_arm_x", "magnet_arm_y", "magnet_arm_z",
                                    "roll_arm", "pitch_arm", "yaw_arm",
                                    "accel_belt_x", "accel_belt_y", "accel_belt_z",
                                    "gyros_belt_x", "gyros_belt_y", "gyros_belt_z",
                                    "magnet_belt_x", "magnet_belt_y", "magnet_belt_z",
                                    "accel_dumbbell_x", "accel_dumbbell_y", "accel_dumbbell_z",
                                    "gyros_dumbbell_x", "gyros_dumbbell_y", "gyros_dumbbell_z",
                                    "magnet_dumbbell_x", "magnet_dumbbell_y", "magnet_dumbbell_z",
                                    "gyros_forearm_x", "gyros_forearm_y", "gyros_forearm_z",
                                    "accel_forearm_x", "accel_forearm_y", "accel_forearm_z",
                                    "magnet_forearm_x", "magnet_forearm_y", "magnet_forearm_z", "num_window", "classe"

)), with=FALSE]



# c(roll_belt, yaw_belt, pitch_belt,
#                                     roll_dumbbell, pitch_dumbbell, yaw_dumbbell,
#                                     gyros_arm_x, gyros_arm_y, gyros_arm_z, 
#                                     accel_arm_x, accel_arm_y, accel_arm_z,
#                                     magnet_arm_x, magnet_arm_y, magnet_arm_z,
#                                     roll_arm, pitch_arm, yaw_arm,
#                                     accel_belt_x, accel_belt_y, accel_belt_z,
#                                     gyros_belt_x, gyros_belt_y, gyros_belt_z,
#                                     magnet_belt_x, magnet_belt_y, magnet_belt_z,
#                                     accel_dumbbell_x, accel_dumbbell_y, accel_dumbbell_z,
#                                     gyros_dumbbell_x, gyros_dumbbell_y, gyros_dumbbell_z,
#                                     magnet_dumbbell_x, magnet_dumbbell_y, magnet_dumbbell_z,
#                                     gyros_forearm_x, gyros_forearm_y, gyros_forearm_z,
#                                     accel_forearm_x, accel_forearm_y, accel_forearm_z,
#                                     magnet_forearm_x, magnet_forearm_y, magnet_forearm_z
#                                     )

makeWaveletVmean <- function(x, warnSingles = FALSE) { 
      if(length(unique(x)) > 1) {
           inner <- wavelets::dwt(as.double(x), filter="la8", n.levels = 1) 
           vmean = mean(inner@V$V1)
           as.double(vmean)
      }
      else {
        if(is.numeric(x[1])) {
              if(warnSingles) {
                    print("returning length 1 vector")
              }
           as.double( x[1])
        } else {
              if(warnSingles) {
                    print("returning length 1 vector")
              }
              x[1]
        }    
      }
}

makeWaveletWmean <- function(x) { 
      if(length(unique(x)) > 1) {
            inner <- wavelets::dwt(as.double(x), filter="la8", n.levels = 1) 
            wmean = mean(inner@W$W1)
            as.double(wmean)
      }
      else {
        if(is.numeric(x[1])) {
           as.double( x[1])
        } else {
              x[1]
        }    
      }
}

safeMean <- function(x) {
      if(is.numeric(x)) {
           result <- mean(x, na.rm = TRUE) #if this happens to work out to a whole number, data.table 
           as.double(result)               #will try to store an int in a C double[] and crash. 
      } else {
            if(is.numeric(x[1])) {
            as.double( x[1])
            } else {
                  x[1]
            } 
      }

}

safeMin <- function(x) {
      if(is.numeric(x)) {
            result <- min(x, na.rm = TRUE)
            as.double(result)
      } else {
            x[1]
      }

}

safeMax <- function(x) {
      if(is.numeric(x)) {
            result <- max(x, na.rm=TRUE)
            as.double(result)
      } else {
            x[1]
      }

}


resultDt <- tmpSet[, lapply(.SD, makeWaveletVmean), by = num_window ]
resultDt2 <- tmpSet[, lapply(.SD, makeWaveletWmean), by = num_window ]
resultDt2 <- resultDt2[, c("num_window", "classe") := NULL,]
colnames(resultDt2) <- paste("w_", colnames(resultDt2), sep="")
resultDt3 <- tmpSet[, lapply(.SD, safeMean), by = num_window ]
resultDt3 <- resultDt3[, c("num_window", "classe") := NULL, ]
colnames(resultDt3) <- paste("mean_", colnames(resultDt3), sep="")
resultDt4 <- tmpSet[, lapply(.SD, safeMin), by = num_window ]
resultDt4 <- resultDt4[, c("num_window", "classe") := NULL, ]
colnames(resultDt4) <- paste("min_", colnames(resultDt4), sep="")
resultDt5 <- tmpSet[, lapply(.SD, safeMax), by = num_window ]
resultDt5 <- resultDt5[, c("num_window", "classe") := NULL, ]
colnames(resultDt5) <- paste("max_", colnames(resultDt5), sep="")
resultDt <- cbind(resultDt, resultDt2, resultDt3, resultDt4, resultDt5)




validatorPartition <- caret::createDataPartition(resultDt$classe, 2, 0.75, list=FALSE)

initialTrainSet <- resultDt[validatorPartition[,1]]#, colnames(resultDt) , with=FALSE]
validateSet <- resultDt[-validatorPartition[,1]]#, colnames(resultDt) , with=FALSE]

mod <- caret::train(classe ~ ., data = initialTrainSet, method="rf", cp = 0.00001)
#rattle::fancyRpartPlot(mod$finalModel)


predictions <- predict(mod$finalModel, newdata = validateSet, type = "class")

correctPredictions <- predictions == validateSet$classe

percentCorrect <- (sum(correctPredictions) / length(correctPredictions)) * 100

print(percentCorrect) # this is out of sample prediction error. 

#DO QUIZ: 
testTmpDt <- testSet[ ,which( colnames(trainSet) %in% c("roll_belt", "yaw_belt", "pitch_belt",
                                    "roll_dumbbell", "pitch_dumbbell", "yaw_dumbbell",
                                    "gyros_arm_x", "gyros_arm_y", "gyros_arm_z", 
                                    "accel_arm_x", "accel_arm_y", "accel_arm_z",
                                    "magnet_arm_x", "magnet_arm_y", "magnet_arm_z",
                                    "roll_arm", "pitch_arm", "yaw_arm",
                                    "accel_belt_x", "accel_belt_y", "accel_belt_z",
                                    "gyros_belt_x", "gyros_belt_y", "gyros_belt_z",
                                    "magnet_belt_x", "magnet_belt_y", "magnet_belt_z",
                                    "accel_dumbbell_x", "accel_dumbbell_y", "accel_dumbbell_z",
                                    "gyros_dumbbell_x", "gyros_dumbbell_y", "gyros_dumbbell_z",
                                    "magnet_dumbbell_x", "magnet_dumbbell_y", "magnet_dumbbell_z",
                                    "gyros_forearm_x", "gyros_forearm_y", "gyros_forearm_z",
                                    "accel_forearm_x", "accel_forearm_y", "accel_forearm_z",
                                    "magnet_forearm_x", "magnet_forearm_y", "magnet_forearm_z", "num_window"

)), with=FALSE]

testDt <- testTmpDt[, lapply(.SD, function(x){ makeWaveletVmean(x, TRUE) } ), by = num_window ]
testDt2 <- testTmpDt[, lapply(.SD, makeWaveletWmean), by = num_window ]
testDt2 <- testDt2[, c("num_window") := NULL,]
colnames(testDt2) <- paste("w_", colnames(testDt2), sep="")
testDt3 <- testTmpDt[, lapply(.SD, safeMean), by = num_window ]
testDt3 <- testDt3[, c("num_window") := NULL,]
colnames(testDt3) <- paste("mean_", colnames(testDt3), sep="")
testDt4 <- testTmpDt[, lapply(.SD, safeMin), by = num_window ]
testDt4 <- testDt4[, c("num_window") := NULL,]
colnames(testDt4) <- paste("min_", colnames(testDt4), sep="")
testDt <- cbind(testDt, testDt2, testDt3, testDt4)
testDt5 <- testTmpDt[, lapply(.SD, safeMax), by = num_window ]
testDt5 <- testDt5[, c("num_window") := NULL,]
colnames(testDt5) <- paste("max_", colnames(testDt5), sep="")
testDt <- cbind(testDt, testDt2, testDt3, testDt4, testDt5)

print(setdiff(colnames(testDt), colnames(resultDt)))
print(setdiff(colnames(resultDt), colnames(testDt)))


testPredictions <- predict(mod$finalModel, newdata = testDt, type = "class")

for(i in 1:length(testPredictions)) {
      print(
            paste(i, ": ", testPredictions[i], sep="")
      )

}
print(paste("Percent correct was: ", percentCorrect, sep = ""))
beepr::beep()