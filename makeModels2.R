completeTrain <- trainSet[complete.cases(trainSet),]
completeTrain$classe <- factor(completeTrain$classe)
only1 <- apply(trainSet, 2, function(x) { length(unique(x)) == 1})
delme <- (which(only1))
completeTrain <- completeTrain[,names(delme) := NULL]

trainSet2 <- trainSet[, names(delme) := NULL] 
trainMat <- model.matrix(classe ~ ., data = trainSet2)

mod <- caret::train(classe ~ ., data=trainSet2, 
                    method="rpart"
                    , na.action = na.omit, preProcess = "zv"
                    )
beepr::beep()
printcp(mod$finalModel) 
#rattle::printRandomForests(mod)
head(completeTrain)