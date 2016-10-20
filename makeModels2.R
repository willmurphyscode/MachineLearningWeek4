completeTrain <- trainSet[complete.cases(trainSet),]
completeTrain$classe <- factor(completeTrain$classe)
only1 <- apply(completeTrain, 2, function(x) { length(unique(x)) == 1})

completeTrain <- completeTrain[,which(only1) := NULL]

mod <- caret::train(classe ~ ., data=completeTrain, method="rpart")
#rattle::printRandomForests(mod)
head(completeTrain)