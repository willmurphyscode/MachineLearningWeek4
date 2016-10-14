countSwitches <- function(vec) {
  if(!is.numeric(vec)) {
    return(NA)
  }
  offsetVec <- c(0, vec)
  otherVec <- c(vec, 0)
  deltas <- offsetVec - otherVec

  signs <- deltas >= 0

  on <- signs[1]
  
  print(deltas)
  print(signs)
  
  switches <- 0
  for(i in 1:(length(deltas) - 1)) {

    if(on != signs[i]) {
      on <- ! on
      switches <- switches + 1
    }
  }
  
  switches
  
}

#test case
# oneSwitch <- c(1,2,3,3,2,1)
# twoSwitches <- c(1,2,3,1,2,3,2,1,2)
# 
# print(countSwitches(oneSwitch))
# print(countSwitches(twoSwitches))


buildWindowVector <-function (dt, startRowIx, endRowIx) {
  print(class(dt))
  print(startRowIx)
  source <- dt[startRowIx : endRowIx]

  mins <- apply(source, 2, function(x) {
      if(is.numeric(x)) {
        min(x)
        } 
    else {
      NA}
    })
  maxes <- apply(source, 2, function(x) {
    if(is.numeric(x)) {
      max(x)
    } 
    else {
      NA}
  })
  switches <- apply(source, 2, countSwitches)
  
  c(mins,maxes,switches)
}

buildModels <- function(dt) {
  windowVec <- dt$new_window == "yes"

  ixesWindowChanges <- which(dt$new_window == "yes")
  print(head(ixesWindowChanges))
  print(class(dt))
  models <- buildWindowVector(dt, 1, ixesWindowChanges[1])
  
  for(i in 2:(length(ixesWindowChanges) - 1)) {
    models <- cbind(models, buildWindowVector(dt, ixesWindowChanges[i], ixesWindowChanges[i + 1]))
  }
  models
  
}



