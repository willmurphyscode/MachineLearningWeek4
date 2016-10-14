countSwitches <- function(v) {
#   if(!is.numeric(vec)) {
#     print(class(vec))
#     return(NA)
#   }
  
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
  
  switches
  
}

#test case
# oneSwitch <- c(1,2,3,3,2,1)
# twoSwitches <- c(1,2,3,1,2,3,2,1,2)
# 
# print(countSwitches(oneSwitch))
# print(countSwitches(twoSwitches))


buildWindowVector <-function (dt, startRowIx, endRowIx) {

  source <- dt[startRowIx : endRowIx,]
  classe <- dt[startRowIx : endRowIx, "classe"]
  if(any(classe != classe[0])) {
    stop("error, window crosses outcomes")
  }

  mins <- apply(source, 2, function(x) {
      if(is.numeric(x)) {
        min(x)
      } 
      
      else {
        min(as.numeric(x))
        }
    })
  maxes <- apply(source, 2, function(x) {
    if(is.numeric(x)) {
      max(x)
    } 
    else {
      max(as.numeric(x))
      }
  })
  switches <- apply(source, 2, countSwitches)
  
  c(mins,maxes,switches, classe[0])
}

buildModels <- function(dt) {
  deltaVec <- c(1, 1 + which(diff(as.numeric(factor(dt$user_name))) != 0))
 
  models <- buildWindowVector(dt, 1, deltaVec[1])
 
  for(i in 2:(length(deltaVec) - 1)) {
    models <- cbind(models, buildWindowVector(dt, deltaVec[i], deltaVec[i + 1]))
  }
  models
  
}



