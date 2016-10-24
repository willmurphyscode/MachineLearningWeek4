#explore.R

#the goal of this function is to plot trends on different windows.
plotTrend <- function(dt, ixColOfInterest, rowsOfInterest) {
  yvec <-  dt[[ixColOfInterest]][rowsOfInterest]
  #print(length(yvec))
  plot(x = rowsOfInterest, y = yvec)
}


