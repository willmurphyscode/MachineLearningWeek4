#explore.R

#the goal of this function is to plot trends on different windows.
plotTrend <- function(dt, ixColOfInterest, rowsOfInterest) {
  yvec <-  dt[[ixColOfInterest]][rowsOfInterest]
  #print(length(yvec))
  plot(x = rowsOfInterest, y = yvec)
}


plotColumnByWindowAndName <- function(dt, strColumnName, intWindowId) {
  ixColOfInterest <- which(colnames(dt) == strColumnName)[1]
  rowsOfInterest <- which(dt$num_window == intWindowId)
  plotTrend(dt, ixColOfInterest, rowsOfInterest)
}


