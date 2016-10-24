#transformRow.R
require(dplyr)

#I can just use the mean(column, na-rm = TRUE) to remove things
#the columns that are maxes will be fine, since they have only one 
#observations per window, and the data that is observed continuously
#will be transformed.

transformData <- function(dt, vecColNamesToIgnore, fnSummarizeNumeric = function(x) {mean(x, na.rm = TRUE)}) {
    tmpDt <- dt#[, which(colnames(dt) %in% vecColNamesToIgnore) := NULL, with = FALSE]
    print(str(dt))

    tmpDt <-tmpDt %>% 
        group_by(tmpDt$num_window) 
        %>% summarise_each(funs(min, max))

    

}