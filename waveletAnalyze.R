
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