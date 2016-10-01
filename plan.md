
I need to make a plan for doing this homework.

Main question: predict what people exercise is being completed 
based on accelerometer data from different sensors

Technical difficulties:

- many values are null
- dataset is too large to be ready entirely into main memory // I think this is not true. I think I was parsing it badly before. 

This consideration leads to a sub problem: I need to select the subset of columns
that permit me to make good predictions.

This consideration leads me to a smaller sub-problem: I need to test whether a column,
or a computed column, is a good test of the data


Ideas:
------

1. Count direction switches within windows
2. Compare mins and maxes within windows
3. Compute other summary statistics of the different columns. 
    - 

Those two are pretty good. Probably enough to go on if I can get R to do that. 




