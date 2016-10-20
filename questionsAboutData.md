The Thinking for This: 
==================

1. `raw_timestamp_part_1` and `raw_timestamp_part_2` -- Why? What are these for? 
2. `new_window` -- this is a yes/no column that is usually no in the training set, and always no in the test set. What does it do?  


Problems With Running Analyses
-----------

It looks like in the training data set, the rows are grouped by type. For example, the "classe" variable for 
all 5580 of the first rows is "A". This is breaking my analysis, because the methods that caret is using to partition
the data create *subsets that all have the same outcome.*


Columns I know I should exclude:
-------------------

1. `user_name` --> this column is no good to include because it has 

Things I Think I Learned
-----------------

1. The change of user or class indicates a new session. Maybe? 