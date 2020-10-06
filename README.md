# RProgramming
Course assignments for R Programming course

The run_analysis.R script does the following:

1. Imports text files (x_train, y_train,subject_train, x_test, y_test, subject_test)
2. Joins the x,y and subject files for both the train and test sets
3. Merges the train and test sets
4. Imports the feature file, and splits the data that originated in the x test and train files into the 561 features.
5. Captures the features that have mean or std in their title and selects only those columns
6. Imports the activity label file and merges with main data set, to bring activity description into data set.
7. Uses summarise and group by to find the mean of the fields captured in step 5, by activity and subject.
8. Exports file to 'courseoutcomefile.txt'
