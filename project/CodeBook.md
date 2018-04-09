# Getting and Cleaning Data Course Project

## Overall flow description

1.- Calculate all the filepaths for both train and test data sets.
2.- Extract all the variable names from features.txt
3.- Load all the activity labels from activity_labels.txt
4.- Load, properly format and create a single data frame for the train data set based on the list of columns of interest(std and mean ones)
5.- Load, properly format and create a single data frame for the test data set based on the list of columns of interest(std and mean ones)
6.- Merge both data frames created in step 4 and 5.
7.- Generate the summary analysis by filtering the data per subject and per activity in order to generate the final_data_set.csv file at the source data location.
