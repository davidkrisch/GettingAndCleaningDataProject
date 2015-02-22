library(data.table)
library(dplyr)

prefix <- "UCI HAR Dataset"

# Function to read a file with data.table
readFile <- function(file) {
    data.table(read.table(file, stringsAsFactors=FALSE))
}

# File holding the names of the columns
features <- paste(prefix, "/features.txt", sep="")
colNames <- readFile(features)

# Activities in order 1:6
activities <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")

# Step 1 in the assignment!
# Read test data, add column names and prepend subject column
# 

# Read the files into a data.table 
test_X_file <- paste(prefix, "/test/X_test.txt", sep="")
test_Y_file <- paste(prefix, "/test/y_test.txt", sep="")
test_subject_file <- paste(prefix, "/test/subject_test.txt", sep="")
test_df <- readFile(test_X_file)
test_activity <- readFile(test_Y_file)
test_subject <- readFile(test_subject_file)

# Add Column names
colnames(test_df) <- colNames$V2  # We'll live with the warning here

# Convert activity to a factor
activity <- factor(test_activity$V1, 1:6, activities)
# Prepend the activity to the data.table
test_with_activity <- cbind(activity, test_df) 

# Set subject column name
setnames(test_subject, "V1", "subject")
# Prepend the subject to the data.table
test_with_subject <- cbind(test_subject, test_with_activity)

# Prepend the subject

#
# Read training data, add column names and prepend subject column
# 

# Read the files into a data.table 
train_X_file <- paste(prefix, "/train/X_train.txt", sep="")
train_Y_file <- paste(prefix, "/train/y_train.txt", sep="")
train_subject_file <- paste(prefix, "/train/subject_train.txt", sep="")
train_df <- readFile(train_X_file)
train_activity <- readFile(train_Y_file)
train_subject <- readFile(train_subject_file)

# Add Column names from features.txt
colnames(train_df) <- colNames$V2  # We'll live with the warning here


# Convert activity to a factor
activity <- factor(train_activity$V1, 1:6, activities)
# Prepend the activity to the data.table
train_with_activity <- cbind(activity, train_df) 

# Set subject column name
setnames(train_subject, "V1", "subject")
# Prepend the subject to the data.table
train_with_subject <- cbind(train_subject, train_with_activity)

#
# Combine the test & train data sets into a single data.frame
#

merged_data <- rbind(train_with_subject, test_with_subject)

# Step 2 in the assignment!
# Extract the columns containing mean and standard deviation for each measurement
#

# Select only the columns that contain "mean" or "std", and the subject and activity
cnames_to_grep <- colnames(merged_data)[3:length(colnames(merged_data))]
filter <- c(TRUE, TRUE, grepl("mean()", cnames_to_grep) | grepl("std()", cnames_to_grep))
mean_std <- merged_data[, filter, with=FALSE]
# mean_std has 81 columns

# Step 3 in the assignment
# Uses descriptive activity names to name the activities in the data set
#
# This was done by converting the activities to a factor before merging the two data sets

# Step 4 in the assignment!
# Appropriately labels the data set with descriptive variable names.
#
# This seems to be pretty good with the way the data stands.


# Step 5 in the assignment!
# From the data set in step 4, create a second, independent tidy data set with the average of 
# each variable for each activity and each subject.
#

# Group by activity & subject, then find the mean of each column for those groups
result <- mean_std %>% group_by(activity, subject) %>% summarise_each(funs(mean))

write.table(result, file="results.txt", row.name=FALSE)