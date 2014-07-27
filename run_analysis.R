# ============================================================================
#          Getting and Cleaning Data Course Project
# ============================================================================
# Author:      Sveinbjorn Gestsson (yngvi@icloud.com)
# Create date: 7/21/2014
#
# Description: The purpose of this project is to demonstrate your ability
#              to collect, work with, and clean a data set. The goal is
#              to prepare tidy data that can be used for later analysis.
#              You will be graded by your peers on a series of yes/no
#              questions related to the project.
#
# Notes:       You will be required to submit:
#              1) a tidy data set as described below, 
#              2) a link to a Github repository with your script for
#                 performing the analysis, and
#              3) a codebook that describes the variables, the data,
#                 the data, and any transformations or work that you
#                 performed to clean up the data called CodeBook.md.
#              You should also include a README.md in the repo with your
#              scripts. This repo explains how all of the scripts work
#              and how they are connected,
# ============================================================================
#                          Loading libraries
# ============================================================================
library(data.table)
library(reshape2)

# ============================================================================
#                          Loading functions
# ============================================================================
# Search function for seperating features in part 4.
finger <- function(regex) {
        grepl(regex, data.dt$feature.factor)
}

# ============================================================================
#                     Loading and cleaning data
# ===========================================================================
# download and unzip data.
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# Use method = "curl" in Mac
download.file(fileUrl, destfile = "gacdproject.zip", method = "curl")
unzip("gacdproject.zip")

# Get working directory
path <- getwd()

# Get file path and list files in directories.
path.in <- file.path(path, "UCI HAR Dataset")

# ============================================================================
#    1. Merges the training and the test sets to create one data set.
# ============================================================================

# Read the data files.

test <- read.table(file.path(path.in, "test", "x_test.txt"))
train <- read.table(file.path(path.in, "train", "x_train.txt"))
# bind together and convert to data.table.
joinData <- rbind(train, test)
joinData.dt <- data.table(joinData)

# Read the subject files.
subject.test <- fread(file.path(path.in, "test", "subject_test.txt"))
subject.train <- fread(file.path(path.in, "train", "subject_train.txt"))

# Then the activity files.
label.test <- fread(file.path(path.in, "test", "y_test.txt"))
label.train <- fread(file.path(path.in, "train", "y_train.txt"))

# Bind together the rows in the data tables.
subject.dt <- rbind(subject.train, subject.test)
activity.dt <- rbind(label.train, label.test)

# Name the columns
setnames(subject.dt, "V1", "Subject")
setnames(activity.dt, "V1", "Activity")

# Bind the columns together.
subject_activity.dt <- cbind(subject.dt, activity.dt)
data.dt <- cbind(subject_activity.dt, joinData.dt)

# Remove reduntant data tables.
rm(subject.train, subject.test, label.train, label.test, train, test, subject.dt, activity.dt, joinData.dt, joinData, subject_activity.dt)

# Set key for data table.
setkey(data.dt, Subject, Activity)

# Save rda file.
#save(data.dt, file="combined_data.rda")

# ============================================================================
#    2. Extracts only the measurements on the mean and standard deviation
#       for each measurement.
# ============================================================================

# Next step is to extract info about m and std from features.txt.
features.dt <- fread(file.path(path.in, "features.txt"))
setnames(features.dt, names(features.dt), c("feature.number", "feature.name"))

# Extract m and std from variable feature.name and subset it.
features.dt <- features.dt[grepl("mean\\(\\)|std\\(\\)", feature.name)]

# Convert feature.number to mach columns in data.dt.
features.dt$feature.code <- features.dt[, paste0("V", feature.number)]

# Subset data.dt based on feature.code.
selected.data <- c(key(data.dt), features.dt$feature.code)
data.dt <- data.dt[, selected.data, with=FALSE]
rm(selected.data)

# ============================================================================
#    3. Uses descriptive activity names to name the activities in
#       the data set.
# ============================================================================

# Load activity_labels file for descriptive names.
activity.names.dt <- fread(file.path(path.in, "activity_labels.txt"))
setnames(activity.names.dt, names(activity.names.dt), c("Activity", "activity.name"))

# Add activity description.
data.dt <- merge(data.dt, activity.names.dt, by="Activity", all.x=TRUE)
rm(activity.names.dt)

# Set activity.name as key.
setkey(data.dt, Subject, Activity, activity.name)

# Melt data to narrow format.
data.dt <- data.table(melt(data.dt, key(data.dt), variable.name="feature.code"))

# Merge data and features by feature.code.
data.dt <- merge(data.dt, features.dt[, list(feature.number, feature.code, feature.name)], by="feature.code", all.x=TRUE)
rm(features.dt)

# Create factor variables for activity and feature names.
data.dt$activity.factor <- factor(data.dt$activity.name)
data.dt$feature.factor <- factor(data.dt$feature.name)

# ============================================================================
#    4. Appropriately labels the data set with descriptive
#       variable names.
# ============================================================================
# Seperate features.

## Feature with 1 category
n <- 2
y <- matrix(seq(1, n), nrow=n)

data.dt$smartJerk <- factor(finger("Jerk"), labels=c(NA, "Jerk"))
data.dt$smartMagnitude <- factor(finger("Mag"), labels=c(NA, "Magnitude"))

## Features with 2 categories.

x <- matrix(c(finger("^t"), finger("^f")), ncol=nrow(y))
data.dt$smartDomain <- factor(x %*% y, labels=c("Time", "Freq"))

x <- matrix(c(finger("Acc"), finger("Gyro")), ncol=nrow(y))
data.dt$smartInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))

x <- matrix(c(finger("BodyAcc"), finger("GravityAcc")), ncol=nrow(y))
data.dt$smartAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))

x <- matrix(c(finger("mean()"), finger("std()")), ncol=nrow(y))
data.dt$smartStatistic <- factor(x %*% y, labels=c("mean", "sd"))

## Features with 3 categories.
n <- 3
y <- matrix(seq(1, n), nrow=n)

x <- matrix(c(finger("-X"), finger("-Y"), finger("-Z")), ncol=nrow(y))
data.dt$smartAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

rm(x, y, n, finger)

# Create tidy dataset with all possible outputs of features.
setkey(data.dt, Subject, Activity, smartDomain, smartAcceleration, smartInstrument, smartJerk, smartMagnitude, smartStatistic, smartAxis)
tidydata.dt <- data.dt[, list(count = .N, average = mean(value)), by=key(data.dt)]

# Write dataset to disk.
write.table(tidydata.dt, "tidydata.txt")

# Create and save codebook.
capture.output(cat("Structure of tidyData", "\n"), file="codebook.txt")
capture.output(str(tidydata.dt), file="codebook.txt", append=TRUE)
capture.output(cat("-------------------------------------------", "\n"), file="codebook.txt", append=TRUE)
capture.output(cat("Summary", "\n"), file="codebook.txt", append=TRUE)
capture.output(summary(tidydata.dt), file="codebook.txt", append=TRUE)
capture.output(cat("-------------------------------------------", "\n"), file="codebook.txt", append=TRUE)
capture.output(cat("Possible outputs of features", "\n"), file="codebook.txt", append=TRUE)
capture.output(tidydata.dt[, .N, by=c(names(tidydata.dt)[grep("^smart", names(tidydata.dt))])], file="codebook.txt", append=TRUE)

