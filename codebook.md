## Codebook.md for tidyData.

This files describes the variables, the data, and any transformations for cleaning the data.

The data came from this website:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Link to the project:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

## The following has been done:

* Datasets merged and variables for mean and std extracted.
* Descriptive activity names used to name ehe activities in the dataset.
* Dataset melted to narrow format by features.
* Data labelled appropriately with descriptive variable names.
* Created tidy dataset with all possible outputs of features.

List of variables in tidy dataset:

* Subject - ID of participant (from 1 to 30).
* Activity - Name of activity (Walking, laying, etc).
* smartDomain - Two domains of smartphone features, time or frequency.
* smartInstrument - Instrument (Accelerometer or Gyroscope).
* smartAcceleration - Signal (Body or Gravity).
* smartStatistics - mean or standard deviation.
* smartJerk - Signal.
* smartMagnitude - Magnitude of the signals.
* smartAxis - X, Y, Z axis signals.
* count - count of data points.
* average - Variable average for each activity and each subject.

Structure and summary for tidydataset is in codebook.txt.
