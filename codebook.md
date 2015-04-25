---
title: "run_analysis_getData_courseProj"
author: "Chengran"
date: "April 24, 2015"
output: html_document
---

# Merges the training and the test sets to create one data set.


```r
setwd("~/USA/JD_Lab/projects/data_science_jhu")
subject_test <- read.table("data/getData/courseProj/UCI HAR Dataset/test/subject_test.txt")
Xfeature_test <- read.table("data/getData/courseProj/UCI HAR Dataset/test/X_test.txt")
yActivity_test <- read.table("data/getData/courseProj/UCI HAR Dataset/test/y_test.txt")

subject_train <- read.table("data/getData/courseProj/UCI HAR Dataset/train/subject_train.txt")
Xfeature_train <- read.table("data/getData/courseProj/UCI HAR Dataset/train/X_train.txt")
yActivity_train <- read.table("data/getData/courseProj/UCI HAR Dataset/train/y_train.txt")
```

## concatenate the test & train data.frames by rows for each set


```r
setwd("~/USA/JD_Lab/projects/data_science_jhu")
dataSubject <- rbind(subject_test, subject_train)
dataFeature <- rbind(Xfeature_test, Xfeature_train)
dataActivity <- rbind(yActivity_test, yActivity_train)
```

## rename each set


```r
setwd("~/USA/JD_Lab/projects/data_science_jhu")
colnames(dataSubject) <- "subject"
colnames(dataActivity) <- "activity"
 ## read in feature names from features.txt file
dataFeature.name <- as.character(read.table("data/getData/courseProj/UCI HAR Dataset/features.txt")$V2) 
colnames(dataFeature) <- dataFeature.name
```

## merge columns together for data.whole from all three sets


```r
setwd("~/USA/JD_Lab/projects/data_science_jhu")
data.whole <- cbind(dataSubject, dataActivity, dataFeature)
```

# Extracts only the measurements on the mean and standard deviation for each measurement. 

## Subset Name of Features by measurements on the mean and standard deviation


```r
setwd("~/USA/JD_Lab/projects/data_science_jhu")
FeatureNames.mean_std <- dataFeature.name[grep("mean\\(\\)|std\\(\\)", dataFeature.name)] 
```

## Subset the data.frame data.whole by seleted names of Features


```r
setwd("~/USA/JD_Lab/projects/data_science_jhu")
whole.mean_std <- c("subject", "activity", FeatureNames.mean_std)
data.whole.mean_std <- data.whole[, whole.mean_std]
```

## Check the structures of the data.frame data.whole

```r
setwd("~/USA/JD_Lab/projects/data_science_jhu")
dim(data.whole.mean_std) 
```

```
## [1] 10299    68
```

# Uses descriptive activity names to name the activities in the data set
## read in descriptive activity names from activity_labels.txt file

```r
setwd("~/USA/JD_Lab/projects/data_science_jhu")
activityDescription <- read.table("data/getData/courseProj/UCI HAR Dataset/activity_labels.txt") 
```

## set the matching activity label for each row aka check corresponding activity names

```r
setwd("~/USA/JD_Lab/projects/data_science_jhu")
SetActivityNames <- function(df){
    activity.ID <- 1
    for (label in activityDescription$V2) {
        df$activity <- gsub(activity.ID, label, df$activity)
        activity.ID <- activity.ID + 1
    }
    return(df)
}
data.whole.mean_std.newLable <- SetActivityNames(data.whole.mean_std)
```

# Appropriately labels the data set with descriptive variable names. 

```r
setwd("~/USA/JD_Lab/projects/data_science_jhu")

colnames(data.whole.mean_std.newLable) <- gsub("^t", "time", 
colnames(data.whole.mean_std.newLable)) # prefix t is replaced by time
colnames(data.whole.mean_std.newLable) <- gsub("^f", "frequency", colnames(data.whole.mean_std.newLable)) # prefix f is replaced by frequency
colnames(data.whole.mean_std.newLable) <- gsub("Acc", "Accelerometer", colnames(data.whole.mean_std.newLable)) # Acc is replaced by Accelerometer
colnames(data.whole.mean_std.newLable) <- gsub("Gyro", "Gyroscope", colnames(data.whole.mean_std.newLable)) # Gyro is replaced by Gyroscope
colnames(data.whole.mean_std.newLable) <- gsub("Mag", "Magnitude", colnames(data.whole.mean_std.newLable)) # Mag is replaced by Magnitude
colnames(data.whole.mean_std.newLable) <- gsub("BodyBody", "Body", colnames(data.whole.mean_std.newLable)) # BodyBody is replaced by Body
colnames(data.whole.mean_std.newLable) <- gsub('-meanFreq()', '.mean.freq', colnames(data.whole.mean_std.newLable)) # substitutes "-meanFreq()" with ".mean.freq"
colnames(data.whole.mean_std.newLable) <- gsub('-mean()', '.mean', colnames(data.whole.mean_std.newLable)) # substitutes "-mean" with ".mean"
colnames(data.whole.mean_std.newLable) <- gsub('-std()', '.std', colnames(data.whole.mean_std.newLable)) # substitutes "-std" with ".std"
colnames(data.whole.mean_std.newLable) <- gsub('[-]', '.', colnames(data.whole.mean_std.newLable)) # substitutes "-" with "."
colnames(data.whole.mean_std.newLable) <- gsub('[()]', '', colnames(data.whole.mean_std.newLable)) # removes "()"
```

# From the data set in step 4, creates a second, independent tidy data set

```r
setwd("~/USA/JD_Lab/projects/data_science_jhu")
library(plyr)
data.whole.mean_std.newLable.tidy <- aggregate(. ~subject + activity, data.whole.mean_std.newLable, mean)
data.whole.mean_std.newLable.tidy <- data.whole.mean_std.newLable.tidy[order(data.whole.mean_std.newLable.tidy$subject,                                                             data.whole.mean_std.newLable.tidy$activity), ]
write.table(data.whole.mean_std.newLable.tidy, file = "reports/independentTidyData.txt", row.name = FALSE)
```
