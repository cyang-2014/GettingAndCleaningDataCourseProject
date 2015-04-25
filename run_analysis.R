## getdata course project

# Values of Varible Activity consist of data from “Y_train.txt” and “Y_test.txt”
# values of Varible Subject consist of data from “subject_train.txt” and subject_test.txt"
# Values of Varibles Features consist of data from “X_train.txt” and “X_test.txt”
# Names of Varibles Features come from “features.txt”
# levels of Varible Activity come from “activity_labels.txt”

# 1. Merges the training and the test sets to create one data set.
subject_test <- read.table("data/getData/courseProj/UCI HAR Dataset/test/subject_test.txt")
Xfeature_test <- read.table("data/getData/courseProj/UCI HAR Dataset/test/X_test.txt")
yActivity_test <- read.table("data/getData/courseProj/UCI HAR Dataset/test/y_test.txt")

subject_train <- read.table("data/getData/courseProj/UCI HAR Dataset/train/subject_train.txt")
Xfeature_train <- read.table("data/getData/courseProj/UCI HAR Dataset/train/X_train.txt")
yActivity_train <- read.table("data/getData/courseProj/UCI HAR Dataset/train/y_train.txt")

## check each data.frame's structure
# str(yActivity_test)
# str(yActivity_train)
# str(subject_test)
# str(subject_train)
# str(Xfeature_test)
# str(Xfeature_train)

## 1.1 concatenate the test & train data.frames by rows for each set
dataSubject <- rbind(subject_test, subject_train)
dataFeature <- rbind(Xfeature_test, Xfeature_train)
dataActivity <- rbind(yActivity_test, yActivity_train)

## 1.2 rename each set
colnames(dataSubject) <- "subject"
colnames(dataActivity) <- "activity"
 ## read in feature names from features.txt file
dataFeature.name <- as.character(read.table("data/getData/courseProj/UCI HAR Dataset/features.txt")$V2)  ## 561
colnames(dataFeature) <- dataFeature.name

## 1.3 merge columns together for data.whole from all three sets
data.whole <- cbind(dataSubject, dataActivity, dataFeature)   ## 10299   563

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## 2.1 Subset Name of Features by measurements on the mean and standard deviation
FeatureNames.mean_std <- dataFeature.name[grep("mean\\(\\)|std\\(\\)", dataFeature.name)]  ## 66
## 2.2 Subset the data.frame data.whole by seleted names of Features
whole.mean_std <- c("subject", "activity", FeatureNames.mean_std)
data.whole.mean_std <- data.whole[, whole.mean_std]
## 2.3 Check the structures of the data.frame data.whole
dim(data.whole.mean_std)   ## 10299    68

# 3. Uses descriptive activity names to name the activities in the data set
## 3.1 read in descriptive activity names from activity_labels.txt file
activityDescription <- read.table("data/getData/courseProj/UCI HAR Dataset/activity_labels.txt")
## 3.2 set the matching activity label for each row aka check corresponding activity names
SetActivityNames <- function(df){
    activity.ID <- 1
    for (label in activityDescription$V2) {
        df$activity <- gsub(activity.ID, label, df$activity)
        activity.ID <- activity.ID + 1
    }
    return(df)
}
data.whole.mean_std.newLable <- SetActivityNames(data.whole.mean_std)


# 4. Appropriately labels the data set with descriptive variable names. 
# In the former part, names of the activities have been labelled using descriptive names.
# In this part, Names of Feteatures will labelled using descriptive variable names.
colnames(data.whole.mean_std.newLable) <- gsub("^t", "time", colnames(data.whole.mean_std.newLable)) # prefix t is replaced by time
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

# 5. From the data set in step 4, creates a second, independent tidy data set
library(plyr)
data.whole.mean_std.newLable.tidy <- aggregate(. ~subject + activity, data.whole.mean_std.newLable, mean)
data.whole.mean_std.newLable.tidy <- data.whole.mean_std.newLable.tidy[order(data.whole.mean_std.newLable.tidy$subject, 
                                                                             data.whole.mean_std.newLable.tidy$activity), ]
write.table(data.whole.mean_std.newLable.tidy, file = "reports/independentTidyData.txt", row.name = FALSE)


## Produce CodeBook

library(knitr)
knit2html("code/clean_data/courseProject/codebook.Rmd")
