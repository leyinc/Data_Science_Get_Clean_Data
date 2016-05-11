
library(reshape2)

## Set working directory
setwd("Data_Science_Get_Clean_Data/data/")

filename <- "getdata_dataset.zip"

## Download and unzip the dataset
if (!file.exists(filename)) {
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, destfile = filename)
}
if (!file.exists("UCI HAR Dataset")) {
    unzip(filename)
}

## Load activity labels and features
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
activityLabels[, 2] <- as.character(activityLabels[, 2])
features <- read.table("UCI HAR Dataset/features.txt")
features[, 2] <- as.character(features[, 2])

## Extract only the data on mean and std
featuresWanted <- grep("mean|std", features[, 2])
featuresWanted.names <- features[featuresWanted, 2]
featuresWanted.names <- gsub("-mean", "Mean", featuresWanted.names)
featuresWanted.names <- gsub("-std", "Std", featuresWanted.names)
featuresWanted.names <- gsub("[()-]", "", featuresWanted.names)

## Get train
train <- read.table("UCI HAR Dataset/train/X_train.txt")[, featuresWanted]
trainActivities <- read.table("UCI HAR Dataset/train/y_train.txt")
trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
train <- cbind(trainSubjects, trainActivities, train)

## Get test
test <- read.table("UCI HAR Dataset/test/X_test.txt")[, featuresWanted]
testActivities <- read.table("UCI HAR Dataset/test/y_test.txt")
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
test <- cbind(testSubjects, testActivities, test)

## Merge datasets and add labels
allData <- rbind(train, test)
colnames(allData) <- c("subject", "activity", featuresWanted.names)

## Turn activities and subjects into factors
allData$subject <- as.factor(allData$subject)
allData$activity <- factor(allData$activity, levels = activityLabels[, 1], labels = activityLabels[, 2])

allData.melted <- melt(allData, id = c("subject", "activity"))
allData.mean <- dcast(allData.melted, subject + activity ~ variable, mean)

## Write tidy data to txt for uploading convenience
write.table(allData.mean, "tidy_means.txt", row.names = FALSE, quote = FALSE)
