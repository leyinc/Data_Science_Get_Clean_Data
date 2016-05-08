# this R script does the following:

# Merges the training and the test sets to create one data set.

# Extracts only the measurements on the mean and standard deviation for each measurement.

# Uses descriptive activity names to name the activities in the data set

# Appropriately labels the data set with descriptive activity names.

# Creates a second, independent tidy data set with the average of each variable for each activity and each subject.



get.data = function() {
  features             = read.table("features.txt", as.is = T)
  colnames(features)   = c("index", "name")
  feature.names        = features[,2]
  feature.names        [duplicated(feature.names)]
  #--561 values, but only 477 unique!
  #---84 duplicates
  
  list                 (feature.names = feature.names)
  dim                  (labels)
  
  #---------------
  # get train
  activity.numbers = read.table("train//y_train.txt", as.is = T)
  activity         = read.table("train//x_train.txt", as.is = T)
  subjects         = read.table("train//subject_train.txt")
  dim(activity.numbers)
  dim (activity)
  activity$activity.number = activity.numbers[,1]
  activity$subject = subjects[,1]
  head(activity)
  table (activity$subject, activity$activity)
  activity.train = activity
  
  #---------------------------
  # get test
  rm (activity)
  activity.numbers = read.table("test//y_test.txt", as.is = T)
  activity         = read.table("test//x_test.txt", as.is = T)
  subjects         = read.table("test//subject_test.txt")
  dim(activity.numbers)
  dim (activity)
  activity$activity.number = activity.numbers[,1]
  activity$subject = subjects[,1]
  head(activity)
  table (activity$subject, activity$activity)
  activity.test = activity
  
  #--------------------------------
  # combine train and test
  
  activity = rbind(activity.train, activity.test)
  print (table (activity$subject, activity$activity))
  colnames(activity)[1:561] = feature.names
  
  
  #-------------------------------
  #--toss extra columns
  
  mean.cols = grep("mean", feature.names)
  sd.cols   = grep("std",  feature.names)
  cols      = c(mean.cols, sd.cols)
  cols      = sort(cols)
  cols = c(563, 562, cols)
  
  activity = activity[,cols]
  head(activity)
  dim(activity)
  
  #--add activity names
  
  labels = read.table("activity_labels.txt")
  labels
  colnames(labels) = c("activity.number", "activity.name")
  activity2 = merge(activity, labels, by = activity.number, all = T)
  dim(activity2)
  table(activity2$activity.number, activity2$activity.name)
  head(activity2)
  
  activity2
  
}

transmogrify = function (data) {
  #--make a data set with rows for subjects and means for each measurement
  #--toss the parentheses in column names
  statistics  = NULL
  stats       = colnames(data)[3:81]
  activities  = sort(unique(data$activity.name))
  
  for (subject in sort(unique(data$subject)))
  {
    for (activity.name in activities)
    {
      local = data[data$subject == subject & data$activity.name == activity.name,]
      m = colMeans(local[3:81])
      row = as.data.frame(t(m))
      row$subject = subject
      row$activity.name = activity.name
      statistics = rbind(statistics, row)
    }
  }
  names = colnames(statistics)
  names = gsub("\\(\\)", "", names)
  colnames(statistics) = names
  ncols = dim(statistics)[2]
  order = c(ncols-1, ncols, 1:(ncols-2))
  statistics = statistics[,order]
  statistics
}





setwd("c://get_clean_data//data")

data = get.data()
table(data$subject, data$activity.name)
statistics = transmogrify(data)
head(statistics)
tail(statistics.tidy)
write.csv (statistics, "tidy_means.csv", row.names = F)