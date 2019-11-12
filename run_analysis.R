## Create file to get and clean data from Samsung Galaxy S Smartphone

library(dplyr)
library(data.table)

# Read feature names and select names with "mean" and "std"

labs <- read.table("features.txt")
selectf <- grep("mean|std", labs[ ,2])

# Read training data

activity <- read.table("y_train.txt")
train_subject <- read.table("subject_train.txt")
train_data <- read.table("X_train.txt")
train_set <- cbind(train_subject, activity, train_data)

# Read test data

activity2 <- read.table("y_test.txt")
test_subject <- read.table("subject_test.txt")
test_data <- read.table("X_test.txt")
test_set <- cbind(test_subject, activity2, test_data)

# Combine training and test data set

bind_set <- rbind(train_set, test_set)
write.csv(bind_set, "train_test_dataset.csv")

# Select "mean" and "std" data columns only 
s_set <- bind_set[ , selectf+2]
data_set <- cbind(bind_set[ ,1], bind_set[ ,2], s_set)
write.csv(bind_set, "mean and std_dataset.csv")

# Uses descriptive activity names to name the activities

data_set$activity <- sub("1", "WALKING", data_set$activity)
data_set$activity <- sub("2", "WALKING_UPSTAIRS", data_set$activity)
data_set$activity <- sub("3", "WALKING_DOWNSTAIRS", data_set$activity)
data_set$activity <- sub("4", "SITTING", data_set$activity)
data_set$activity <- sub("5", "STANDING", data_set$activity)
data_set$activity <- sub("6", "LAYING", data_set$activity)

# Name the data_set columns. The description of column names stored in column names.csv

colnames(data_set)[1] <- "subject"
colnames(data_set)[2] <- "activity"
for(i in 1:79) {
  colnames(data_set)[i+2] <- paste("feature", as.character(i), sep = "")
}
namef <- cbind(colnames(data_set), c("subject", "activity", 
                                     as.character(labs[selectf, 2])))
write.csv(namef, "column names.csv", quote = FALSE)

# Create data set with average of features for each activity and each subject

data_df <- tbl_df(data_set) %>% group_by(subject, activity)
temp1 <- tapply(data_df[[3]],list(data_df$subject, data_df$activity), mean)
sub_act_mean <- melt(temp1)
for(i in 4:81) {
  temp <- tapply(data_df[[i]], list(data_df$subject, data_df$activity), mean)
  tempv <- melt(temp)
  sub_act_mean <- cbind(sub_act_mean, tempv[ ,3])
  }
colnames(sub_act_mean) <- colnames(data_df)

write.csv(sub_act_mean, "average_activity_subject.csv")

