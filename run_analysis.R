## Create file to get and clean data from Samsung Galaxy S Smartphone

setwd("raw data")
library(dplyr)
library(data.table)

# Read feature names and select names with "mean" and "std"

labs <- read.table("features.txt")
selectf <- grep("mean|std", labs[ ,2])

# Read training data

subject <- read.table("subject_train.txt")
activity <- read.table("y_train.txt")
train_data <- read.table("X_train.txt", col.names = labs[ ,2])
train_set <- cbind(subject, activity, train_data)

# Read test data

subject2 <- read.table("subject_test.txt")
activity2 <- read.table("y_test.txt")
test_data <- read.table("X_test.txt", col.names = labs[ ,2])
test_set <- cbind(subject2, activity2, test_data)

# Combine training and test data set

bind_set <- rbind(train_set, test_set)
colnames(bind_set)[1] <- "subject"
colnames(bind_set)[2] <- "activity"

# Select "mean" and "std" data columns only 
s_set <- bind_set[ , selectf+2]
data_set <- cbind(bind_set[ ,1], bind_set[ ,2], s_set)
colnames(data_set)[1] <- "subject"
colnames(data_set)[2] <- "activity"

# Uses descriptive activity names to name the activities

data_set$activity <- sub("1", "WALKING", data_set$activity)
data_set$activity <- sub("2", "WALKING_UPSTAIRS", data_set$activity)
data_set$activity <- sub("3", "WALKING_DOWNSTAIRS", data_set$activity)
data_set$activity <- sub("4", "SITTING", data_set$activity)
data_set$activity <- sub("5", "STANDING", data_set$activity)
data_set$activity <- sub("6", "LAYING", data_set$activity)

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


# Write data frame
setwd <- "tidy data"
write.csv(bind_set, "train_test_dataset.csv")
write.csv(data_set, "mean and std_dataset.csv")
write.table(sub_act_mean, "tidy data set.txt", quote = FALSE, sep = " ")

