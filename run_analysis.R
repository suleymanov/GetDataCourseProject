# This is "Getting and cleaning data" course project script.
# Author: Rail Suleymanov (Moscow, Russia)
# Mail: rail.suleymanov@gmail.com
# Date: 23rd November, 2014

rm(list = ls())

proj_dir <- paste(getwd(), "/getdata/UCI HAR Dataset", sep = "")
features <- read.table(paste(proj_dir, "/features.txt", sep = ""), header = F)
activity_type <- read.table(paste(proj_dir, "/activity_labels.txt", sep = ""), header = F)
subject_train <- read.table(paste(proj_dir, "/train/subject_train.txt", sep = ""), header = F)
x_train <- read.table(paste(proj_dir, "/train/x_train.txt", sep = ""), header = F)
y_train <- read.table(paste(proj_dir, "/train/y_train.txt", sep = ""), header = F)

colnames(activity_type) <- c("activityId", "activityType")
colnames(subject_train) <- "subjectId"
colnames(x_train) <- features[, 2]
colnames(y_train) <- "activityId"

training_data <- cbind(y_train, subject_train, x_train)

subject_test <- read.table(paste(proj_dir, "/test/subject_test.txt", sep = ""), header = F)
x_test <- read.table(paste(proj_dir, "/test/x_test.txt", sep = ""), header = F)
y_test <- read.table(paste(proj_dir, "/test/y_test.txt", sep = ""), header = F)

colnames(subject_test) <- "subjectId"
colnames(x_test) <- features[, 2]
colnames(y_test) <- "activityId"

test_data <- cbind(y_test, subject_test, x_test)
final_data <- rbind(training_data, test_data)
colNames <- colnames(final_data)

log_expr <- (grepl("activity..", colNames) |
             grepl("subject..", colNames) |
             grepl("-mean..", colNames) &
             !grepl("-meanFreq..", colNames) &
             !grepl("mean..-", colNames) |
             grepl("-std..", colNames) &
             !grepl("-std()..-", colNames))

final_data <- final_data[log_expr == T]
final_data <- merge(final_data, activity_type, by = "activityId", all.x = T)

colNames <- colnames(final_data)

for (i in 1:length(colNames)) 
{
    colNames[i] <- gsub("\\()", "", colNames[i])
    colNames[i] <- gsub("-std$", "StdDev", colNames[i])
    colNames[i] <- gsub("-mean", "Mean", colNames[i])
    colNames[i] <- gsub("^(t)", "time", colNames[i])
    colNames[i] <- gsub("^(f)", "freq", colNames[i])
    colNames[i] <- gsub("([Gg]ravity)", "Gravity", colNames[i])
    colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)", "Body", colNames[i])
    colNames[i] <- gsub("[Gg]yro", "Gyro", colNames[i])
    colNames[i] <- gsub("AccMag", "AccMagnitude", colNames[i])
    colNames[i] <- gsub("([Bb]odyaccjerkmag)", "BodyAccJerkMagnitude", colNames[i])
    colNames[i] <- gsub("JerkMag", "JerkMagnitude", colNames[i])
    colNames[i] <- gsub("GyroMag", "GyroMagnitude", colNames[i])
}

colnames(final_data) <- colNames
final_data_no_activity_type <- final_data[, names(final_data) != "activityType"]
tidy_data <- aggregate(final_data_no_activity_type[, names(final_data_no_activity_type) != 
                       c("activityId", "subjectId")], by = 
                       list(activityId = final_data_no_activity_type$activityId, 
                       subjectId = final_data_no_activity_type$subjectId), mean)
tidy_data <- merge(tidy_data,activity_type, by = "activityId", all.x = T)
write.table(tidy_data, "tidy_data.txt", row.names = T, sep = "\t")