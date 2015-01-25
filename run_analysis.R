
# Getting and cleaning data. Project Course

#You should create one R script called run_analysis.R that does the following. 

#Merges the training and the test sets to create one data set.

temp1 <- read.table("train/X_train.txt") 
temp2 <- read.table("test/X_test.txt")
x <- rbind(temp1, temp2)

temp1 <- read.table("train/subject_train.txt")
temp2 <- read.table("test/subject_test.txt")
subject <- rbind(temp1, temp2)

temp1 <- read.table("train/y_train.txt")
temp2 <- read.table("test/y_test.txt")
y <- rbind(temp1, temp2)

#Extracts only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("features.txt")
indices <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
x <- x[, indices]
names(x) <- features[indices, 2]
names(x) <- gsub("\\(|\\)", "", names(x))
names(x) <- tolower(names(x))

#Uses descriptive activity names to name the activities in the data set

act <- read.table("activity_labels.txt")
act[, 2] = gsub("_", "", tolower(as.character(act[, 2])))
y[,1] = act[y[,1], 2]
names(y) <- "activity"

#Appropriately labels the data set with descriptive variable names. 

names(subject) <- "subject"
data <- cbind(subject, y, x)
write.table(, "merged_data.txt")

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(subject)[,1]
numSubjects = length(unique(subject)[,1])
numActivities = length(act[,1])
numCols = dim(data)[2]
result = data[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = act[a, 2]
    tmp <- data[data$subject==s & data$activity==act[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "dataset_with_averages.txt",row.name=FALSE)
