#1. Merge Sets
XtrainSet <- read.table("UCI HAR Dataset/train/X_train.txt")
XtestSet <- read.table("UCI HAR Dataset/test/X_test.txt")
XcombSet <- rbind(XtrainSet, XtestSet)

SubjecttrainSet <- read.table("UCI HAR Dataset/train/subject_train.txt")
SubjecttestSet <- read.table("UCI HAR Dataset/test/subject_test.txt")
SubjectcombSet <- rbind(SubjecttrainSet, SubjecttestSet)

YtrainSet <- read.table("UCI HAR Dataset/train/Y_train.txt")
YtestSet <- read.table("UCI HAR Dataset/test/Y_test.txt")
YcombSet <- rbind(YtrainSet, YtestSet)

# 2. Extract only measures of mean and standard deviation for each measurement
featuresTable <- read.table("UCI HAR Dataset/features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", featuresTable[, 2])
XcombSet <- XcombSet[, indices_of_good_features]
names(XcombSet) <- featuresTable[indices_of_good_features, 2]
names(XcombSet) <- gsub("\\(|\\)", "", names(XcombSet))
names(XcombSet) <- tolower(names(XcombSet))

# 3. Uses descriptive activity names to name the activities in the data set.
activities <- read.table("UCI HAR Dataset/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
YcombSet[,1] = activities[YcombSet[,1], 2]
names(YcombSet) <- "activity"

# 4. labels the data set with descriptive activity names.
names(SubjectcombSet) <- "subject"
cleaned <- cbind(SubjectcombSet, YcombSet, XcombSet)
write.table(cleaned, "merged.txt")

# 5. tidy data set with the average of each variable for each activity and each subject.
uniqueSubjects = unique(SubjectcombSet)[,1]
numSubjects = length(unique(SubjectcombSet)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]
row = 1
for (s in 1:numSubjects) {
        for (a in 1:numActivities) {
                result[row, 1] = uniqueSubjects[s]
                result[row, 2] = activities[a, 2]
                tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
                result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
                row = row+1
        }
}
write.table(result, "averages.txt")