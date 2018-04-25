## Getting and Cleaning Data Assignment
#
#
#
##############################################################################
# Getting data
##############################################################################

# download zip file containing data
FileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
Filedata <- "UCI HAR Dataset.zip"

if (!file.exists(Filedata)) {
  download.file(FileUrl, Filedata, method="curl")
}

# unzip zip file containing data 
DataSet <- "UCI HAR Dataset"
if (!file.exists("UCI HAR Dataset")) {
  unzip(DataSet)
}

##############################################################################
# Reading Data
##############################################################################

# read training data and create one single Object that contains all information
trainingSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
trainingActivities <- read.table("UCI HAR Dataset/train/y_train.txt")
trainingValues <- read.table("UCI HAR Dataset/train/X_train.txt")
training <- cbind(trainingSubjects, trainingActivities, trainingValues)  

# read test data and create one single Object that contains all information
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
testActivities <- read.table("UCI HAR Dataset/test/y_test.txt")
testValues <- read.table("UCI HAR Dataset/test/X_test.txt")
test <- cbind(testSubjects, testActivities, testValues)

# read features as it is and  don't convert text labels to factors
features <- read.table("UCI HAR Dataset/features.txt", as.is = TRUE)

# read activity labels
activities <- read.table("UCI HAR Dataset/activity_labels.txt", as.is = TRUE)
colnames(activities) <- c("activityId", "activityLabel") 

######################################################################################
# Step 1 - Merge the training and the test sets to create one data set and add labels
######################################################################################

CompleteData <- rbind(training, test)
colnames(CompleteData) <- c("Subject", "Activity", features[,2])

##############################################################################
# Step 2 - Extract only the measurements on the mean and standard deviation
#          for each measurement
##############################################################################

# determine columns with measurements of means and standard deviation along with Subject and Activity 
Requiredcolumns <- grepl("Subject|Activity|mean|std", colnames(CompleteData))

# Get the required Data in a new variable
RequiredData <- CompleteData[ ,Requiredcolumns]

# Remove unwanted variables to clear memory
rm(trainingSubjects,trainingActivities,trainingValues, training,
   testSubjects,testActivities,testValues,test, CompleteData)

##############################################################################
# Step 3 - Use descriptive activity names to name the activities in the data
#          set
##############################################################################

# replace activity values with named factor levels
RequiredData$Activity <- factor(RequiredData$Activity, 
                                 levels = activities[, 1], labels = activities[, 2])


##############################################################################
# Step 4 - Appropriately label the data set with descriptive variable names
##############################################################################

# get column names
RequiredDataColunms <- colnames(RequiredData)

# remove special characters
RequiredDataColumns <- gsub("[\\(\\)-]", "", RequiredDataColunms)

# expand abbreviations and clean up names
RequiredDataColumns <- gsub("^f", "frequencyDomain", RequiredDataColumns)
RequiredDataColumns <- gsub("^t", "timeDomain", RequiredDataColumns)
RequiredDataColumns <- gsub("Acc", "Accelerometer", RequiredDataColumns)
RequiredDataColumns <- gsub("Gyro", "Gyroscope", RequiredDataColumns)
RequiredDataColumns <- gsub("Mag", "Magnitude", RequiredDataColumns)
RequiredDataColumns <- gsub("Freq", "Frequency", RequiredDataColumns)
RequiredDataColumns <- gsub("mean", "Mean", RequiredDataColumns)
RequiredDataColumns <- gsub("std", "StandardDeviation", RequiredDataColumns)

# correct typo
RequiredDataColumns <- gsub("BodyBody", "Body", RequiredDataColumns)

# use new labels as column names
colnames(RequiredData) <- RequiredDataColumns


##############################################################################
# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
##############################################################################

# group by subject and activity and summarise using mean

library(dplyr)
RequiredDataMeans <- RequiredData %>% 
  group_by(Subject, Activity) %>%
  summarize_all(funs(mean))

# output to file "tidy_data.txt"
write.table(RequiredDataMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
