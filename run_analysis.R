library(downloader)
url <-  ("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")  
download(url, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir='.')

# getting the list of files
path1 <- file.path(getwd(), "UCI HAR Dataset")
(files <- list.files(path1, recursive = TRUE))

# Reading the data

Subject.Train <- read.table(file.path(path1, "train", "subject_train.txt"))
Subject.Test  <-read.table(file.path(path1, "test", "subject_test.txt"))

Features.Train <- read.table(file.path(path1, "train", "X_train.txt"))
Features.Test  <- read.table(file.path(path1, "test", "X_test.txt"))

Activity.Train <-read.table(file.path(path1, "train", "y_train.txt"))
Activity.Test  <-read.table(file.path(path1, "test", "y_test.txt"))

Features.Names <- read.table(file.path(path1, "features.txt"))
dim(Features.Names) #looking at the dimensions

# taking a look of the properties/dimensions 
str(Subject.Train)
str(Subject.Test)
str(Measurements.Train)
str(Measurements.Test)
str(Activity.Train)
str(Activity.Test)

# merging the test and training datasets
Subject.Data <- rbind(Subject.Train, Subject.Test)
Features.Data <- rbind(Features.Train, Features.Test)
Activity.Data <- rbind(Activity.Train, Activity.Test)

#naming the variables
names(Subject.Data) <- "Subject"
names(Activity.Data) <- "Activity"
names(Features.Data) <- Features.Names$V2 # taking the names from features.txt 2nd col.

#merging the columns by activity type and subject
combined <- cbind(Subject.Data, Activity.Data)
#merging the measurements(features) and "combined" data frame
Dataset <- cbind(combined, Features.Data)

# EXTRACTING MEAN AND STANDART DEVIATION FOR EACH MEASUREMENT
List.Features <-Features.Names$V2[grep("mean\\(\\)|std\\(\\)",Features.Names$V2)] # creating the list
Dataset <- subset(Dataset, select=List.Features)
Dataset <- cbind(combined, Dataset) # adding "Subject" and "Activity" columns back
# DESCRIPTIVE ACTIVITY NAMES
Activity_Labels <- read.table(file.path(path1, "activity_labels.txt"), header = FALSE)
#converting Activity to Factors
Dataset$Activity <- as.factor(Dataset$Activity)
# renaming factors according to "activity_labels.txt"
library(tidyverse)
Dataset$Activity <- fct_recode(Dataset$Activity,
                              "WALKING" = "1",
                              "WALKING UPSTAIRS" = "2",
                              "WALKING DOWNSTAIRS" = "3",
                              "SITTING" = "4",
                              "STANDING" = "5",
                              "LAYING" = "6"
                              )

# APPROPRIATE DESCRIPTIVE VARIABLE NAMES
names(Dataset) <- gsub("^t", "Time", names(Dataset))
names(Dataset) <- gsub("^f", "Frequency", names(Dataset))
names(Dataset) <- gsub("Acc", "Acceleration", names(Dataset)) 
names(Dataset) <- gsub("Gyro", "Gyroscope", names(Dataset)) 
names(Dataset) <- gsub("Mag", "Magnitude", names(Dataset)) 

colnames(Dataset)[1:2] <- c("Subject","Activity")

#INDEPENDENT DATA SET WITH AVERAGE OF EACH VARIABLE AND EACH SUBJECT
Dataset2 <- aggregate(. ~ Subject + Activity , Dataset, mean) %>%
  arrange(Subject, Activity)








