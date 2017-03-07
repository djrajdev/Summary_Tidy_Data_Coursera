# @Author: DJ Rajdev
# @Purpose: Lecture 4, Project

# rm(list=ls())
# setwd(paste('','Users','divyajyotirajdev','Desktop','code', 'getting_cleaning_data', 'assignment_quiz',sep='/'))

## Before starting check for dir, if not found download file
if(!file.exists(file.path(getwd(),'UCI HAR Dataset'))) {
  download.file(url='https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip',
                destfile = file.path(getwd(),'HAR Data.zip'))
  # Unzip File
  unzip(zipfile=file.path(getwd(),'HAR Data.zip'),
        exdir=getwd())
}
## Task 1
## Merges the training and the test sets to create one data set.

# Reading the observations
train <- read.table(file.path(getwd(),'UCI HAR Dataset', 'train', 'X_train.txt'),
                   header = F)
test <- read.table(file.path(getwd(),'UCI HAR Dataset', 'test', 'X_test.txt'),
                   header = F)
# merge function cannot be used since there is no 'primary key', using rbind
combined_dat <- rbind(train,test)

# Similarly, reading the activity labels
activity_train <- read.table(file.path(getwd(),'UCI HAR Dataset', 'train', 'y_train.txt'),
                    header = F)
activity_test <- read.table(file.path(getwd(),'UCI HAR Dataset', 'test', 'y_test.txt'),
                   header = F)
activity_combined_dat <- rbind(activity_train,activity_test)
names(activity_combined_dat) <- 'activity'

# Similarly, reading the subject labels
subject_train <- read.table(file.path(getwd(),'UCI HAR Dataset', 'train', 'subject_train.txt'),
                             header = F)
subject_test <- read.table(file.path(getwd(),'UCI HAR Dataset', 'test', 'subject_test.txt'),
                            header = F)
subject_combined_dat <- rbind(subject_train,subject_test)
names(subject_combined_dat) <- 'subject'

## Task 2
## Extract only Mean, SD information for each measurement
# From Features file get logic vector of names that contain mean, std
feature_names <- read.table(file.path(getwd(),'UCI HAR Dataset', 'features.txt'),
                            header= F, stringsAsFactors = F)
feature_names <- feature_names[,2]
feature_names_sd_mean <- grepl('mean\\(\\)|std\\(\\)', feature_names)

# Using above log function select cols in combined_dat
combined_dat <- combined_dat[,feature_names_sd_mean]

## Task 3 Uses descriptive activity names to name the activities in the data set
map_activity_names <- read.table(file.path(getwd(),'UCI HAR Dataset', 'activity_labels.txt'),
                                 header= F, stringsAsFactors = F)
names(map_activity_names) <- c('label', 'activity')
for(i in 1:nrow(activity_combined_dat)) {
  tmp <- activity_combined_dat[i,]
  activity_combined_dat[i,] <- map_activity_names[tmp,2]
}

## Task 4 Appropriately labels the data set with descriptive variable names.

names(combined_dat) <- feature_names[feature_names_sd_mean]

## Combining Vars, Label, Subjects
tidy_combined <- cbind(subject_combined_dat, combined_dat, activity_combined_dat)  

## Task 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

summary_tidy_combined <- with(tidy_combined, aggregate(tidy_combined, by= list(act=activity, sub=subject), FUN=mean))
summary_tidy_combined <- summary_tidy_combined[,!(names(summary_tidy_combined) %in% c('activity', 'sub'))]  
names(summary_tidy_combined)[names(summary_tidy_combined)=="act"] <- "activity"

## Writing tidy data and analysis to file
write.table(tidy_combined, file = file.path(getwd(), 'Tidy_UCI_Dataset.txt')
            , row.names= F)
write.table(summary_tidy_combined, file = file.path(getwd(), 'Summary_Tidy_UCI_Dataset.txt')
            , row.names= F)

## Testing written file
# tmp <- read.table(file = file.path(getwd(), 'Tidy_UCI_Dataset.txt'), header=T)
# tmp <- read.table(file = file.path(getwd(), 'Summary_Tidy_UCI_Dataset.txt'), header=T)
