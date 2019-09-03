#TEST PART
#import the "test" tables

test_data<-read.csv("./UCI HAR Dataset/test/X_test.txt",sep="",header=FALSE)
type_act_test<-read.csv("./UCI HAR Dataset/test/y_test.txt",sep=" ",header=FALSE)
features<-read.csv("./UCI HAR Dataset/features.txt",sep=" ",header=FALSE)
subject_test<-read.csv("./UCI HAR Dataset/test/subject_test.txt",sep=" ",header=FALSE)

#rename test data table with the "feature" table

names(test_data) <- features$V2

#rename rename variables of the subject_test and y_test tables

names(type_act_test)[1]<-"act"
names(subject_test)[1]<-"subject"

#bring the tables together

data_compl_test<-cbind(subject_test,type_act_test,test_data)

#TRAIN PART
#import the "test" tables

train_data<-read.csv("./UCI HAR Dataset/train/X_train.txt",sep="",header=FALSE)
type_act_train<-read.csv("./UCI HAR Dataset/train/y_train.txt",sep=" ",header=FALSE)
subject_train<-read.csv("./UCI HAR Dataset/train/subject_train.txt",sep=" ",header=FALSE)

#rename train data table with the "feature" table

names(train_data) <- features$V2

#rename rename variables of the subject_train and y_train tables

names(type_act_train)[1]<-"act"
names(subject_train)[1]<-"subject"

#bring the tables together

data_compl_train<-cbind(subject_train,type_act_train,train_data)      

# 1 - Merges the training and the test sets to create one data set.

testc_trainc<-rbind(data_compl_train,data_compl_test)

# 2 - Extracts only the measurements on the mean and standard deviation for each measurement.

index_col<-grep("*mean*|*std*|subject|act",names(testc_trainc))
test_train_reduct<-testc_trainc[,(index_col)]

# 3- Uses descriptive activity names to name the activities in the data set
library(tidyverse)

test_train_reduct$act<-as.factor(test_train_reduct$act)

test_train_reduct$act<-fct_recode(test_train_reduct$act, "WALKING"="1",
               "WALKING_UPSTAIRS"="2",
               "WALKING_DOWNSTAIRS"="3",
               "SITTING"="4",
               "STANDING"="5",
               "LAYING"="6"
                                )

# 4 Appropriately labels the data set with descriptive variable names.

#It's OK

# 5 From the data set in step 4, creates a second, independent tidy data set
#   with the average of each variable for each activity and each subject.

apply(test_train_reduct,2,class)

test_train_reduct_num<-as.numeric(test_train_reduct[,(3)])

apply(test_train_reduct[,(3:81)],2,as.numeric)

groupby_subj_act<-group_by(test_train_reduct,subject,act)

library(plyr)
library(dplyr)

mean_all<-groupby_subj_act %>%
        select_if(is.numeric) %>%
        summarise_all(mean)

write.table (mean_all,file="./mean_all.txt",row.name = FALSE)
