#The plyr library is used for using the aggregate function for creating the tidy data set
library(plyr);

#The first part of the script is for reading the different files
#All files should be located in the folder data/UCI HAR Dataset in the working dir 
path_rf <- file.path("./data" , "UCI HAR Dataset")

#Activity information
#These are the different activities, stored in the files "test/Y_test.txt" and "train/Y_train.txt".
#1 WALKING
#2 WALKING_UPSTAIRS
#3 WALKING_DOWNSTAIRS
#4 SITTING
#5 STANDING
#6 LAYING
#For Tests are 2947 observations and 1 variable
dataActivityTest  <- read.table(file.path(path_rf, "test" , "Y_test.txt" ),header = FALSE)
#For Training are 7352 observations and 1 variable
dataActivityTrain <- read.table(file.path(path_rf, "train", "Y_train.txt"),header = FALSE)

#Subject Information
#There are different activities, stored in the files "test/subject_test.txt" and "train/subject_train.txt"
#For Tests are 2947 observations and 1 variable
dataSubjectTest  <- read.table(file.path(path_rf, "test" , "subject_test.txt"),header = FALSE)
#For Training are 7352 observations and 1 variable
dataSubjectTrain <- read.table(file.path(path_rf, "train", "subject_train.txt"),header = FALSE)

#Test and Training information
#Test and Training information are stored in the "test/X_test.txt" and "train/X_train.txt" files 
#For Test are 2947 observations and 561 variables
dataFeaturesTest  <- read.table(file.path(path_rf, "test" , "X_test.txt" ),header = FALSE)
#For Training are 7352 observations and 561 variables
dataFeaturesTrain <- read.table(file.path(path_rf, "train", "X_train.txt"),header = FALSE)

#Now all the activities, subjects, tests and trainings is in different variables
#The following is expected in the Course Project:
#1-Merges the training and the test sets to create one data set.
#2-Extracts only the measurements on the mean and standard deviation for each measurement. 
#3-Uses descriptive activity names to name the activities in the data set.
#4-Appropriately labels the data set with descriptive variable names.
#5-From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#1-For merging the data sets in one data set i have to:
#a-Concatenate the information by rows
#Concatenate Activities
dataActivity<- rbind(dataActivityTrain, dataActivityTest)
#Concatenate Subjects
dataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
#Features (Test and Training)
dataFeatures<- rbind(dataFeaturesTrain, dataFeaturesTest)
#Now all the variables have 10299 observations

#b-Add names to data sets
names(dataActivity)<- c("activity")
names(dataSubject)<-c("subject")
#For the features, names are in the file "features.txt" column $V2
names(dataFeatures)<- read.table(file.path(path_rf, "features.txt"),head=FALSE)$V2

#c-Data is combined by columns, first activity and subject, then features
dataCombine <- cbind(dataActivity,dataSubject)
Data <- cbind(dataCombine,dataFeatures)

#2-Extracts only the measurements on the mean and standard deviation for each measurement.
#Using grep i get only the columns that contains mean() or std()
subdataFeaturesNames<-read.table(file.path(path_rf, "features.txt"),head=FALSE)$V2[grep("mean\\(\\)|std\\(\\)", read.table(file.path(path_rf, "features.txt"),head=FALSE)$V2)]
#Only the selected names will be part of the final Data Set
selectedNames<-c("activity","subject",as.character(subdataFeaturesNames))
#The data has 10299 observations and only 68 variables (mean and std)
Data<-subset(Data,select=selectedNames)

#3-Uses descriptive activity names to name the activities in the data set
activityLabels <- read.table(file.path(path_rf, "activity_labels.txt"),header = FALSE)
#For adding the descriptive activity names I use factors
Data$activity<-factor(Data$activity);
Data$activity<- factor(Data$activity,labels=as.character(activityLabels$V2))

#4-Appropriately labels the data set with descriptive variable names
#Time, Frequency, Accelerometer, Gyroscope, Magnitude, Body
names(Data)<-gsub("^t", "time", names(Data))
names(Data)<-gsub("^f", "frequency", names(Data))
names(Data)<-gsub("Acc", "Accelerometer", names(Data))
names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
names(Data)<-gsub("Mag", "Magnitude", names(Data))
names(Data)<-gsub("BodyBody", "Body", names(Data))


#5-From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidyData<-aggregate(. ~subject + activity, Data, mean)
tidyData<-tidyData[order(tidyData$subject,tidyData$activity),]
write.table(tidyData, file = "tidydata.txt",row.name=FALSE)
