library(dplyr)
library(tidyr)

run_analysis<-function(dir){
#Step 1. Merges the training and the test sets to create one data set.

x<-read.table(paste(dir,"/train/X_train.txt",sep=""))
x2<-read.table(paste(dir,"/test/X_test.txt",sep=""))
data<-rbind(x,x2)

#Step 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

#Use feature table to assign name to columns in data set

colnames(data)<-read.table(paste(dir,"/features.txt",sep=""),stringsAsFactors=FALSE)[,2]

#Remove the data with duplicate names since we are not going to use those columns
data<-data[,unique(names(data))]

#Select columns which has mean() or std() in their name. 
#As per Feature Infor these are the columns we need.

data<-select(data,contains("mean()"), contains("std()"))

#Now add Subjects and Activity to data set.
#First read training data for subjects and Activity, bind it by coloumns
x<-read.table(paste(dir,"/train/subject_train.txt",sep=""),stringsAsFactors=FALSE)
x2<-read.table(paste(dir,"/train/Y_train.txt",sep=""),stringsAsFactors=FALSE)
x<-cbind(x,x2)

#then read test data for subjects and Activity, bind it by coloumns
x2<-read.table(paste(dir,"/test/subject_test.txt",sep=""),stringsAsFactors=FALSE)
x3<-read.table(paste(dir,"/test/Y_test.txt",sep=""),stringsAsFactors=FALSE)
x2<-cbind(x2,x3)

#Now add both training and test data by rows (in same order in which we added feature data sets)
x<-rbind(x,x2)
colnames(x)<-c("Subject","Activity")


#Step 3. Uses descriptive activity names to name the activities in the data set
#use activity lables table to replace intergers with descriptive names

x2<-read.table(paste(dir,"/activity_labels.txt",sep=""),stringsAsFactors=FALSE)
x$Activity<-x2[x$Activity,2]

#Add Activity and Subjects to feature data set
data<-cbind(x,data)

#clean up unused variables
rm("x")
rm("x2")
rm("x3")

#Step 4. Appropriately labels the data set with descriptive variable names
nw<-gsub("^t","time",names(data))
nw<-gsub("^f","fft",nw)
nw<-gsub("Acc","accelerometer",nw)
nw<-gsub("Gyro","gyroscope",nw)
nw<-gsub("std","standarddeviation",nw)
nw<-gsub("\\(\\)","",nw)
nw<-tolower(nw)
colnames(data)<-nw

#Step 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#First Group the data by acitvity and Subjects.

data<-group_by(data,activity,subject)
# Then use summarise to calculate mean of each feature in data set
#To do this we need to create a string which can be passed to function summarise

cols<-names(data)[c(-1,-2)]
dots <- sapply(cols ,function(x) substitute(mean(x), list(x=as.name(x))))
#Now dots is a list in which each element is argument we need to pass to summarise
#use do.call function to call summarize with created list dots

newdata<-do.call(summarise, c(list(.data=data), dots))

#write the newdata to disk
write.table(newdata,file=paste(dir,"/tidydata.csv",sep=""),row.name=FALSE)

}

