#Create the directory and download the files

directory.file<-"D:/Documents/2A/R courses John Hopkins University/Getting and cleaning data/Project"

if (!file.exists(directory.file)){
  dir.create(directory.file)  
}

setwd(directory.file)

fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

if (!file.exists("./donnée.zip")){
  download.file(fileUrl,destfile="./donnée.zip")
  unzip("./donnée.zip", exdir = ".")
}

#Extract the data

list.files()
dir<-"UCI HAR Dataset"
setwd(paste("./",dir,sep=""))

list.files()
dir<-"test"
setwd(paste("./",dir,sep=""))

list.files()
X_test <- read.table("./X_test.txt", quote="\"")
Y_test <- read.table("./y_test.txt", quote="\"")
subject_test <- read.table("./subject_test.txt", quote="\"")

setwd("../")
dir<-"train"
setwd(paste("./",dir,sep=""))

list.files()
X_train <- read.table("./X_train.txt", quote="\"")
Y_train <- read.table("./y_train.txt", quote="\"")
subject_train <- read.table("./subject_train.txt", quote="\"")

setwd("../")

#Question 1 : Merges the training and the test sets to create one data set

DataTest <- data.frame(subject_test,Y_test,X_test)
DataTrain <- data.frame(subject_train,Y_train,X_train)
Data <- rbind(DataTest,DataTrain)

#Give the name of the features to the colon names of the data corresponding to Xs
features <- read.table("./features.txt", quote="\"")
colnames(Data)<-c("Subject","Activities.Labels",as.character(features[,2]))

#Question 2 : Extracts only the measurements on the mean and standard deviation for each measurement

#the extraction is made by looking for the features refering to mean and data

s <- grepl("mean",colnames(Data),ignore.case=TRUE) | grepl("std",colnames(Data),ignore.case=TRUE)
s[1]<-TRUE
s[2]=TRUE
NewData <- subset(Data,select=s)

#Question 3 and 4 :

#Cleaning the colnames (removing "(",")","-")
colnames(NewData)<-gsub("-","",colnames(NewData))
colnames(NewData)<-gsub(")","",colnames(NewData))
colnames(NewData)<-gsub("\\(","",colnames(NewData))
colnames(NewData)<-gsub("mean",".Mean.",colnames(NewData))
colnames(NewData)<-gsub(",","",colnames(NewData))
colnames(NewData)<-gsub("std",".Std.",colnames(NewData))

#Renaming activities names
NewData$Activities.Labels<-gsub("1","Walking",NewData$Activities.Labels)
NewData$Activities.Labels<-gsub("2","WalkingUpstairs",NewData$Activities.Labels)
NewData$Activities.Labels<-gsub("3","WalkingDownstairs",NewData$Activities.Labels)
NewData$Activities.Labels<-gsub("4","Sitting",NewData$Activities.Labels)
NewData$Activities.Labels<-gsub("5","Standing",NewData$Activities.Labels)
NewData$Activities.Labels<-gsub("6","Laying",NewData$Activities.Labels)

#Question 5 :

n <- ncol(NewData)

agg<-aggregate(NewData[,3], by = list(Subject=NewData$Subject,Activities=NewData$Activities.Labels), sum)
colnames(agg)[3]<-colnames(NewData)[3]

#Calculate the mean of each column for each subject and activity
for (i in 4:n){
  agg<-merge(agg,aggregate(NewData[,i], by = list(Subject=NewData$Subject,Activities=NewData$Activities.Labels), sum))
  colnames(agg)[i]<-colnames(NewData)[i]  
}

Tidy.Data.Set<-agg

#Write the tidy data set in text file
write.table(Tidy.Data.Set, file = "TidyDataSet.txt")


###The End###
  