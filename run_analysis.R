##import files
x<-read.csv("X_train.txt")
y<-read.csv("y_train.txt")
subject<-read.csv("subject_train.txt")
testx<-read.csv("X_test.txt")
testy<-read.csv("y_test.txt")
testsubject<-read.csv("subject_test.txt")

##add id field to each variable
x<- cbind(x,seq.int(nrow(x)))
y<- cbind(y,seq.int(nrow(y)))
subject<- cbind(subject,seq.int(nrow(subject)))
testx<- cbind(testx,seq.int(nrow(testx)))
testy<- cbind(testy,seq.int(nrow(testy)))
testsubject<- cbind(testsubject,seq.int(nrow(testsubject)))

colnames(x)<-c("Data","ID")
colnames(y)<-c("Activity Label","ID")
colnames(subject)<-c("Subject","ID")
colnames(testx)<-c("Data","ID")
colnames(testy)<-c("Activity Label","ID")
colnames(testsubject)<-c("Subject","ID")

##merge test and train files
train1<-merge(x,y)
train2<-merge(train1,subject)
test1<-merge(testx,testy)
test2<-merge(train1,testsubject)
data<- rbind(train2,test2)


##import feature files
features<-read.csv("features.txt",sep=" ",header=FALSE)
colnames(features)<-c("feature","feature_desc")
vectnames<-as.vector(paste(features$feature,":",features$feature_desc,sep=""))

##split data column into 561 different columns
library(tidyr)
library(stringr)
data<-cbind(data,d2=gsub("  "," ",str_trim(data$Data)))
data2<-type.convert(separate(data,d2,into=vectnames,sep=" "))

##find features related to mean and standard deviation
features<-cbind(features,Include=grepl("mean|Mean|std",features$feature_desc))
features2<-filter(features,Include==TRUE)
##vectnames variable will be used to populate field names appropriately
vectnames2<-as.vector(paste(features2$feature,":",features2$feature_desc,sep=""))

##select columns where feature related to mean and standard deviation
library(dplyr)
data3<-type.convert(select (data2,"ID","Activity Label","Subject",vectnames2))



##import activity label file
al<-read.csv("activity_labels.txt",header=FALSE)
al<-separate(al,V1,c("num","activity"),sep=" ")
data4<- merge(data3,al,by.x="Activity Label",by.y="num",all=FALSE)

##average of each variable for each activity and each subject
final <- (data4%>%group_by(Subject,activity)%>%
              summarise('tBodyAcc-mean()-X' = mean(.[[4]]),
                        'tBodyAcc-mean()-Y' = mean(.[[5]]),
                        'tBodyAcc-mean()-Z' = mean(.[[6]]),
                        'tBodyAcc-std()-X' = mean(.[[7]]),
                        'tBodyAcc-std()-Y' = mean(.[[8]]),
                        'tBodyAcc-std()-Z' = mean(.[[9]]),
                        'tGravityAcc-mean()-X' = mean(.[[10]]),
                        'tGravityAcc-mean()-Y' = mean(.[[11]]),
                        'tGravityAcc-mean()-Z' = mean(.[[12]]),
                        'tGravityAcc-std()-X' = mean(.[[13]]),
                        'tGravityAcc-std()-Y' = mean(.[[14]]),
                        'tGravityAcc-std()-Z' = mean(.[[15]]),
                        'tBodyAccJerk-mean()-X' = mean(.[[16]]),
                        'tBodyAccJerk-mean()-Y' = mean(.[[17]]),
                        'tBodyAccJerk-mean()-Z' = mean(.[[18]]),
                        'tBodyAccJerk-std()-X' = mean(.[[19]]),
                        'tBodyAccJerk-std()-Y' = mean(.[[20]]),
                        'tBodyAccJerk-std()-Z' = mean(.[[21]]),
                        'tBodyGyro-mean()-X' = mean(.[[22]]),
                        'tBodyGyro-mean()-Y' = mean(.[[23]]),
                        'tBodyGyro-mean()-Z' = mean(.[[24]]),
                        'tBodyGyro-std()-X' = mean(.[[25]]),
                        'tBodyGyro-std()-Y' = mean(.[[26]]),
                        'tBodyGyro-std()-Z' = mean(.[[27]]),
                        'tBodyGyroJerk-mean()-X' = mean(.[[28]]),
                        'tBodyGyroJerk-mean()-Y' = mean(.[[29]]),
                        'tBodyGyroJerk-mean()-Z' = mean(.[[30]]),
                        'tBodyGyroJerk-std()-X' = mean(.[[31]]),
                        'tBodyGyroJerk-std()-Y' = mean(.[[32]]),
                        'tBodyGyroJerk-std()-Z' = mean(.[[33]]),
                        'tBodyAccMag-mean()' = mean(.[[34]]),
                        'tBodyAccMag-std()' = mean(.[[35]]),
                        'tGravityAccMag-mean()' = mean(.[[36]]),
                        'tGravityAccMag-std()' = mean(.[[37]]),
                        'tBodyAccJerkMag-mean()' = mean(.[[38]]),
                        'tBodyAccJerkMag-std()' = mean(.[[39]]),
                        'tBodyGyroMag-mean()' = mean(.[[40]]),
                        'tBodyGyroMag-std()' = mean(.[[41]]),
                        'tBodyGyroJerkMag-mean()' = mean(.[[42]]),
                        'tBodyGyroJerkMag-std()' = mean(.[[43]]),
                        'fBodyAcc-mean()-X' = mean(.[[44]]),
                        'fBodyAcc-mean()-Y' = mean(.[[45]]),
                        'fBodyAcc-mean()-Z' = mean(.[[46]]),
                        'fBodyAcc-std()-X' = mean(.[[47]]),
                        'fBodyAcc-std()-Y' = mean(.[[48]]),
                        'fBodyAcc-std()-Z' = mean(.[[49]]),
                        'fBodyAcc-meanFreq()-X' = mean(.[[50]]),
                        'fBodyAcc-meanFreq()-Y' = mean(.[[51]]),
                        'fBodyAcc-meanFreq()-Z' = mean(.[[52]]),
                        'fBodyAccJerk-mean()-X' = mean(.[[53]]),
                        'fBodyAccJerk-mean()-Y' = mean(.[[54]]),
                        'fBodyAccJerk-mean()-Z' = mean(.[[55]]),
                        'fBodyAccJerk-std()-X' = mean(.[[56]]),
                        'fBodyAccJerk-std()-Y' = mean(.[[57]]),
                        'fBodyAccJerk-std()-Z' = mean(.[[58]]),
                        'fBodyAccJerk-meanFreq()-X' = mean(.[[59]]),
                        'fBodyAccJerk-meanFreq()-Y' = mean(.[[60]]),
                        'fBodyAccJerk-meanFreq()-Z' = mean(.[[61]]),
                        'fBodyGyro-mean()-X' = mean(.[[62]]),
                        'fBodyGyro-mean()-Y' = mean(.[[63]]),
                        'fBodyGyro-mean()-Z' = mean(.[[64]]),
                        'fBodyGyro-std()-X' = mean(.[[65]]),
                        'fBodyGyro-std()-Y' = mean(.[[66]]),
                        'fBodyGyro-std()-Z' = mean(.[[67]]),
                        'fBodyGyro-meanFreq()-X' = mean(.[[68]]),
                        'fBodyGyro-meanFreq()-Y' = mean(.[[69]]),
                        'fBodyGyro-meanFreq()-Z' = mean(.[[70]]),
                        'fBodyAccMag-mean()' = mean(.[[71]]),
                        'fBodyAccMag-std()' = mean(.[[72]]),
                        'fBodyAccMag-meanFreq()' = mean(.[[73]]),
                        'fBodyBodyAccJerkMag-mean()' = mean(.[[74]]),
                        'fBodyBodyAccJerkMag-std()' = mean(.[[75]]),
                        'fBodyBodyAccJerkMag-meanFreq()' = mean(.[[76]]),
                        'fBodyBodyGyroMag-mean()' = mean(.[[77]]),
                        'fBodyBodyGyroMag-std()' = mean(.[[78]]),
                        'fBodyBodyGyroMag-meanFreq()' = mean(.[[79]]),
                        'fBodyBodyGyroJerkMag-mean()' = mean(.[[80]]),
                        'fBodyBodyGyroJerkMag-std()' = mean(.[[81]]),
                        'fBodyBodyGyroJerkMag-meanFreq()' = mean(.[[82]]),
                        'angle(tBodyAccMean,gravity)' = mean(.[[83]]),
                        'angle(tBodyAccJerkMean),gravityMean)' = mean(.[[84]]),
                        'angle(tBodyGyroMean,gravityMean)' = mean(.[[85]]),
                        'angle(tBodyGyroJerkMean,gravityMean)' = mean(.[[86]]),
                        'angle(X,gravityMean)' = mean(.[[87]]),
                        'angle(Y,gravityMean)' = mean(.[[88]]),
                        'angle(Z,gravityMean)' = mean(.[[89]])))
                        
write.table(final,file="courseoutcomefile.txt",row.names=FALSE)
  