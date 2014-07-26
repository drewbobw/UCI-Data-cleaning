#run_analysis script
# Andrew Warlick
# Coursera Cleaning Data project\


# This script takes in the data provided by XXXXXXXXX
# it reads both the test and training datsets
# and performs two major functions. First, It concatenates 
# the files (on rows) and then keeps variables labeled
# as the mean and std of each measurement. 
# Second, it collapses the data set to one observation
# participant per activity (see documentation for activities).
# It does so by taking the mean of each variable. Variables
# are relabled to be more usable. See readme file for more
# details.


#Required R Packages
library(data.table);
library(sqldf);
library(memisc);

#Set local work directory
#setwd("C:\\Users\\dwarlick\\Dropbox\\Data Cleaning Project");
setwd("C:\\Users\\Andrew\\Dropbox\\Data Cleaning Project")
#=======================================================================================
#Read in variable names and activity labels from main folder
varnames<-read.table("features.txt", colClasses= c("integer", "character"));
varnames<-gsub("\\(\\)","",varnames$V2, perl=TRUE);
varnames<-gsub("-","_",varnames, perl=TRUE);
keepcols<-grep("mean|std", varnames);
activitylabels<-read.table("activity_labels.txt", colClasses= c("integer", "character"));
#=======================================================================================
#=======================================================================================
#============================================================================rm===========
#Read in test data - subjects
testsubjects <- read.table("test/subject_test.txt");
names(testsubjects)[1] = "subjectID" ;
#=======================================================================================
#Read in test data - results
testdata<- read.table("test/X_test.txt");
names(testdata) <- varnames;
testdata<-testdata[,c(keepcols)];
testdata$Source<-"TEST";
#=======================================================================================
#Read in test data - activity labels
activityname<-read.table("test/Y_test.txt");
#activityname$V1<-factor(activityname$V1,levels=c(activitylabels$V1), labels=c(activitylabels$V2));
names(activityname)[1] <- "activityIndex";
#=======================================================================================
#Read in test data - bind results
MasterTest<-cbind(testsubjects, testdata,activityname);
        if (exists("MasterTest"))
                 {rm(activityname, testdata, testsubjects);}
#=======================================================================================
#=======================================================================================
#=======================================================================================
#=======================================================================================
#Read in training data - subjects
trainsubjects <- read.table("train/subject_train.txt");
names(trainsubjects)[1] = "subjectID" ;
#=======================================================================================
#Read in training data - results
traindata<- read.table("train/X_train.txt");
names(traindata) <- varnames;
traindata<-traindata[,c(keepcols)];
traindata$Source<-"TRAIN";
#=======================================================================================
#Read in training data - activity labels
trainactivityname<-read.table("train/Y_train.txt");
#trainactivityname$V1<-factor(trainactivityname$V1,levels=c(activitylabels$V1), labels=c(activitylabels$V2));
names(trainactivityname)[1] <- "activityIndex";
#=======================================================================================
#Read in training data  - bind results
MasterTrain<-cbind(trainsubjects, traindata, trainactivityname);
        if (exists("MasterTrain"))
                {rm(trainactivityname, traindata, trainsubjects);}
#=======================================================================================
#Bind Master Resutls; Step 1, 2, & 3 completed
MasterSet <-rbind(MasterTrain, MasterTest);
        if (exists("MasterSet"))
                {rm(MasterTrain, MasterTest);}
#=======================================================================================
#=======================================================================================
#=======================================================================================
# create second clean, tidy dataset

#Combine the given activity descriptions. This probably isn't the best
#way to do it but I know SQL well enough to know it works. Case of 
#framiliarity vs. efficiency.
MasterSet<-sqldf("
           select 
                a.*, 
                b.V2 as activityDesc
                from MasterSet as a, activitylabels as b
                where a.activityIndex = b.V1
           ")

#Big SQL collapse.  Keeps only the grouped variables while calculating the mean
#of all other variables in the data set. New variable names were generated using
#quick concattenate command in excel  
MasterClean<-sqldf("
        select  subjectID,
                Source,
                count(subjectID) as n_obs,
                activityIndex,
                activityDesc,
                avg(tBodyAcc_mean_X) as mean_bodyaccel_x,
                avg(tBodyAcc_mean_X) as mean_BodyAcc_X,
                avg(tBodyAcc_mean_Y) as mean_tBodyAcc_Y,
                avg(tBodyAcc_std_Y) as std_tBodyAcc_Y,
                avg(tGravityAcc_mean_Y) as mean_tGravityAcc_Y,
                avg(tGravityAcc_mean_Z) as mean_tGravityAcc_Z,
                avg(tGravityAcc_std_Z) as std_tGravityAcc_Z,
                   avg(tBodyAccJerk_mean_Z) as mean_tBodyAccJerk_Z,
                   avg(tBodyAccJerk_std_Z) as std_tBodyAccJerk_Z,
                   avg(tBodyGyro_mean_Z) as mean_tBodyGyro_Z,
                   avg(tBodyGyro_std_Z) as std_tBodyGyro_Z,
                   avg(tBodyGyroJerk_mean_Z) as mean_tBodyGyroJerk_Z,
                   avg(tBodyGyroJerk_std_Z) as std_tBodyGyroJerk_Z,
                   avg(fBodyAcc_mean_Y) as mean_fBodyAcc_Y,
                   avg(fBodyAcc_std_Y) as std_fBodyAcc_Y,
                   avg(fBodyAcc_meanFreq_Y) as meanFreq_fBodyAcc_Y,
                   avg(fBodyAccJerk_mean_Y) as mean_fBodyAccJerk_Y,
                   avg(fBodyAccJerk_std_Y) as std_fBodyAccJerk_Y,
                   avg(fBodyAccJerk_meanFreq_Y) as meanFreq_fBodyAccJerk_Y,
                   avg(fBodyGyro_mean_Y) as mean_fBodyGyro_Y,
                   avg(fBodyGyro_std_Y) as std_fBodyGyro_Y,
                   avg(fBodyGyro_meanFreq_Y) as meanFreq_fBodyGyro_Y,
                   avg(tBodyAcc_mean_Z) as mean_tBodyAcc_Z,
                   avg(tBodyAcc_std_Z) as std_tBodyAcc_Z,
                   avg(tGravityAcc_std_X) as std_tGravityAcc_X,
                   avg(tBodyAccJerk_mean_X) as mean_tBodyAccJerk_X,
                   avg(tBodyAccJerk_std_X) as std_tBodyAccJerk_X,
                   avg(tBodyGyro_mean_X) as mean_tBodyGyro_X,
                   avg(tBodyGyro_std_X) as std_tBodyGyro_X,
                   avg(tBodyGyroJerk_mean_X) as mean_tBodyGyroJerk_X,
                   avg(tBodyGyroJerk_std_X) as std_tBodyGyroJerk_X,
                   avg(fBodyAcc_mean_Z) as mean_fBodyAcc_Z,
                   avg(fBodyAcc_std_Z) as std_fBodyAcc_Z,
                   avg(fBodyAcc_meanFreq_Z) as meanFreq_fBodyAcc_Z,
                   avg(fBodyAccJerk_mean_Z) as mean_fBodyAccJerk_Z,
                   avg(fBodyAccJerk_std_Z) as std_fBodyAccJerk_Z,
                   avg(fBodyAccJerk_meanFreq_Z) as meanFreq_fBodyAccJerk_Z,
                   avg(fBodyGyro_mean_Z) as mean_fBodyGyro_Z,
                   avg(fBodyGyro_std_Z) as std_fBodyGyro_Z,
                   avg(fBodyGyro_meanFreq_Z) as meanFreq_fBodyGyro_Z,
                   avg(tBodyAcc_mean_X) as mean_tBodyAcc_X,
                   avg(tBodyAcc_std_X) as std_tBodyAcc_X,
                   avg(tGravityAcc_mean_X) as mean_tGravityAcc_X,
                   avg(tGravityAcc_std_Y) as std_tGravityAcc_Y,
                   avg(tBodyAccJerk_mean_Y) as mean_tBodyAccJerk_Y,
                   avg(tBodyAccJerk_std_Y) as std_tBodyAccJerk_Y,
                   avg(tBodyGyro_mean_Y) as mean_tBodyGyro_Y,
                   avg(tBodyGyro_std_Y) as std_tBodyGyro_Y,
                   avg(tBodyGyroJerk_mean_Y) as mean_tBodyGyroJerk_Y,
                   avg(tBodyGyroJerk_std_Y) as std_tBodyGyroJerk_Y,
                   avg(fBodyAcc_mean_X) as mean_fBodyAcc_X,
                   avg(fBodyAcc_std_X) as std_fBodyAcc_X,
                   avg(fBodyAcc_meanFreq_X) as meanFreq_fBodyAcc_X,
                   avg(fBodyAccJerk_mean_X) as mean_fBodyAccJerk_X,
                   avg(fBodyAccJerk_std_X) as std_fBodyAccJerk_X,
                   avg(fBodyAccJerk_meanFreq_X) as meanFreq_fBodyAccJerk_X,
                   avg(fBodyGyro_mean_X) as mean_fBodyGyro_X,
                   avg(fBodyGyro_std_X) as std_fBodyGyro_X,
                   avg(fBodyGyro_meanFreq_X) as meanFreq_fBodyGyro_X,
                   avg(fBodyBodyAccJerkMag_meanFreq) as meanFreq_fBodyBodyAccJerkMag,
                   avg(fBodyBodyGyroMag_meanFreq) as meanFreq_fBodyBodyGyroMag,
                   avg(fBodyBodyGyroJerkMag_meanFreq) as meanFreq_fBodyBodyGyroJerkMag,
                   avg(fBodyAccMag_mean) as mean_fBodyAccMag,
                   avg(fBodyBodyAccJerkMag_mean) as mean_fBodyBodyAccJerkMag,
                   avg(fBodyBodyGyroMag_mean) as mean_fBodyBodyGyroMag,
                   avg(fBodyBodyGyroJerkMag_mean) as mean_fBodyBodyGyroJerkMag,
                   avg(fBodyAccMag_std) as std_fBodyAccMag,
                   avg(fBodyBodyAccJerkMag_std) as std_fBodyBodyAccJerkMag,
                   avg(fBodyBodyGyroMag_std) as std_fBodyBodyGyroMag,
                   avg(fBodyBodyGyroJerkMag_std) as std_fBodyBodyGyroJerkMag,
                   avg(fBodyAccMag_meanFreq) as meanFreq_fBodyAccMag,
                   avg(tGravityAccMag_mean) as mean_tGravityAccMag,
                   avg(tBodyAccJerkMag_std) as std_tBodyAccJerkMag,
                   avg(tBodyGyroJerkMag_mean) as mean_tBodyGyroJerkMag,
                   avg(tBodyAccMag_mean) as mean_tBodyAccMag,
                   avg(tGravityAccMag_std) as std_tGravityAccMag,
                   avg(tBodyGyroMag_mean) as mean_tBodyGyroMag,
                   avg(tBodyGyroJerkMag_std) as std_tBodyGyroJerkMag,
                   avg(tBodyAccMag_std) as std_tBodyAccMag,
                   avg(tBodyAccJerkMag_mean) as mean_tBodyAccJerkMag,
                   avg(tBodyGyroMag_std) as std_tBodyGyroMag

         from MasterSet
         group by subjectID, activityIndex
         order by subjectID, activityIndex")

#=======================================================================================
#=======================================================================================
#=======================================================================================
# Break the larger dataset in smaller ones based on activity measured

for (act in 1:nrow(activitylabels))
  {
    file_name=paste0("resutls_",(activitylabels[act,2]),".csv");
    write.csv(MasterClean[(MasterClean$activityDesc==activitylabels[act,2]),],file=file_name, row.names=F) ;
  }

#Write Master Data to file
write.csv(MasterClean,file="Master_Results.csv", row.names=F) ;
#=======================================================================================
#=======================================================================================
#=======================================================================================
#generate codebook
if (!exists("codebook.md"))
        {
                sink("codebook.md");
                TempFrame<-data.set(MasterClean);
                codebook(TempFrame);
                rm(TempFrame);
                sink();
        }
#==================================================================
#============================EOF=================================
#================================================================