### Getting and Cleaning Data Course Project

## data: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
## Step 1: Merge the training and the test sets to create one data set.
## Step 2: Extract only the measurements on the mean and standard deviation for each measurement. 
## Step 3: Use descriptive activity names to name the activities in the data set
## Step 4: Appropriately label the data set with descriptive activity names. 
## Step 5: Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Step 1
# set working directory to the location where the UCI HAR Dataset was unzipped
setwd('/Users/linqin/Downloads/UCI HAR Dataset/');

# Read in the data from files
features = read.table('./features.txt', header=FALSE);
activity = read.table('./activity_labels.txt', header=FALSE);
subjecttrain = read.table ('./train/subject_train.txt', header=FALSE);
xtrain = read.table ('./train/x_train.txt', header=FALSE);
ytrain = read.table ('./train/y_train.txt', header=FALSE);

# Assign column names to the imported tables
colnames(activity)=c('activityID', 'activityType');
colnames(subjecttrain)='subjectID';
colnames(xtrain)=features[,2];
colnames(ytrain)='activityID';

# Create the final training set by merging ytrain, subjecttrain, and xtrain
trainingdata=cbind(ytrain, subjecttrain, xtrain);

# Read in the test data
subjecttest = read.table('./test/subject_test.txt', header=FALSE);
xtest = read.table('./test/x_test.txt', header=FALSE);
ytest = read.table('./test/y_test.txt', header=FALSE);

# Assign column names to the test data
colnames(subjecttest)='subjectID';
colnames(xtest)=features[,2];
colnames(ytest)='activityID';

# Create the final test set by merging the xtest, ytest, and subjecttest 
testdata = cbind(ytest, subjecttest, xtest);

# Combine training and test data to create a final data set
finaldata = rbind (trainingdata, testdata);

colNames = colnames(finaldata);

# Step 2
# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table based on the logicalVector to keep only desired columns
finaldata = finaldata[logicalVector==TRUE];


# Step 3
# Merge the finalData set with the acitivityType table to include descriptive activity names
finaldata = merge(finaldata,activity,by='activityID',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(finalData); 

# Step 4
# Cleaning up the variable names
for (i in 1:length(colNames)) 
{colNames[i] = gsub("\\()","",colNames[i])
 colNames[i] = gsub("-std$","StdDev",colNames[i])
 colNames[i] = gsub("-mean","Mean",colNames[i])
 colNames[i] = gsub("^(t)","time",colNames[i])
 colNames[i] = gsub("^(f)","freq",colNames[i])
 colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
 colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
 colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
 colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
 colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
 colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
 colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
 };
 
 # Reassigning the new descriptive column names to the finaldata set
 colnames(finaldata)=colNames;
 
 # Step 5
 # Create a new table, finaldata2 without the activityType column
 finaldata2 = finaldata[,names(finaldata) !='activityType'];
 
 # Summarizing the finaldata2 table to include just the mean of each variable for each activity and each subject
tidydata=aggregate(finaldata2[,names(finaldata2) != c('activityID','subjectID')],by=list(activityID=finaldata2$activityID,subjectID = finaldata2$subjectID),mean);

# Merging the tidydata with activityType to include descriptive acitvity names
tidydata    = merge(tidydata,activity,by='activityID',all.x=TRUE);

# Export the tidydata set
write.table(tidydata, './tidydata.txt',row.names=TRUE,sep='\t');
