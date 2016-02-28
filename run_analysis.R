# 1. Merge the training and the test sets.

#set working directory to the location where the UCI HAR Dataset was unzipped
setwd('C:/Users/Daniel/Downloads/Coursera/getcleandata/UCI HAR Dataset');

# Read in the data from files
features = read.table('./features.txt',header=FALSE);
activity_label = read.table('./activity_labels.txt',header=FALSE); 
subject_train = read.table('./train/subject_train.txt',header=FALSE);
x_train = read.table('./train/x_train.txt',header=FALSE); 
y_train = read.table('./train/y_train.txt',header=FALSE); 

# Assign column names to the data
colnames(activity_label) = c('activityId','activityType');
colnames(subject_train) = "subjectId";
colnames(x_train) = features[,2]; 
colnames(y_train) = "activityId";

# Create the final training dataset 
training_data = cbind(y_train,subject_train,x_train);

# Read in the test data
subject_test = read.table('./test/subject_test.txt',header=FALSE); 
x_test = read.table('./test/x_test.txt',header=FALSE);
y_test = read.table('./test/y_test.txt',header=FALSE); 

# Assign column names to the test dataset
colnames(subject_test) = "subjectId";
colnames(x_test) = features[,2]; 
colnames(y_test) = "activityId";


# Create the final test dataset 
test_data = cbind(y_test,subject_test,x_test);


# Combine training and test data to create a final dataset
final_data = rbind(training_data,test_data);

# Create a vector for the column names from the final dataset, which will be used to select the desired mean() & stddev() columns
col_names  = colnames(final_data); 

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a logical vector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
log_vector = (grepl("activity..",col_names) | grepl("subject..",col_names) | grepl("-mean..",col_names) & !grepl("-meanFreq..",col_names) & !grepl("mean..-",col_names) | grepl("-std..",col_names) & !grepl("-std()..-",col_names));

# Subset final_data table based on the log_vector to keep only desired columns
final_data = final_data[log_vector==TRUE];

# 3. Use descriptive activity names to name the activities in the dataset

# Merge the final dataset with the acitivity_type table to include descriptive activity names
final_data = merge(final_data,activity_label,by='activityId',all.x=TRUE);

# Updating the col_names vector to include the new column names after merge
col_names  = colnames(final_data); 

# 4. Appropriately label the dataset with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(col_names)) 
{
  col_names[i] = gsub("\\()","",col_names[i])
  col_names[i] = gsub("-std$","StdDev",col_names[i])
  col_names[i] = gsub("-mean","Mean",col_names[i])
  col_names[i] = gsub("^(t)","time",col_names[i])
  col_names[i] = gsub("^(f)","freq",col_names[i])
  col_names[i] = gsub("([Gg]ravity)","Gravity",col_names[i])
  col_names[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",col_names[i])
  col_names[i] = gsub("[Gg]yro","Gyro",col_names[i])
  col_names[i] = gsub("AccMag","AccMagnitude",col_names[i])
  col_names[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",col_names[i])
  col_names[i] = gsub("JerkMag","JerkMagnitude",col_names[i])
  col_names[i] = gsub("GyroMag","GyroMagnitude",col_names[i])
};

# Reassigning the new descriptive column names to the final dataset
colnames(final_data) = col_names;

# 5. Create a second, independent tidy dataset with the average of each variable for each activity and each subject. 

# Create a new table final_data_no_type without the activity_label column
final_data_no_type  = final_data[,names(final_data) != 'activityType'];

# Summarize the final_data_no_type table to include just the mean of each variable for each activity and subject
tidy_data    = aggregate(final_data_no_type[,names(final_data_no_type) != c('activityId','subjectId')],by=list(activityId=final_data_no_type$activityId,subjectId = final_data_no_type$subjectId),mean);

# Merge the tidy_data with activity_label to include descriptive acitvity names
tidy_data    = merge(tidy_data,activity_label,by='activityId',all.x=TRUE);

# Export the tidydataset 
write.table(tidy_data, './tidy_data.txt',row.names=TRUE,sep='\t');
