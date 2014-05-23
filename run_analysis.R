# Returns one data set by reading and merging all component files.
# Data set comprises of the X values, Y values and Subject IDs specified by the input filenames
# further the data set is filtered by the input filter which eliminates all but mean and s.d. columns
# This also subsets the data to extract only the measurements on the mean and standard deviation for each measurement.
# The required columns in the subset is determined by selecting only those columns that have either "mean()" or "std()" in their names.
# Subsetting is done early on to help reduce memory requirements (avoiding crashes on merge calls)

#the following function receives multiple filenames it exists to read data separately for train and test
# col_filter is the reduced set of columns requires, x_col_names is the col_names for the x file extracted once using features
readData <- function(x_datafilename, subjectfilename, y_datafilename, x_col_names, col_filter) {
 
  # get labels
  subject_data <- read.table(subjectfilename, header=F, col.names=c("SubjectID"))
  
  # get activity id
  y_data <- read.table(y_datafilename, header=F, col.names=c("ActivityID"))

  # read the X data file
  data <- read.table(x_datafilename, header=F, col.names=x_col_names)
  
  # req2. Extracts only the measurements on the mean and standard deviation for each measurement.   
  # filter out so only specified columns are retained
  data <- data[,col_filter]
  
  # append the activity id and subject id columns
  data$ActivityID <- y_data$ActivityID
  data$SubjectID <- subject_data$SubjectID
  
  # return the data
  return(data)
}

# Load & Merge both train and test data sets
# returns the merged neat data set with ActivityNames replacing ActivityID
#
loadData <- function() {
  library(reshape2)
  
  # read the column names
  cols <- read.table("features.txt", header=F, as.is=T, col.names=c("MeasureID", "MeasureName"))

  # names of subset columns required
  filter <- grep(".*mean\\(\\)|.*std\\(\\)", cols$MeasureName)
  
  # read the activity labels
  activity_labels <- read.table("activity_labels.txt", header=F, as.is=T, col.names=c("ActivityID", "ActivityName"))
  activity_labels$ActivityName <- as.factor(activity_labels$ActivityName)  
  
  # req1. Merges the training and test sets to create one data set.
  # req2. see inside readData function
  # read the two data sets and merge them
  data <- rbind(readData("test/X_test.txt","test/subject_test.txt","test/y_test.txt", cols$MeasureName, filter),
           readData("train/X_train.txt","train/subject_train.txt","train/y_train.txt", cols$MeasureName, filter))

  
  # req3. Uses descriptive activity names to name the activities in the data set
  # req4. Appropriately labels the data set with descriptive activity names. 
  data_labeled <- merge(data, activity_labels, by="ActivityID")

  # remove ActivityID col. as it provides no additional informational value
  col_filter = setdiff(colnames(data_labeled), c("ActivityID"))
  data_labeled <- data_labeled[,col_filter]
  
  # clean up column names
  cnames <- gsub("\\.", colnames(data_labeled), replacement="")
  colnames(data_labeled) <- cnames
  
  
  return(data_labeled)
}

# CreateTidyData uses the melt 
# input is the data set
# Address the requirement:
# req5. Creates a second, independent tidy data set with the average of each variable for each 
#   activity and each subject. 
#
createTidyData<- function(data) {
  library(reshape2)

  # create molten frame 
  id_vars = c("ActivityName", "SubjectID")
  no_id_vars = setdiff(colnames(data), id_vars)
  moltenData <- melt(data, id=id_vars)
  
  # merge out activityName and SubjectID vals + means
  return(dcast(moltenData,ActivityName + SubjectID ~ variable, mean))
}

print("Assuming data files have been extracted under the current directory")
print("expecting working dir to match  \"UCI HAR Dataset\" untouched post archive extraction.")
print("Use setwd() if your working dir does not match the data set directory")
print("Creating  dataset as tidy_UCIHAR.txt...")
write.table(createTidyData(loadData()), "tidy_UCIHAR.txt")
print("Finished.")
