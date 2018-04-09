

# Main entry point method that, as specified on the Course project, parses, cleans and formats the 
# data in such a way to generate a report which consist on the means of each std() and mean() variables
# of each measurement per activity and per subject.
# Parameters: 
#  -dataSetLocation: String containing the base location of all the data and where the final_data_set file
#  would be generated.
run_analysis<-function(dataSetLocation){
  library(stringr)
  #Create the paths to both training set and test set
  trainingSetLocation <- paste(dataSetLocation,"train",sep = "\\")
  message(paste("Training set location",trainingSetLocation))
  testSetLocation <- paste(dataSetLocation,"test",sep = "\\")
  message(paste("Test set location",testSetLocation))
  
  #Proceed to load the features of the measurements
  featureList <- load_variables_dictionary(dataSetLocation)
  #Proceed to load the y vector labels
  activityLabelList <-load_activity_dictionary(dataSetLocation)
  
  # First load the training data
  trainingData <- load_target_data(trainingSetLocation,featureList,activityLabelList,"train")
  # The load the test data
  testData <- load_target_data(testSetLocation,featureList,activityLabelList,"test")
  # Merge the data into memory
  consolidatedData <-rbind(trainingData,testData)
  
  message("Global data frame merged")
  #message("Dumping the consolidated data set...")
  #write.csv(consolidatedData,paste(dataSetLocation,"totalData.csv",sep = "\\"),row.names = FALSE)
  #message("Consolidated data set dumped as backup")
  
  #Proceed to generate the summary data set
  generate_analysis_report(dataSetLocation,consolidatedData)
  
}

# Method to load the column names used on both training and test sets.
# Parameters: 
#  -dataSetLocation: String containing the base location of all the data and where the final_data_set file
#  would be generated.
load_variables_dictionary <- function(dataSetLocation){
  
  #Construct the location of the file that contains the variable names
  featureFileName <- paste(dataSetLocation,"features.txt",sep = "\\")
  message(paste("Location of the variable dictionary",featureFileName))
  
  featureData <- read.table(featureFileName,sep = " ")
  message("Variable dictionary loaded")
  featureData[[2]]
}

# Method to load the dictionary for the data contained in the Y vector of the target data
# Parameters: 
#  -dataSetLocation: String containing the base location of all the data and where the final_data_set file
#  would be generated.
load_activity_dictionary<-function(dataSetLocation){
  
  #Construct the location of the file that contains the activity labels
  activityFileName <- paste(dataSetLocation,"activity_labels.txt",sep = "\\")
  message(paste("Location of the activity dictionary",activityFileName))
  
  activityData <- read.table(activityFileName,sep = " ")
  message("Activity dictionary loaded")
  activityData[[2]]
}

# Method to load all the data with its corresponding variable names
# Parameters: 
#  -dataSetLocation: String containing the base location of all the data and where the final_data_set file
#  would be generated.
#  -variableDictionary: Collection containing all the column names for the actual data measurements.
#  -labelDictionary: Labels previously loaded from the activity_labels.txt for activity data encoding.
#  -setType: String containing either test or train in order to process the entire data file set to
#  generate the report.
load_target_data <- function(dataSetLocation,variableDictionary,labelDictionary,setType){
  
  # Construct required file names
  xFile <- paste(paste(dataSetLocation,paste("X_",setType,sep = ""),sep = "\\"),".txt",sep = "")
  message(paste("Calculated X data file",xFile))
  
  yFile <- paste(paste(dataSetLocation,paste("y_",setType,sep = ""),sep = "\\"),".txt",sep = "")
  message(paste("Calculated y data file",yFile))
  
  subjectFile <- paste(paste(dataSetLocation,paste("subject_",setType,sep = ""),sep = "\\"),".txt",sep = "")
  message(paste("Calculated subject file",subjectFile))
  
  #First we need to format the x data file in order to be able to read the table
  formattedXFile<-format_x_data(xFile)
  
  targetXData <- read.table(formattedXFile,sep = " ")
  # Generate the data classes for better parsing and future processing.
  xDataClasses <- rep("numeric",length(variableDictionary))
  xTargetData <- read.table(formattedXFile, sep = " ", col.names = variableDictionary,colClasses = xDataClasses ) 
  
  # Now load the data from the Y file
  message("Loading data from Y file...")
  yRawData <- read.table(yFile,col.names = c("Activity"),colClasses = c("integer"))
  yTargetData <-factor(yRawData[[1]],labels = labelDictionary)
  message("Y Data formatted and loaded.")
  
  # Now load the data of the subject file
  message("Loading data from the subject file...")
  subjectRawData <- read.table(subjectFile,col.names = c("Observed.Subject"),colClasses = c("integer"))
  message("Subject data loaded.")
  
  #  Proceed to unify all the data
  
  # Append the X data based on the mean and std deviation columns only
  selectedXColumns <- grep("std|mean",variableDictionary)
  targetData <- xTargetData[,selectedXColumns]
  # Append the Activity data
  targetData$Activity <- yTargetData
  # Append the Subject data
  targetData$Subject <- subjectRawData[[1]]
  # Return the data
  targetData
}

# Method to format properly the data in order to be parseable as a table for data treatment. Once
# formatted, then it will store the file again in a temp file and will return the path.
# Parameters: 
#  -xFile: String that contains the location of the X file of each data set.
format_x_data <-function(xFile){
  
  xDataConnection<-file(xFile,"r")
  xData <- readLines(xDataConnection)
  close.connection(xDataConnection)
  message("Source X Data loaded")
  message("Formatting Data properly...")
  formatedXData<- str_trim(xData)
  formatedXData <- gsub("  "," ",formatedXData)
  message("Source X Data formatted")
  
  xFormattedFile <- paste(xFile,"formatted",sep = ".")
  xFormattedConnection <- file(xFormattedFile,"w")
  # Now open the connection to store the formatted data
  message(paste("Writing formatted x Data at",xFormattedFile))
  writeLines(formatedXData,xFormattedConnection)
  message("Formatted X Data written.")
  #Close the connection
  close.connection(xFormattedConnection)
  xFormattedFile
}

# Method to generate the summary required for point # 5 in the project
# Parameters: 
#  -sourceDataSet: Data frame containing all the consolidated information after data cleaning.
#  -dataSetLocation: String containing the base location of all the data and where the final_data_set file
#  would be generated.
generate_analysis_report<-function(dataSetLocation,sourceDataSet){

  # First create the analysis data frame to store the results
  resultColumnNames <- names(sourceDataSet)
  summaryDataFrame <- read.table(text = "",col.names = resultColumnNames)
  
  #Extract the different subjects on the data
  targetSubjects <-sort(unique(sourceDataSet$Subject))
  message(paste("Detected subjects to generate the summary:",length(targetSubjects)))

  for(targetSubject in targetSubjects){
    message(paste("Generating the summary for the subject",targetSubject))
    subjectDataFrame <- generate_subject_analysis(sourceDataSet,targetSubject,resultColumnNames)
    #Append the resulting data frame from the subject
    summaryDataFrame <-rbind(summaryDataFrame,subjectDataFrame)
  }
  
  message("Analysis summary completed.")
  
  #Proceed to dump the final analysis report
  summaryFile<-paste(dataSetLocation,"final_data_set.csv",sep = "\\")
  write.csv(summaryDataFrame,file = summaryFile,row.names = FALSE)
  message(paste("Summary data set dumped at",summaryFile))
}

# Method to generate the analysis for a given individual on the resulting data set
# Parameters: 
#  -srcDataSet: Data frame containing all the consolidated information after data cleaning.
#  -targetSubject: Integer from 1 to n indicating which subject is being processed for summary generation
#  -targetVariables: Collection of all the column names being processed for summary generation
generate_subject_analysis<-function(srcDataSet,targetSubject,targetVariables){
  
  #Generate the subject data frame
  subjectDataFrame <- read.table(text = "",col.names = targetVariables)
  
  #First proceed to filter out the data from the target subject
  subjectData <-  srcDataSet[srcDataSet$Subject == targetSubject,]
  message("Subject Data filtered.")
  
  targetActivities <- sort(unique(subjectData$Activity))
  message(paste("Detected different activities:",length(targetActivities)))
  nonProcessingColumns <-c("Activity")
  #Proceed to analyze each activity
  for(targetActivity in targetActivities){
    
    message(paste("Processing activity",targetActivity))
    activityData <-subjectData[subjectData$Activity == targetActivity,]
    calculableData<-activityData[,1:(length(targetVariables)-2)]
    calculableData <-t(colMeans(calculableData))
    #append the activity
    calculableData <- cbind(calculableData,Activity=c(targetActivity),Subject=c(targetSubject))
    #Finally append the data frame into the subject one
    subjectDataFrame <- rbind(subjectDataFrame,calculableData)
  }
  
  subjectDataFrame
  
}
