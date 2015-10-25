## Allows to merge text file data in order to produce a tidy data set


##First check for required packages. Important step!
if (!("reshape2" %in% rownames(installed.packages())) ) {
    print("Please install required package \"reshape2\" before proceeding")
} else {
   ##Open this into the library you are running
    library(reshape2)
    
    ## Read text files
    
    ## label columns 
    activity_labels <- read.table("./activity_labels.txt",col.names=c("activity_id","activity_name"))
    
    ## Read dataframe names
    features <- read.table("features.txt")
    feature_names <-  features[,2]
    
    ## Read test data and label the dataframe column
    testdata <- read.table("./test/X_test.txt")
    colnames(testdata) <- feature_names
    
    ## Read the training data repeat column name
    traindata <- read.table("./train/X_train.txt")
    colnames(traindata) <- feature_names
    
   ##THese next lines reads the activities and labels everything as it was
    test_subject_id <- read.table("./test/subject_test.txt")
    colnames(test_subject_id) <- "subject_id"
    
    
    test_activity_id <- read.table("./test/y_test.txt")
    colnames(test_activity_id) <- "activity_id"
    
  
    train_subject_id <- read.table("./train/subject_train.txt")
    colnames(train_subject_id) <- "subject_id"
    
  
    train_activity_id <- read.table("./train/y_train.txt")
    colnames(train_activity_id) <- "activity_id"
    
    ##Combine  test subject id's,  test activity id's 
    ##and  test data 
    test_data <- cbind(test_subject_id , test_activity_id , testdata)
    
   ##does same as above but for train
    train_data <- cbind(train_subject_id , train_activity_id , traindata)
    
    ##ALL DATA
    all_data <- rbind(train_data,test_data)
    
    ##More cleaning for keeping the right columns
    mean_col_idx <- grep("mean",names(all_data),ignore.case=TRUE)
    mean_col_names <- names(all_data)[mean_col_idx]
    std_col_idx <- grep("std",names(all_data),ignore.case=TRUE)
    std_col_names <- names(all_data)[std_col_idx]
    meanstddata <-all_data[,c("subject_id","activity_id",mean_col_names,std_col_names)]
    
    ##Merge 
    descrnames <- merge(activity_labels,meanstddata,by.x="activity_id",by.y="activity_id",all=TRUE)
    
    ##Melt 
    data_melt <- melt(descrnames,id=c("activity_id","activity_name","subject_id"))
    
    ##Cast 
    mean_data <- dcast(data_melt,activity_id + activity_name + subject_id ~ variable,mean)
    
    ## Tidy new file ready to use
    write.table(mean_data,"./tidy_movement_data.txt")
    
}