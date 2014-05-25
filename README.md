GettingAndCleaningDataCourseProject
===================================

################################# ################################# ## Getting and Cleaning Data  ## Course Project:   ########## ## You must be in a correct working directory (with dowloaded data files) ########## ## Reading train data: data.train &lt;- read.table("./UCI HAR Dataset/train/X_train.txt", nrows=10) dat.train.classes &lt;- sapply(data.train,class) ## Reading variable names: data.names &lt;- read.table("./UCI HAR Dataset/features.txt") ## Reading training subjects: sub.train &lt;- read.table("./UCI HAR Dataset/train/subject_train.txt",                         col.names="subject") # table(sub.train) # str(sub.train)  ## Descriptive activity names: activity.train &lt;- read.table("./UCI HAR Dataset/train/y_train.txt",                              colClasses="factor",col.names="descriptive.activity.names") levels(activity.train[,1]) &lt;- read.table("./UCI HAR Dataset/activity_labels.txt",                                          colClasses=c("numeric","character"))[,2] # table(activity.train) rm(data.train) data.train.dt &lt;- read.table("./UCI HAR Dataset/train/X_train.txt",                              colClasses=dat.train.classes, col.names=data.names[,2]) data.train &lt;- cbind(sub.train,activity.train,data.train.dt)   ########## ## Reading test data: data.test &lt;- read.table("./UCI HAR Dataset/test/X_test.txt", nrows=10) dat.test.classes &lt;- sapply(data.test,class) ## Reading variable names (is the same as is at training data): # data.names &lt;- read.table("./UCI HAR Dataset/features.txt")  ## Reading testing subjects: sub.test &lt;- read.table("./UCI HAR Dataset/test/subject_test.txt",                        col.names="subject") # table(sub.test) # str(sub.test)  ## Descriptive activity names: activity.test &lt;- read.table("./UCI HAR Dataset/test/y_test.txt",                             colClasses="factor",col.names="descriptive.activity.names") levels(activity.test[,1]) &lt;- read.table("./UCI HAR Dataset/activity_labels.txt",                                         colClasses=c("numeric","character"))[,2] # table(activity.test) rm(data.test) data.test.dt &lt;- read.table("./UCI HAR Dataset/test/X_test.txt",                             colClasses=dat.test.classes, col.names=data.names[,2]) data.test &lt;- cbind(sub.test,activity.test,data.test.dt)   ########## ## Merging data: # dim(data.train) # dim(data.test) data &lt;- rbind(data.train,data.test) # str(data) # dim(data)   ########## ## Extracting only the measurements on the mean and standard deviation  ## for each measurement: # Find variables that contain "mean" or "std": index &lt;- grep("mean|std",names(data)) # ectract data.frame: extract.data &lt;- data[,c(1,2,index)]  # names(extract.data) # str(extract.data)   ########## ### Creating tidy data set with the average of each variable  ### for each activity and each subject:  # Creating factor for splitting: factor.data &lt;- interaction(extract.data$subject,extract.data$descriptive.activity.names) str(factor.data);length(factor.data) # table(factor.data) # str(factor.data)  # Splitting: split.extract.data &lt;- split(extract.data,factor.data)  # length(split.extract.data) # str(split.extract.data[[2]])  # Calculating the mean for every subset of list "split.extract.data" and # transfer results to the data.frame "mean.extract.data": mean.extract.data &lt;- as.data.frame(t(rep(NA,dim(extract.data)[2]))) for(i in seq_along(split.extract.data)){       mean.extract.data[i,] &lt;- data.frame(split.extract.data[[i]][1,c(1,2)],                                           t(sapply(split.extract.data[[i]][,-c(1,2)],mean))) } # Writting the names of the variables names(mean.extract.data) &lt;- names(extract.data) str(mean.extract.data)  # Appropriately labelling the data set with descriptive activity names: mean.extract.data[,2] &lt;- as.factor(mean.extract.data[,2]) levels(mean.extract.data[,2]) &lt;- read.table("./UCI HAR Dataset/activity_labels.txt",                                             colClasses=c("numeric","character"))[,2] # str(mean.extract.data)  # ordering the data.set by subject's id: mean.extract.data.end &lt;- mean.extract.data[order(mean.extract.data[,1],                                                  decreasing=FALSE),]  # saving data.frame: write.csv(mean.extract.data.end,file="tidy.data.set.csv",           row.names = FALSE)