#################################
#################################
## Getting and Cleaning Data 
## Course Project:


##########
## You must be in a correct working directory (with dowloaded data files)
##########
## Reading train data:
data.train <- read.table("./UCI HAR Dataset/train/X_train.txt", nrows=10)
dat.train.classes <- sapply(data.train,class)
## Reading variable names:
data.names <- read.table("./UCI HAR Dataset/features.txt")
## Reading training subjects:
sub.train <- read.table("./UCI HAR Dataset/train/subject_train.txt",
                        col.names="subject")
# table(sub.train)
# str(sub.train)

## Descriptive activity names:
activity.train <- read.table("./UCI HAR Dataset/train/y_train.txt",
                             colClasses="factor",col.names="descriptive.activity.names")
levels(activity.train[,1]) <- read.table("./UCI HAR Dataset/activity_labels.txt",
                                         colClasses=c("numeric","character"))[,2]
# table(activity.train)
rm(data.train)
data.train.dt <- read.table("./UCI HAR Dataset/train/X_train.txt", 
                            colClasses=dat.train.classes, col.names=data.names[,2])
data.train <- cbind(sub.train,activity.train,data.train.dt)


##########
## Reading test data:
data.test <- read.table("./UCI HAR Dataset/test/X_test.txt", nrows=10)
dat.test.classes <- sapply(data.test,class)
## Reading variable names (is the same as is at training data):
# data.names <- read.table("./UCI HAR Dataset/features.txt")

## Reading testing subjects:
sub.test <- read.table("./UCI HAR Dataset/test/subject_test.txt",
                       col.names="subject")
# table(sub.test)
# str(sub.test)

## Descriptive activity names:
activity.test <- read.table("./UCI HAR Dataset/test/y_test.txt",
                            colClasses="factor",col.names="descriptive.activity.names")
levels(activity.test[,1]) <- read.table("./UCI HAR Dataset/activity_labels.txt",
                                        colClasses=c("numeric","character"))[,2]
# table(activity.test)
rm(data.test)
data.test.dt <- read.table("./UCI HAR Dataset/test/X_test.txt", 
                           colClasses=dat.test.classes, col.names=data.names[,2])
data.test <- cbind(sub.test,activity.test,data.test.dt)


##########
## Merging data:
# dim(data.train)
# dim(data.test)
data <- rbind(data.train,data.test)
# str(data)
# dim(data)


##########
## Extracting only the measurements on the mean and standard deviation 
## for each measurement:
# Find variables that contain "mean" or "std":
index <- grep("mean|std",names(data))
# ectract data.frame:
extract.data <- data[,c(1,2,index)]

# names(extract.data)
# str(extract.data)


##########
### Creating tidy data set with the average of each variable 
### for each activity and each subject:

# Creating factor for splitting:
factor.data <- interaction(extract.data$subject,extract.data$descriptive.activity.names)
str(factor.data);length(factor.data)
# table(factor.data)
# str(factor.data)

# Splitting:
split.extract.data <- split(extract.data,factor.data)

# length(split.extract.data)
# str(split.extract.data[[2]])

# Calculating the mean for every subset of list "split.extract.data" and
# transfer results to the data.frame "mean.extract.data":
mean.extract.data <- as.data.frame(t(rep(NA,dim(extract.data)[2])))
for(i in seq_along(split.extract.data)){
      mean.extract.data[i,] <- data.frame(split.extract.data[[i]][1,c(1,2)],
                                          t(sapply(split.extract.data[[i]][,-c(1,2)],mean)))
}
# Writting the names of the variables
names(mean.extract.data) <- names(extract.data)
str(mean.extract.data)

# Appropriately labelling the data set with descriptive activity names:
mean.extract.data[,2] <- as.factor(mean.extract.data[,2])
levels(mean.extract.data[,2]) <- read.table("./UCI HAR Dataset/activity_labels.txt",
                                            colClasses=c("numeric","character"))[,2]
# str(mean.extract.data)

# ordering the data.set by subject's id:
mean.extract.data.end <- mean.extract.data[order(mean.extract.data[,1],
                                                 decreasing=FALSE),]

# saving data.frame:
write.csv(mean.extract.data.end,file="tidy.data.set.csv",
          row.names = FALSE)

