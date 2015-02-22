## You should create one R script called run_analysis.R that does the following:
run_analysis <- function(){

library("dplyr") 

original_wd <- getwd()

## 1. Merges the training and the test sets to create one data set.

## Read all the data to local variables
setwd("./test")
subject_test <- read.table("subject_test.txt", header=TRUE, sep="")
x_test <- read.table("X_test.txt", header=TRUE, sep="")
y_test <- read.table("y_test.txt", header=TRUE, sep="")

setwd("./../train")
subject_train <- read.table("subject_train.txt", header=TRUE, sep="")
x_train <- read.table("X_train.txt", header=TRUE, sep="")
y_train <- read.table("y_train.txt", header=TRUE, sep="")

# merge  data
test_data <- cbind(subject_test, y_test, x_test)
train_data <- cbind(subject_train, y_train, x_train)
names(test_data) <- names(train_data)
raw_data <- rbind(test_data, train_data)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
measurements <- c(3:8, 43:48, 83:88, 123:128, 163:168, 203:204, 216:217, 229:230, 242:243, 255:256, 268:273, 247:252, 426:431, 505:506, 518:519, 531:532, 544:545)
sel_data <- data.frame(raw_data[,c(1:2, measurements)])
#write(sel_data_frame, file="sel_data.txt")

## 3. Uses descriptive activity names to name the activities in the data set
setwd("./..")
activity <- read.table("activity_labels.txt", header=FALSE, sep="")
activity_names <- as.character(activity[,2])


n<-names(sel_data)
n[2]<-"V2"
names(sel_data)<-n

sel_data$V2[sel_data$V2 == 1] <- activity_names[1] #"Walking_upstairs"
sel_data$V2[sel_data$V2 == 2] <- activity_names[2] #"Walking_upstairs"
sel_data$V2[sel_data$V2 == 3] <- activity_names[3] #"Walking_downstairs"
sel_data$V2[sel_data$V2 == 4] <- activity_names[4] #"Sitting"
sel_data$V2[sel_data$V2 == 5] <- activity_names[5] #"Standing"
sel_data$V2[sel_data$V2 == 6] <- activity_names[6] #"Laying"

## 4. Appropriately labels the data set with descriptive variable names. 

features <- read.table("features.txt", header=FALSE, sep="")
names_features <- as.character(features[measurements-2,2])
names(sel_data) <- c("Subject", "Activity", names_features)

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Create an empty data frame and get the variables names
tidy_data <- data.frame(matrix(data=NA, nrow=180, ncol=68))
names(tidy_data) <- names(sel_data)

# Write the first two columns
tidy_data[,1] <- rep(1:30, each = 6)
tidy_data[,2] <- rep(activity_names, times <- 30)

# Calculate the mean values for each activity for each subject for each variable
for(j in 1:30){
	for(k in 3:(ncol(tidy_data))){
		for(l in 1:6){
			suma <- sum(sel_data[(sel_data[,1]==j)&(sel_data[,2]==activity_names[l]),k])
			long <- length(sel_data[(sel_data[,1]==j)&(sel_data[,2]==activity_names[l]),k])
			tidy_data[6*(j-1)+l,k] <- suma/long		
		}		
	}
}

setwd(original_wd)
write.table(tidy_data, file="tidy_data.txt", row.name=FALSE)
}