# Hello there! Reading commented code is quite difficult, I have generated a 
# knitr document explaining most of the steps. I would urge you look into that 
#  file inthe same repository.


fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url = fileUrl, destfile = "./UCIHAR.zip")
unzip("./UCIHAR.zip")

# Merge data files
train <- read.table("./UCI HAR Dataset/train/X_train.txt")
test <- read.table("./UCI HAR Dataset/test/X_test.txt")
merged.df <- rbind(train, test)

# Mean of each column
means <- colMeans(merged.df, na.rm = TRUE)
# means[1]

# SD
sds <- apply(merged.df, 2, sd)
# sds[500]

# Read responses

response.train <- read.table("./UCI HAR Dataset/train/y_train.txt")
response.test <- read.table("./UCI HAR Dataset/test/y_test.txt")
activity.merged <- rbind(response.train, response.test)


labeler <- function(x) {
  if(x==1) return("WALKING")
  if (x==2) return("WALKING_UPSTAIRS")
  if (x==3) return("WALKING_DOWNSTAIRS")
  if (x==4) return("SITTING")
  if (x==5) return("STANDING")
  if (x==6) return("LAYING")
}


activity.labeled <- apply(activity.merged, 1, labeler)
merged.df$activity.name = activity.labeled


# head(merged.df$activity.name, n=5)

# Step 4: Appropriately labels the data set with descriptive variable names


names.df <- read.table("./UCI HAR Dataset/features.txt")
listnames <- as.character(names.df$V2)
names(merged.df) <- c(listnames, "ActivityName")
names(merged.df)

# Step 5: From the data set in step 4, creates a second, independent tidy data 
# set with the average of each variable for each activity and each subject.

# We have nice merged, cleaned data frame. Oh I didn't read the subject data.

subject.train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
subject.test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
merged.subject <- rbind(subject.train, subject.test)
merged.df$SubjectNumber <- merged.subject

# Now, we have all the data within one merged.df dataframe. Awesome isn't it?

ave.aggregate.activity <- aggregate(merged.df, FUN= mean, by= list(merged.df$ActivityName),
                                     formula= .~ ActivityName, na.rm = TRUE)
ave.aggregate.subject <-  aggregate(merged.df, FUN= mean, by= merged.df$SubjectNumber, formula = . ~ SubjectNumber, na.rm = TRUE)

colnames(ave.aggregate.activity)[1] <- "GroupType"
colnames(ave.aggregate.subject)[1] <- "SubjectNumber"

write.table(ave.aggregate.activity, file = "./avg.activity", row.names = FALSE)
write.table(ave.aggregate.subject, file = "./avg.subject", row.names = FALSE)

