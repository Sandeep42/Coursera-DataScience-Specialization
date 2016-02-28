setwd("C:/Users/Sandeep/Notebooks/Coursera/DataScience/Practical Machine Learning")
fileUrl <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
download.file(fileUrl, destfile = "train.csv")
fileUrl2 <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'
download.file(fileUrl2, destfile = "test.csv")
df_train <- readLines("train.csv", n=10)
df <- read.csv(file = "train.csv", header = TRUE)
head(df)                     
table(df$classe)
colnames(df)
summary(df$roll_belt)
sapply(df,class)
require(ggplot2)
qplot(x= df$classe, y=df$avg_roll_belt, geom= "boxplot", col= df$classe)
qplot(x= df$classe, y=df$avg_yaw_arm, geom= "boxplot", col= df$classe)
qplot(x= df$classe, y=df$avg_pitch_belt, geom= "boxplot", col= df$classe)
qplot(x= df$classe, y=df$avg_pitch_arm, geom= "boxplot", col= df$classe)
# Too much of variablitiy, outliers
# PCA?
require(caret)
trainIndex <- createDataPartition(df$classe, p= 0.7, list = FALSE)
trainData <- df[trainIndex,]
testData <- df[-trainIndex,]
# Cleaning the data
clean <- grep("name|timestamp|window|X", colnames(trainData))
trainData <- trainData[, -clean]
goodColumns <- !apply(trainData, 2,
                      function(x) sum(is.na(x)) > (dim(trainData)[1]) * 0.95 || 
                        sum(x=="") > length(dim(trainData)[1]) * 0.95)
trainingClean <- trainData[,goodColumns]
# 160 to 93 variable, remaining ones are with more than 95% missing values
testData <- testData[, -clean]
goodColumnsTest <- !apply(testData, 2,
                      function(x) sum(is.na(x)) > (dim(testData)[1]) * 0.95 || 
                        sum(x=="") > length(dim(testData)[1]) * 0.95)
testDataClean <- testData[, goodColumnsTest]
# testData <- testData[ lapply( testData, function(x) sum( is.na(x) ) / length(x)) < 0.05]
# We'll do PCA, you have to exclude the response
trainDataPCA <- preProcess(trainingClean[,1:52], method = "pca", thresh = 0.95)
trainDataPCA
# PCA needed 25 components to capture 95 percent of the variance
# PCA needed 40 components to capture 99 percent of the variance
# testDataPCA <- preProcess(testData, method = "pca", thresh = 0.95), we apply the same PCA dumbass!
# testDataPCA
# rm(testDataPCA)
# Train and test data cleaned
trainPCA <- predict(trainDataClean, trainingClean[,1:52])
# rm(trainDataPCA)
# rm(trainingPCA)
rm(testDataPCA)
rm(testPCA)
testPCA <- predict(trainDataPCA, testDataClean[,1:52])
# Great, we have both train PCA and test PCA

# GBM never seems to converge

install.packages("gbm")
require(gbm)
trainPCA$classe <- trainingClean$classe
set.seed(11)
fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)
gbmModel <- train(x = trainPCA[,1:26], y= as.factor(trainPCA$classe), method = "gbm", 
                  trControl = fitControl)


# We'll do a plot
# PCA with two components
pcaex <- preProcess(trainingClean[,1:52], method = "pca", thresh = 0.7)
pcaex
pcaEx <- predict(pcaex, trainingClean[,1:52])
library(ggplot2)
qplot(x= PC1, y= PC2, col= trainPCA$classe, data = pcaEx)
library(parallel)
install.packages("doParallel")
library(doParallel)
cl <- makeCluster(detectCores() - 4)
registerDoParallel(cl)
ctrl <- trainControl(classProbs=TRUE,
                     savePredictions=TRUE,
                     allowParallel=TRUE)

### Models, Random Forests(
model1 <- train(classe ~ ., data=trainPCA, method="gbm")
model2 <- train(classe ~ ., data=trainPCA, method="rf")
library(caret)
testPCA$predict <- predict(model1, testPCA[,1:25])
confusionMatrix(testData$classe, testPCA$predict)
testPCA$predictrf <- predict(model2, testPCA[,1:25])
confusionMatrix(testData$classe, testPCA$predictrf)
# RF performed just fine.
df_test <- read.csv("test.csv", header = TRUE)
df_clean <- df_test[, -clean]
goodColumnsDF <- !apply(df_clean, 2,
                      function(x) sum(is.na(x)) > (dim(df_clean)[1]) * 0.95 || 
                        sum(x=="") > length(dim(df_clean)[1]) * 0.95)
testClean <- df_clean[, goodColumnsDF]
testCleanPCA <- predict(trainDataPCA, testClean[,1:52])
testCleanPCA$predict_rf <- predict(model2, testCleanPCA[,1:25])
answers = testCleanPCA$predict_rf
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
save(model2, file = "randomModel.rda")
