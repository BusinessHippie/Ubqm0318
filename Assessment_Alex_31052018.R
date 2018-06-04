#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(stats)
library(openair)
library(scales)
library(ggplot2)
library(reshape2)
library(forecast)
library(data.table)
library(plotly)
library(discretization)
library(arules)
library(caret)
library(tidyverse)
library(doParallel)
#Load file
Assess<-read.csv(file = "C:/Users/alexw/Desktop/Ubiqum/assesment_Alex.csv",header = F,sep = " ");Assess<-na.omit(Assess)
#Split column into 5 columns by space
#Assessment<-separate(Assess,sep = " ",into =c("X1","X2","X3","X4","X5"), remove = T,convert = T)
Assess$V5<-as.factor(Assess$V5)
#Eliminate Outliers
outliers_V1<-boxplot(Assess$V1)$out
Assess<-Assess[-which(Assess$V1 %in% outliers_V1),]
outliers_V4<-boxplot(Assess$V4)$out
Assess<-Assess[-which(Assess$V4 %in% outliers_V4),]
#_______________________________________________kNN_Model________________________________________
set.seed(123)
TrainData<-createDataPartition(y=Assess$V5,p=0.75,list=FALSE)
training<-Assess[TrainData,]
testing<-Assess[-TrainData,]
#
Ctrl<-trainControl(method = "repeatedcv",#This_is_CROSS_VALIDATION
                   number = 5,
                   repeats = 5,
                   verboseIter = TRUE)
                   #classProbs = TRUE)
#
set.seed(111)
knnFit<-train(V5~.,#alternative_being:_(brand~.,)
              data = training,
              method = "knn",
              trControl=Ctrl,
              tuneLength = 5, #,
              preProc = c("center", "scale"))
knnFit
#
#Predict
kNN_Predict<-predict(knnFit,newdata = testing)
#str(kNN_Predict)
#plot(knnFit)
kNN_Predict
kNN_Probs<-predict(knnFit,newdata = testing,type = "prob")
head(kNN_Probs)
confusionMatrix(data = kNN_Predict,testing$V5)
#
postResample(kNN_Predict,testing$V5)
#______________________________________________RANDOM_FOREST___________________________________________
set.seed(111)
RF_Fit<-train(V5~.,#alternative_being:_(brand~.,)
              data = training,
              method = "rf",
              ntree = 10,
              #metric = "Accuracy",
              trControl=Ctrl,
              tuneLength = 3,
              preProc = c("center", "scale"))
RF_Fit
#plot(RF_Fit)
#Predict
RF_Predict<-predict(RF_Fit,newdata = testing)
#str(RF_Predict)
RF_Probs<-predict(RF_Fit,newdata = testing,type = "prob")
head(RF_Probs)
confusionMatrix(data = RF_Predict,testing$V5)
#
postResample(RF_Predict,testing$V5)
