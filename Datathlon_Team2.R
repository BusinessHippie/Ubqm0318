#2 csv files
Sentiment<-read.csv(file = "C:/Users/alexw/Desktop/Datathlon 110618/train.csv",header = TRUE)
Sentiment_orig<-read.csv(file = "C:/Users/alexw/Desktop/Datathlon 110618/train.csv",header = TRUE)
Sentiment_test<-read.csv(file = "C:/Users/alexw/Desktop/Datathlon 110618/test.csv",header = TRUE)
#Load_packages
#library(lubridate)
library(dplyr)
library(tidyr)
library(stats)
#library(openair)
#library(scales)
library(ggplot2)
library(reshape2)
#library(forecast)
library(data.table)
#library(plotly)
#library(discretization)
library(arules)
library(caret)
library(tidyverse)
hist(Sentiment$iphoneSentiment,main = "iPhoneSentiment")
summary(Sentiment$iphoneSentiment)
Sentiment<-na.omit(Sentiment)
Sentiment_orig<-na.omit(Sentiment_orig)
Sentiment$id<-NULL
Sentiment_orig$id<-NULL
Sentiment<-distinct(Sentiment)
# Sentiment$Mean<-rowMeans(Sentiment,na.rm = T)
# Sentiment<-filter(Sentiment,Mean!=0)
# Sentiment$Mean<-NULL
#Remove extreme outliers
Out_iPhone<-boxplot(Sentiment$iphoneSentiment)$out
Sentiment[which(abs(Sentiment$iphoneSentiment)>1000),]
Sentiment<-Sentiment[-which(abs(Sentiment$iphoneSentiment)>1000),]
#Sentiment<-Sentiment[-which(Sentiment$iphoneSentiment %in% Out_iPhone),]
####Correlation####
corrplot::corrplot(cor(Sentiment[1:60]),type = "upper")
corrplot::corrplot(cor(Sentiment_orig[c(1:20,59:60)]),type = "upper")
corrplot::corrplot(cor(Sentiment_orig[c(21:40,59:60)]),type = "upper")
corrplot::corrplot(cor(Sentiment_orig[c(41:60)]),type = "upper")
corrplot::corrplot(cor(Sentiment[c(1:20,59:60)]),type = "upper")
corrplot::corrplot(cor(Sentiment[c(21:40,59:60)]),type = "upper")
corrplot::corrplot(cor(Sentiment[c(41:60)]),type = "upper")
#columns that start with "i" in the testing set
grep(pattern="^i",names(Sentiment_test))
colnames(Sentiment_test[grep(pattern="^i",names(Sentiment_test))])
#columns that start with "i" in the training set
grep(pattern="^i",names(Sentiment))
colnames(Sentiment[grep(pattern="^i",names(Sentiment))])
#
test_attributes<-names(Sentiment_test[,c( 6, 32, 37, 42, 47, 49, 51,52)])
training_attributes<-names(Sentiment[,c( 6, 32, 37, 42, 47, 49, 51,52)])
corrplot::corrplot(cor(Sentiment_test[test_attributes]),type="upper")
corrplot::corrplot(cor(Sentiment[training_attributes]),type="upper")
#iPhoneBins
iPhoneBins <- discretize(Sentiment$iphoneSentiment, "fixed", categories= c(-Inf, -50, -10, -1, 1, 10, 50, Inf))
Sentiment$iPhoneBins<-iPhoneBins
#GalaxyBins
#GalaxyBins <- discretize(Sentiment$galaxySentiment, "fixed", categories= c(-Inf, -50, -10, -1, 1, 10, 50, Inf))
#Sentiment$GalaxyBins<-GalaxyBins
#
levels(Sentiment$iPhoneBins)<-c("poor","bad","below average","neutral","above average","good","outstanding")
#levels(Sentiment$GalaxyBins)<-c("poor","bad","below average","neutral","above average","good","outstanding")
#Remove training set columns that are not part of the testing set
which(colnames(Sentiment)%in%colnames(Sentiment_test)=="FALSE")
#Keep the iPhoneSentiment Column
which(colnames(Sentiment)%in%colnames(Sentiment_test)=="FALSE")[1:7]
colnames(Sentiment[which(colnames(Sentiment)%in%colnames(Sentiment_test)=="FALSE")[1:7]])
Sentiment<-Sentiment[-which(colnames(Sentiment)%in%colnames(Sentiment_test)=="FALSE")[1:7]]
#### MODELS ####
#Remove iPhoneSentiment from Trainingdata
Sentiment1<-Sentiment[c(1:51,53,54)]
#Take out super highly correlated columns
corsent1<-cor(Sentiment1[1:52])
corsent2<-findCorrelation(cor(Sentiment1[1:52]),cutoff = .80)
Sentiment1<-Sentiment1[-corsent2]
#Models________________________________________________KNN______________________________________________________________
set.seed(123)
TrainSet_Sent<-createDataPartition(Sentiment1$iPhoneBins,p=0.75,list = F)
training_Sent<-Sentiment1[TrainSet_Sent,]
testing_Sent<-Sentiment1[-TrainSet_Sent,]
#
Ctrl_Sent<-trainControl(method = "repeatedcv",
                        number = 2,
                        repeats = 3,
                        verboseIter = TRUE)
Ctrl_Sent
#kNN Model
set.seed(111)
KNNiphone<- train(iPhoneBins~. ,
                  data= Sentiment1,
                  method= "knn",
                  trControl = Ctrl_Sent)
#
Predict_iphone<-predict(KNNiphone, testing_Sent)
Predict_iphone
confusionMatrix(testing_Sent$iPhoneBins, Predict_iphone)
postResample(Predict_iphone,testing_Sent$iPhoneBins)
Predict_iphone_1<-predict(KNNiphone,newdata = Sentiment_test)
Predict_iphone_1
#### WORK Submission####
wd <- "C:\Users\alexw\Google Drive\Team2"
setwd(wd)
source("ubiqum_datathon.R")
function.wd <- "C:\Users\alexw\Google Drive\Team2"
team <- "Team2"
vector <- Predict_iphone_1
confusion <- ubiqum_datathon(team, vector, function.wd)
