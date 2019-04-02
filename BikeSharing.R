####Load libraries####
library(lubridate)
library(dplyr)
library(tidyr)
library(stats)
#library(openair)
#library(scales)
library(ggplot2)
library(reshape2)
library(forecast)
library(data.table)
#library(plotly)
#library(discretization)
library(arules)
library(caret)
library(tidyverse)
library(corrplot)

BikeShare <- read.csv(file="C:/Users/AlexanderWinkler/Documents/R/hour.csv",header = TRUE)

BikeShare$dteday <- as.POSIXct(BikeShare$dteday)
table(is.na(BikeShare))
#BikeShare <- na.omit(BikeShare)

str(BikeShare)
summary(BikeShare)

colnames(BikeShare)[2] <- "Date"
length(colnames(BikeShare))

#Check for outliers of bike rental numbers
boxplot(BikeShare$casual)$out
boxplot(BikeShare$registered)$out
boxplot(BikeShare$cnt)$out

Outliers <- boxplot.stats(BikeShare$cnt)
Outliers_cas <- boxplot.stats(BikeShare$casual)
Outliers_reg <- boxplot.stats(BikeShare$registered)

summary(BikeShare$cnt)

Outliers$stats
Outliers$out
Outliers_reg$out
#Get respective indices (positions) of the outliers
a <- which(BikeShare$cnt %in% Outliers$out)

Outliers_cas$out
#Get respective indices (positions) of the outliers of casual rentals
b <- which(BikeShare$casual %in% Outliers_cas$out)

which(b %in% a)
#Take out the outliers that overlap in total and casual bike rentals
BikeShare <- BikeShare[-which(b %in% a),]

 #BikeShare[,-c(2,3,4,5,6,7)]

#Convert the temperatures back to standard temperatures
BikeShare$temp <- round(BikeShare$temp * 39,digits = 0)
BikeShare$atemp <- round(BikeShare$atemp * 50, digits = 0)

#Create Weekday DF
Weekdays <- c("Monday","Tuesday","Wednsday","Thursday","Friday","Saturday","Sunday")
WeekdayNo <- c(1,2,3,4,5,6,0)
WeekdayDF <- data.frame(Weekdays,WeekdayNo)

colnames(BikeShare)[colnames(BikeShare) == "weekday"] <- "WeekdayNo"
colnames(BikeShare)[colnames(BikeShare) == "hr"] <- "Hour"

BikeShare <- merge(BikeShare, WeekdayDF, by = "WeekdayNo")
#rm(BikeShare1)

BikeShare$Weekdays <- as.factor(BikeShare$Weekdays)
BikeShare$Weekdays <- ordered(BikeShare$Weekdays,levels = c("Monday","Tuesday","Wednsday","Thursday","Friday","Saturday","Sunday"))
####Daily effects on use####
Daily_Bikes <- BikeShare %>% 
                group_by(Weekdays = BikeShare$Weekdays) %>% 
                summarise(MeanDay = round(mean(cnt),digits = 0),
                          Mean_cas = round(mean(casual),digits = 0),
                          Mean_reg = round(mean(registered),digits = 0))
Daily_Bikes <- Daily_Bikes %>% mutate(Casual_perc = round(Mean_cas/MeanDay,digits = 2))

barplot(x = Daily_Bikes$Weekdays, y = Daily_Bikes$MeanDay)

ggplot(data = Daily_Bikes ,aes(x=Weekdays,y=MeanDay))+
  geom_bar(stat = "identity",fill = "orange")+
  ggtitle("Weekly usage")+
  ylab("Average daily use")+
  coord_cartesian(ylim = c(150,200))
####Hourly effects on use####
Hourly_Bikes <- BikeShare %>%
                group_by(Hour = BikeShare$Hour) %>%
                summarise(MeanHour = round(mean(cnt),digits = 0),
                          Mean_cas = round(mean(casual),digits = 0),
                          Mean_reg = round(mean(registered),digits = 0))

Hourly_Bikes <- Hourly_Bikes %>% mutate(Casual_perc = round(Mean_cas/MeanHour,digits = 2))
####Seasonal effects on use####
Season_Bikes <- BikeShare %>%
                  group_by(Season = season) %>%
                  summarise(Mean = round(mean(cnt),digits = 0),
                            Mean_cas = round(mean(casual),digits = 0),
                            Mean_reg = round(mean(registered),digits = 0))

Season_Bikes <- Season_Bikes %>% mutate(Casual_perc = round(Mean_cas/Mean,digits = 2))
  
ggplot(data = Hourly_Bikes ,aes(x=Hour,y=MeanHour))+
  geom_bar(stat = "identity",fill = "orange")+
  ggtitle("Daily usage")+
  ylab("Average daily use")#+
  #coord_cartesian(ylim = c(150,200))

Wettercode <- c(1,2,3,4)
Wetter <- c("good","cloudy","bad","very bad")

WetterDF <- data.frame(Wettercode,Wetter)

BikeShare <- BikeShare %>% 
                mutate(Weather = ifelse(weathersit == 1, "good", 
                                  ifelse(weathersit == 2, "cloudy",
                                  ifelse(weathersit == 3, "bad", "very bad"))))

Weather_Bikes <- BikeShare %>% 
                  group_by(Weather = Weather) %>% 
                  summarise(Mean_util_total = round(mean(cnt),digits = 0),
                            Mean_util_casual = round(mean(casual),digits = 0),
                            Mean_util_reg = round(mean(registered),digits = 0))
#Check if there the rental behaviour among casual and registered users changes when weather changes
Weather_Bikes <- Weather_Bikes %>% mutate(Casual_perc = round(Mean_util_casual/Mean_util_total,digits = 2))

ggplot(data = Weather_Bikes ,aes(x=Weather,y=Mean_util_total))+
  geom_bar(stat = "identity",fill = "orange")+
  ggtitle("Weather and usage")+
  ylab("Average use")#+
  #coord_cartesian(ylim = c(150,200))

ggplot(data = Weather_Bikes ,aes(x=Weather,y=Mean_util_casual))+
  geom_bar(stat = "identity",fill = "orange")+
  ggtitle("Weather and casual usage")+
  ylab("Average use")#+
  #coord_cartesian(ylim = c(150,200))

ggplot(data = Weather_Bikes ,aes(x=Weather,y=Mean_util_reg))+
  geom_bar(stat = "identity",fill = "orange")+
  ggtitle("Weather and registered usage")+
  ylab("Average use")#+
#coord_cartesian(ylim = c(150,200))


#CorData <- cor(BikeShare[c(1,4,6,7,10,11,14,17)])
CorData <- cor(BikeShare[c(which(colnames(BikeShare) %in% c("season","mnth","Hour","weathersit","temp","atemp","windspeed","cnt","WeekdayNo")))])

corrplot(CorData, method = "color")
corrplot(CorData1, method = "color")
corrplot(CorData, method = "number")

#After checking for correlation, eliminate some redundant variables
BikeShare[which(names(BikeShare)== "instant")] <- NULL
BikeShare[which(names(BikeShare)== c("windspeed","yr","atemp","hum"))] <- NULL
BikeShare[which(names(BikeShare)== c("holiday","workingday"))] <- NULL
BikeShare[which(names(BikeShare)== "workingday")] <- NULL

BikeShare1 <- BikeShare[,c("Date","Hour","temp","casual","registered","cnt","Weekdays","WeekdayNo")]
#Taking out same outliers as for the BikeShare DF
BikeShare1 <- BikeShare1[-which(b %in% a),]

# What is the average usage of bikes per day on a Monday compared to the overall average?
mean(BikeShare1$cnt)
mean(BikeShare1$cnt[which(BikeShare1$Weekdays == "Monday")])
which(BikeShare1$Weekdays == "Monday")

#### Start building the regression model ####
#kNN Model__________________________________________________________________________
set.seed(123)
Train_Model<-createDataPartition(y=BikeShare1$cnt,p=.75,list = FALSE)
#str(Train_Model)
#TRAIN_Model
TrainingSet<-BikeShare1[Train_Model,]
TestingSet<-BikeShare1[-Train_Model,]

#nrow(training)
#nrow(testing)
kNNControl<-trainControl(method = "repeatedcv",#This_is_CROSS_VALIDATION
                         number = 5,
                         repeats = 5,
                         verboseIter = TRUE)#,#verbose =TRUE
#classProbs = TRUE)
#summaryFunction = twoClassSummary)
#RUN_MODEL
set.seed(111)
knnFit<-train(cnt ~ Hour+temp+atemp+hum+WeekdayNo+windspeed,#alternative_being:_(brand~.,)
              data = TrainingSet,
              method = "knn",
              trControl=kNNControl,
              tuneLength = 20,
              preProc = c("center", "scale")) #only center and scale integer variables for regression???
knnFit
#Predict
kNN_Predict<-predict(knnFit,newdata = TestingSet)
#str(kNN_Predict)
#plot(knnFit)
kNN_Predict
kNN_Probs<-predict(knnFit,newdata = TestingSet,type = "prob")
head(kNN_Probs)
#Use a Confusion Matrix only for classification problems!!!
#confusionMatrix(data = kNN_Predict,TestingSet$cnt)

#the postResample function is being used to estimate the Root mean squared error
postResample(kNN_Predict,TestingSet$cnt)
#Apply the model to Validation data
kNN_Predict_Validation<-predict(knnFit,newdata = Wifi_Vald)
kNN_Predict_Validation
postResample(kNN_Predict_Validation,Wifi_Vald$UniquePosit)


#SVM Model__________________________________________________________________________
SVMControl<-trainControl(method = "repeatedcv",#This_is_CROSS_VALIDATION
                   number = 10,
                   repeats = 10,
                   verboseIter = TRUE)#,#verbose =TRUE
set.seed(111)
SVM_Fit<-train(registered~Hour+temp+hum,#alternative_being:_(Volume~.,)
               data = TrainingSet,
               method = "svmLinear",
               trControl=SVMControl,
               tuneLength = 15,
               preProc = c("center", "scale"))
SVM_Fit
#Predict
SVM_Predict<-predict(SVM_Fit,newdata = TestingSet)

postResample(SVM_Predict,TestingSet$registered)

#Random Forest Model__________________________________________________________________________

RF_Fit<-train(registered~Hour+temp+hum,#alternative_being:_(Volume~.,)
               data = TrainingSet,
               method = "rf",
               trControl=SVMControl,
               tuneLength = 10,
               preProc = c("center", "scale"))
RF_Fit
#Predict
RF_Predict<-predict(RF_Fit,newdata = TestingSet)

postResample(RF_Predict,TestingSet$registered)