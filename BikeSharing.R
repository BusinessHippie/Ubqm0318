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

BikeShare <- read.csv(file="C:/Users/alexw/Desktop/Practice_Datasets/Bike_Sharing/hour.csv",header = TRUE)

BikeShare$dteday <- as.POSIXct(BikeShare$dteday)
BikeShare <- na.omit(BikeShare)

colnames(BikeShare)[2] <- "Date"

BikeShare[,-c(2,3,4,5,6,7)]

#Create Weekday DF
Weekdays <- c("Monday","Tuesday","Wednsday","Thursday","Friday","Saturday","Sunday")
WeekdayNo <- c(1,2,3,4,5,6,0)
WeekdayDF <- data.frame(Weekdays,WeekdayNo)

colnames(BikeShare)[colnames(BikeShare) == "weekday"] <- "WeekdayNo"
colnames(BikeShare)[colnames(BikeShare) == "hr"] <- "Hour"

BikeShare <- merge(BikeShare, WeekdayDF, by = "WeekdayNo")
rm(BikeShare1)

BikeShare$Weekdays <- as.factor(BikeShare$Weekdays)
BikeShare$Weekdays <- ordered(BikeShare$Weekdays,levels = c("Monday","Tuesday","Wednsday","Thursday","Friday","Saturday","Sunday"))

Daily_Bikes <- BikeShare %>% 
                group_by(Weekdays = BikeShare$Weekdays) %>% 
                summarise(MeanDay = round(mean(cnt),digits = 0))

barplot(x = Daily_Bikes$Weekdays, y = Daily_Bikes$MeanDay)

ggplot(data = Daily_Bikes ,aes(x=Weekdays,y=MeanDay))+
  geom_bar(stat = "identity",fill = "orange")+
  ggtitle("Weekly usage")+
  ylab("Average daily use")+
  coord_cartesian(ylim = c(150,200))

Hourly_Bikes <- BikeShare %>%
                group_by(Hour = BikeShare$Hour) %>%
                summarise(MeanHour = round(mean(cnt),digits = 0))

ggplot(data = Hourly_Bikes ,aes(x=Hour,y=MeanHour))+
  geom_bar(stat = "identity",fill = "orange")+
  ggtitle("Daily usage")+
  ylab("Average daily use")#+
  #coord_cartesian(ylim = c(150,200))

###### TO DO #####################
BikeShare1 <- BikeShare %>% mutate(Weather = ifelse(weathersit = 1, "good", ifelse(weathersit = 2, "cloudy",ifelse(weathersit = 3, "bad","very bad"))))

Weather_Bikes <- BikeShare %>% 
                  group_by(Weather = BikeShare$weathersit) %>% 
                  summarise(Mean_util_total = round(mean(cnt),digits = 0),
                            Mean_util_casual = round(mean(casual),digits = 0),
                            Mean_util_reg = round(mean(registered),digits = 0))
#Check if there the rental behaviour among casual and registered users changes when weather changes
Weather_Bikes <- Weather_Bikes %>% mutate(cas_reg_relation = round(Mean_util_casual/Mean_util_reg,digits = 2))

ggplot(data = Weather_Bikes ,aes(x=Weather,y=Mean_utilisation))+
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


CorData <- cor(BikeShare[c(4,6,7,10,11,14,17)])
corrplot(CorData, method = "color")
corrplot(CorData, method = "number")

#After checking for correlation, eliminate some redundant variables
BikeShare[which(names(BikeShare)== "instant")] <- NULL
BikeShare[which(names(BikeShare)== c("instant","windspeed","yr","season","holiday","Workingday","atemp","hum","windspeed"))] <- NULL
BikeShare[which(names(BikeShare)== c("holiday","workingday"))] <- NULL
BikeShare[which(names(BikeShare)== "holiday")] <- NULL
BikeShare[which(names(BikeShare)== "workingday")] <- NULL
