#Task: Deploy a wifi-fingerprint-based navigation system ´that shall be installed wihtin industrial buildings 
#----> determine a person's location in indoor spaces
Wifi<-read.csv(file = "C:/Users/alexw/Desktop/Ubiqum/Section3/Task3_Wifi_fingerprinting/trainingData.csv",header = TRUE)
Wifi_Vald<-read.csv(file = "C:/Users/alexw/Desktop/Ubiqum/Section3/Task3_Wifi_fingerprinting/validationData.csv",header = TRUE)
#Load_packages
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
detectCores()
registerDoParallel(2)
#
#wifi1<-Wifi[,c(1:10,521:529)]
Wifi[,ncol(Wifi)]<-as.POSIXct(Wifi[,ncol(Wifi)],origin='1970-01-01')
Wifi_Vald[,ncol(Wifi_Vald)]<-as.POSIXct(Wifi_Vald[,ncol(Wifi_Vald)],origin='1970-01-01')
#
Wifi<-transform(Wifi,UniquePosit=paste0(BUILDINGID,FLOOR))
Wifi_Vald<-transform(Wifi_Vald,UniquePosit=paste0(BUILDINGID,FLOOR))
#unique(Wifi$UniquePosit)
#unique(Wifi$USERID)
#
#SpaceID_unique<-Wifi1%>%dplyr::select(UniquePosit,SPACEID)%>%
#  dplyr::group_by(UniquePosit)%>%
#  summarize(Min_SpaceID=min(SPACEID),
#            Mean_SPACEID=mean(SPACEID),
#            Max_SpaceID=max(SPACEID))
#Convert Values of 100 to -100 so that afterwards columns with values only smaller than -80 can be omitted,
#as they suggest a weak signal
Wifi[Wifi==100]<- (-100)
#Wifi1<-Wifi[,sapply(Wifi[1:520],function(x) all(x==100))]
Wifi1<-Wifi[,sapply(Wifi[1:528],function(x) !all(x < (-80)))]
Wifi1<-cbind(Wifi1,Wifi[,c(521,529,530)])
Wifi1<-cbind(Wifi1[,c(1:326,334,327:333,335:336)])
#__________________________________________________Prepare Validationset____________________________
Wifi_Vald[Wifi_Vald==100]<- (-100)
Wifi_Vald<-Wifi_Vald[sapply(Wifi_Vald,function(x) !all(x == (-100))),]
#Wifi1<-Wifi[,sapply(Wifi[1:520],function(x) all(x==100))]
#Wifi_Vald_1<-Wifi_Vald[,sapply(Wifi_Vald[1:528],function(x) !all(x < (-80)))]
#Wifi1<-Wifi1[sapply(Wifi1,function(x) !all(x < (-80))),]
#Wifi_Vald_1<-cbind(Wifi_Vald_1,Wifi_Vald[,c(521,529,530)])
#Wifi_Vald_1<-cbind(Wifi_Vald_1[,c(1:326,334,327:333,335:336)])
#
#
haurein<-Wifi1%>%dplyr::select(USERID,UniquePosit)%>%
  dplyr::group_by(USERID)%>%
  summarize(UP00=sum(UniquePosit=="00"),
            UP01=sum(UniquePosit=="01"),
            UP02=sum(UniquePosit=="02"),
            UP03=sum(UniquePosit=="03"),
            UP10=sum(UniquePosit=="10"),
            UP11=sum(UniquePosit=="11"),
            UP12=sum(UniquePosit=="12"),
            UP13=sum(UniquePosit=="13"),
            UP20=sum(UniquePosit=="20"),
            UP21=sum(UniquePosit=="21"),
            UP22=sum(UniquePosit=="22"),
            UP23=sum(UniquePosit=="23"),
            UP24=sum(UniquePosit=="24"))
#Wifi1<-Wifi[,sapply(Wifi[,1:463],function(x) !all(x<(-80)))]
Wifi2<-c()
for(i in 1:520){
  if(all(Wifi[i]) == 100){
    Wifi2[i]<-Wifi[,-i]
  }
}
#### The loop if1####
to_del<-c()
for(i in 1:518){
  if(all(Wifi[i] < (-80))){
    to_del<-cbind(to_del,i)
  }
}
wifi2 <- as.data.frame(Wifi[,-to_del])
#
#rbind(to_del,i)
####Subset DFs####
Building0<-subset(Wifi1,BUILDINGID ==0)
Building1<-subset(Wifi1,BUILDINGID ==1)
Building2<-subset(Wifi1,BUILDINGID ==2)
#
Building00<-subset(Wifi1, UniquePosit == "00")
Building01<-subset(Wifi1, UniquePosit == "01")
Building02<-subset(Wifi1, UniquePosit == "02")
Building03<-subset(Wifi1, UniquePosit == "03")
#
Building10<-subset(Wifi1, UniquePosit == "10")
Building11<-subset(Wifi1, UniquePosit == "11")
Building12<-subset(Wifi1, UniquePosit == "12")
Building13<-subset(Wifi1, UniquePosit == "13")
#
Building20<-subset(Wifi1, UniquePosit == "20")
Building21<-subset(Wifi1, UniquePosit == "21")
Building22<-subset(Wifi1, UniquePosit == "22")
Building23<-subset(Wifi1, UniquePosit == "23")
Building24<-subset(Wifi1, UniquePosit == "24")
sum(Wifi$BUILDINGID==0)
sum(Wifi$BUILDINGID==1)
sum(Wifi$BUILDINGID==2)
sum(Building2$FLOOR==0)
#Plot the floor usage of each building
####plots####
ggplot(data=Building0%>%group_by(FLOOR)%>% count(FLOOR),aes(x=FLOOR,y=n))+geom_col()
ggplot(data=Building1%>%group_by(FLOOR)%>% count(FLOOR),aes(x=FLOOR,y=n))+geom_col()
ggplot(data=Building2%>%group_by(FLOOR)%>% count(FLOOR),aes(x=FLOOR,y=n))+geom_col()
#
ggplot(data=Wifi1,aes(x = LONGITUDE,y = LATITUDE))+
                  geom_point(size=2,color="red")
                  # geom_tile(aes(fill=UniqueLocID))+
                   #scale_fill_gradient(low = "white",high = "steelblue")
ggplot(data=Building0,aes(x = LONGITUDE,y = LATITUDE))+
                  geom_point(size=2,color="red")
#Plotting the UniquePosit with Lat and Long
ggplot(data=Wifi1,aes(x = LONGITUDE,y = LATITUDE))+
  geom_point(size=1,aes(colour=UniquePosit))
####Create new Vars####
Wifi1$LatitudeSpan<-discretize(Wifi1$LATITUDE,method = "frequency",breaks = 20,include.lowest = TRUE)
Wifi1$LongitudeSpan<-discretize(Wifi1$LONGITUDE,method = "frequency",breaks = 20,include.lowest = TRUE)
Wifi1$LatitudeBin<-discretize(Wifi1$LATITUDE,method = "frequency",breaks = 20,include.lowest = TRUE,labels = F)
Wifi1$LongitudeBin<-discretize(Wifi1$LONGITUDE,method = "frequency",breaks = 20,include.lowest = TRUE,labels = F)
Wifi1<-transform(Wifi1,UniqueLocID=paste0(UniquePosit,LongitudeBin,LatitudeBin))
#Apply above commands to validation data set
Wifi_Vald$LatitudeSpan<-discretize(Wifi_Vald$LATITUDE,method = "frequency",breaks = 20,include.lowest = TRUE)
Wifi_Vald$LongitudeSpan<-discretize(Wifi_Vald$LONGITUDE,method = "frequency",breaks = 20,include.lowest = TRUE)
Wifi_Vald$LatitudeBin<-discretize(Wifi_Vald$LATITUDE,method = "frequency",breaks = 20,include.lowest = TRUE,labels = F)
Wifi_Vald$LongitudeBin<-discretize(Wifi_Vald$LONGITUDE,method = "frequency",breaks = 20,include.lowest = TRUE,labels = F)
Wifi_Vald<-transform(Wifi_Vald,UniqueLocID=paste0(UniquePosit,LongitudeBin,LatitudeBin))
#
levels(Wifi_Vald$LatitudeSpan)<-levels(Wifi1$LatitudeSpan)
levels(Wifi_Vald$LongitudeSpan)<-levels(Wifi1$LongitudeSpan)
ordered(levels(Wifi1$UniqueLocID))
#Count
unique(Wifi1$WAP007)
#Wifi2<-unique(Wifi1[,1:326],incomparables = F)
#
#Fiilter  Building  0  WAPs
WAPs0<-Wifi1%>%dplyr::select(starts_with("WAP"),BUILDINGID)%>%
  dplyr::group_by(BUILDINGID)%>%
  filter(BUILDINGID=="0")%>%
  summarise_all(funs(max))
WAPs0<-WAPs0[,sapply(WAPs0,function(x) all(x >=(-80)))]
#Fiilter  Building  1  WAPs
WAPs1<-Wifi1%>%dplyr::select(starts_with("WAP"),BUILDINGID)%>%
  dplyr::group_by(BUILDINGID)%>%
  filter(BUILDINGID=="1")%>%
  summarise_all(funs(max))
WAPs1<-WAPs1[,sapply(WAPs1,function(x) all(x >=(-80)))]
#Fiilter  Building  2  WAPs
WAPs2<-Wifi1%>%dplyr::select(starts_with("WAP"),BUILDINGID)%>%
  dplyr::group_by(BUILDINGID)%>%
  filter(BUILDINGID=="2")%>%
  summarise_all(funs(max))
WAPs2<-WAPs2[,sapply(WAPs2,function(x) all(x >=(-80)))]
#
WAPs00<-Wifi1%>%dplyr::select(starts_with("WAP"),UniquePosit)%>%
                dplyr::group_by(UniquePosit)%>%
                filter(UniquePosit=="00")%>%
                summarise_all(funs(max))
#
WAPs<-grep("^WAP",names(Wifi1))
#
#Wifi2<-Wifi1[1:5000,]
#
####k-NN Model####
#____________________________________________Predict_UniquePosit_______________________________________
set.seed(123)
Train_Model<-createDataPartition(y=Wifi1$UniquePosit,p=.75,list = FALSE)
#str(Train_Model)
#TRAIN_Model
training<-Wifi1[Train_Model,]
testing<-Wifi1[-Train_Model,]
#nrow(training)
#nrow(testing)
kNNControl<-trainControl(method = "repeatedcv",#This_is_CROSS_VALIDATION
                   number = 3,
                   repeats = 3,
                   verboseIter = TRUE)#,#verbose =TRUE
                   #classProbs = TRUE)
#summaryFunction = twoClassSummary)
#RUN_MODEL
set.seed(111)
knnFit<-train(UniquePosit~.,#alternative_being:_(brand~.,)
              data = training[,c(WAPs,which(names(training)=="UniquePosit"))],
              method = "knn",
              trControl=kNNControl,
              tuneLength = 2) #,
              #preProc = c("center", "scale"))
knnFit
#Predict
kNN_Predict<-predict(knnFit,newdata = testing)
#str(kNN_Predict)
#plot(knnFit)
kNN_Predict
kNN_Probs<-predict(knnFit,newdata = testing,type = "prob")
head(kNN_Probs)
confusionMatrix(data = kNN_Predict,testing$UniquePosit)
#
postResample(kNN_Predict,testing$UniquePosit)
#Apply the model to Validation data
kNN_Predict_Validation<-predict(knnFit,newdata = Wifi_Vald)
kNN_Predict_Validation
postResample(kNN_Predict_Validation,Wifi_Vald$UniquePosit)
#______________________________________________Predict_LONGITUDE___________________________________
set.seed(123)
Train_Model_LONGIT<-createDataPartition(y=Wifi1$LONGITUDE,p=.75,list = FALSE)
#TRAIN_Model
training_LONGIT<-Wifi1[Train_Model_LONGIT,]
testing_LONGIT<-Wifi1[-Train_Model_LONGIT,]
#
kNNControl<-trainControl(method = "repeatedcv",#This_is_CROSS_VALIDATION
                         number = 3,
                         repeats = 3,
                         verboseIter = TRUE)#,#verbose =TRUE
#classProbs = TRUE)
#summaryFunction = twoClassSummary)
#RUN_MODEL
set.seed(111)
knnFit_LONGIT<-train(LONGITUDE~.,#alternative_being:_(brand~.,)
              data = training_LONGIT[,c(WAPs,which(names(training)=="LONGITUDE"))],
              method = "knn",
              trControl=kNNControl,
              tuneLength = 2) #,
#preProc = c("center", "scale"))
knnFit_LONGIT
#Predict
kNN_Predict_LONGIT<-predict(knnFit_LONGIT,newdata = testing)
kNN_Predict_LONGIT
#
postResample(kNN_Predict_LONGIT,testing$LONGITUDE)
#Apply the model to Validation data
kNN_Predict_Validation_LONGIT<-predict(knnFit_LONGIT,newdata = Wifi_Vald)
kNN_Predict_Validation_LONGIT
postResample(kNN_Predict_Validation_LONGIT,Wifi_Vald$LONGITUDE)
#______________________________________________Predict_LATITUDE___________________________________
set.seed(123)
Train_Model_LATIT<-createDataPartition(y=Wifi1$LATITUDE,p=.75,list = FALSE)
#TRAIN_Model
training_LATIT<-Wifi1[Train_Model_LATIT,]
testing_LATIT<-Wifi1[-Train_Model_LATIT,]
#
kNNControl<-trainControl(method = "repeatedcv",#This_is_CROSS_VALIDATION
                         number = 3,
                         repeats = 3,
                         verboseIter = TRUE)#,#verbose =TRUE
#classProbs = TRUE)
#summaryFunction = twoClassSummary)
#RUN_MODEL
set.seed(111)
knnFit_LATIT<-train(LATITUDE~.,#alternative_being:_(brand~.,)
                     data = training_LATIT[,c(WAPs,which(names(training)=="LATITUDE"))],
                     method = "knn",
                     trControl=kNNControl,
                     tuneLength = 2) #,
#preProc = c("center", "scale"))
knnFit_LATIT
#Predict
kNN_Predict_LATIT<-predict(knnFit_LATIT,newdata = testing)
kNN_Predict_LATIT
#
postResample(kNN_Predict_LATIT,testing$LATITUDE)
#Apply the model to Validation data
kNN_Predict_Validation_LATIT<-predict(knnFit_LATIT,newdata = Wifi_Vald)
kNN_Predict_Validation_LATIT
postResample(kNN_Predict_Validation_LATIT,Wifi_Vald$LATITUDE)
####++Aggregate kNN Result####
kNN_Predict_df<-as.data.frame(cbind(kNN_Predict_Validation,kNN_Predict_Validation_LATIT,kNN_Predict_Validation_LONGIT))
colnames(kNN_Predict_df)[1]<-"UniquePosit"
colnames(kNN_Predict_df)[2]<-"LATITUDE"
colnames(kNN_Predict_df)[3]<-"LONGITUDE"
kNN_Predict_df$UniquePosit<-as.factor(kNN_Predict_df$UniquePosit)
#Plotting the UniquePosit with Lat and Long
ggplot(data=kNN_Predict_df,aes(x = LONGITUDE,y = LATITUDE))+
  geom_point(size=1,aes(colour=UniquePosit))
#Plotting the simple Prediction results
ggplot(data=kNN_Predict_df,aes(x = LONGITUDE,y = LATITUDE))+
  geom_point(size=1,color="blue")
#Create new data frame to compare the prediction with original and validation data
kNN_Predict_df1<-kNN_Predict_df
kNN_Predict_df1$Allocation<-"predict"
Wifi1$Allocation<-"original"
Wifi_Vald$Allocation<-"validation"
kNN_Predict_df1<- rbind(kNN_Predict_df1[,1:4],Wifi1[,c(336,327:328,337)],Wifi_Vald[,c(530,521:522,531)])
#Comparable plot of Original, Validation and Prediction Data
ggplot(data=kNN_Predict_df1,aes(x = LONGITUDE,y = LATITUDE))+
  geom_point(size=1,aes(colour=Allocation))
  
####Random Forest####
#______________________________________________RANDOM_FOREST_UniquePosit___________________________________________
set.seed(111)
t0<-proc.time()[3]
RF_Fit<-train(UniquePosit~.,#alternative_being:_(brand~.,)
              data = training[,c(WAPs,which(names(training)=="UniquePosit"))],
              method = "rf",
              ntree = 10,
              #metric = "Accuracy",
              trControl=kNNControl,
              tuneLength = 3)
              #preProc = c("center", "scale"))
t1<-proc.time()[3]
t2<-(t1-t0)
RF_Fit
#plot(RF_Fit)
#Predict
RF_Predict<-predict(RF_Fit,newdata = testing)
RF_Predict
#str(RF_Predict)
RF_Probs<-predict(RF_Fit,newdata = testing,type = "prob")
confusionMatrix(data = RF_Predict,testing$UniquePosit)
#
postResample(RF_Predict,testing$UniquePosit)
#
RF_Predict_Validation<-predict(RF_Fit,newdata = Wifi_Vald)
RF_Predict_Validation
postResample(RF_Predict_Validation,Wifi_Vald$UniquePosit)
#
#______________________________________________RANDOM_FOREST_LONGITUDE___________________________________________
set.seed(111)
t0<-proc.time()[3]
RF_Fit_LONGIT<-train(LONGITUDE~.,#alternative_being:_(brand~.,)
              data = training_LONGIT[,c(WAPs,which(names(training)=="LONGITUDE"))],
              method = "rf",
              ntree = 10,
              #metric = "Accuracy",
              trControl=kNNControl,
              tuneLength = 3)
#preProc = c("center", "scale"))
t1<-proc.time()[3]
t2<-(t1-t0)
RF_Fit_LONGIT
#plot(RF_Fit)
#Predict
RF_Predict_LONGIT<-predict(RF_Fit_LONGIT,newdata = testing)
RF_Predict_LONGIT
#str(RF_Predict)
postResample(RF_Predict_LONGIT,testing$LONGITUDE)
#
RF_Predict_Validation_LONGIT<-predict(RF_Fit_LONGIT,newdata = Wifi_Vald)
RF_Predict_Validation_LONGIT
postResample(RF_Predict_Validation_LONGIT,Wifi_Vald$LONGITUDE)
#
#______________________________________________RANDOM_FOREST_LATITUDE___________________________________________
set.seed(111)
t0<-proc.time()[3]
RF_Fit_LATIT<-train(LATITUDE~.,#alternative_being:_(brand~.,)
                     data = training_LATIT[,c(WAPs,which(names(training)=="LATITUDE"))],
                     method = "rf",
                     ntree = 10,
                     #metric = "Accuracy",
                     trControl=kNNControl,
                     tuneLength = 3)
#preProc = c("center", "scale"))
t1<-proc.time()[3]
t2<-(t1-t0)
RF_Fit_LATIT
#plot(RF_Fit)
#Predict
RF_Predict_LATIT<-predict(RF_Fit_LATIT,newdata = testing)
RF_Predict_LATIT
#str(RF_Predict)
postResample(RF_Predict_LATIT,testing$LATITUDE)
#
RF_Predict_Validation_LATIT<-predict(RF_Fit_LATIT,newdata = Wifi_Vald)
RF_Predict_Validation_LATIT
postResample(RF_Predict_Validation_LATIT,Wifi_Vald$LATITUDE)
####++Aggregate RF Result####
RF_Predict_df<-as.data.frame(cbind(RF_Predict_Validation,RF_Predict_Validation_LATIT,RF_Predict_Validation_LONGIT))
colnames(RF_Predict_df)[1]<-"UniqueID"
colnames(RF_Predict_df)[2]<-"LATITUDE"
colnames(RF_Predict_df)[3]<-"LONGITUDE"
ggplot(data=RF_Predict_df,aes(x = LONGITUDE,y = LATITUDE))+
  geom_point(size=1,color="blue")
#Original Map
ggplot(data=Wifi1,aes(x = LONGITUDE,y = LATITUDE))+
  geom_point(size=1,color="red")
#
#
#Building0$LatitudeSpan<-discretize(Building0$LATITUDE,method = "frequency",breaks = 20,include.lowest = TRUE)
#Building0$LongitudeSpan<-discretize(Building0$LONGITUDE,method = "frequency",breaks = 20,include.lowest = TRUE)
#Building0$LatitudeBin<-discretize(Building0$LATITUDE,method = "frequency",breaks = 20,include.lowest = TRUE,labels = F)
#Building0$LongitudeBin<-discretize(Building0$LONGITUDE,method = "frequency",breaks = 20,include.lowest = TRUE,labels = F)
#
#
#
#
#
#
#
#
#
#
#Wifi[19938,]<-NA
#Wifi[19938,1:520]<-apply(Wifi[1:19937,1:520],2,mean)
#Wifi<-Wifi[,!Wifi[19938,]==100]
#Wifi2<-Wifi[-which(Wifi[19938,]!=100)]
#curated <- Wifi[,-to_delete]
#
#Wifi_1 <- 
#to_delete <- c()
#for(i in 1:520){
#  if (mean(Wifi[,i]) == 100) {
#      to_delete <- rbind(to_delete,i)  
#  }
#}
#wifi_1 <- as.data.frame(Wifi[,-to_delete])
#
####The Loop if####
#
#___________________________________________________________Function
#Function1<-function(df){
#  for(i in 1:520){
#      if(mean(df[,i])==100){
#       df<-df[,-i]
#     }
#    }
#  }
#
#Avg<-as.data.frame(sapply(Wifi[1:520],mean))
#Avg1<-transpose(Avg)
#colnames(Avg1)<-rownames(Avg)
#Delete_col<-as.data.frame(Avg1[Avg1[1,]!=100])
#Wifi1<-Wifi%>%select_if(mean()!=100)
#Avg1<-Avg1[,!Avg1==100]
#__________For the MOdel: measure the WAP's strength for the following variables; the following may be the dependent variables
#Longitud
#Latitud
#Building
#Floor
#SpaceID