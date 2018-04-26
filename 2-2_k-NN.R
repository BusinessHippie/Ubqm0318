#attributes(DatasetName)___List your attributes within your data set.
#summary(DatasetName)___Prints the min, max, mean, median, and quartiles of each attribute.
#str(DatasetName)___Displays the structure of your data set.
#names(DatasetName)___Names your attributes within your data set.
#DatasetName$ColumnName___Will print out the instances within that particular column in your data set.
#DatasetName$ColumnName<-as.typeofdata(DatasetName$ColumnName)___>CHANGE_DATA_TYPE
#names(DatasetName)<-c("ColumnName","ColumnName","ColumnName")__RENAME_ATTRIBUTES/COLUMNS
#DatasetName$ColumnName[is.na(DatasetName$ColumnName)]<-mean(DatasetName$ColumnName,na.rm = TRUE)
#_____________>Replace_MISSING_Values_with_the_mean
library(caret)
Survey_Response<-read.csv(file="C:/Users/alexw/Desktop/Ubiqum/Section2/Task2/Survey_Key_and_Complete_Responses.csv",header = TRUE)
summary(Survey_Key_and_Complete_Responses_excel)
names(Survey_Key_and_Complete_Responses)
#rm(Survey_Key_and_Complete_Responses_excel)
#rm____->Remove_DataSet
attributes(Survey_Response)
names(Survey_Response)
colnames(Survey_Response)[3]<-"educ"
str(Survey_Response)
#Survey_Response$age<-as.integer(Survey_Response$age)
Survey_Response$brand<-as.factor(Survey_Response$brand)
#Survey_Response$brand<-as.integer(Survey_Response$brand)
Survey_Response$educ<-as.factor(Survey_Response$educ)
Survey_Response$car<-as.factor(Survey_Response$car)
Survey_Response$zipcode<-as.factor(Survey_Response$zipcode)
#Assign_2_Level_Factor
Survey_Response$brand<-factor(Survey_Response$brand)
#MODEL_CREATION
set.seed(123)
Train_Model<-createDataPartition(y=Survey_Response$brand,p=.75,list = FALSE)
str(Train_Model)
#TRAIN_Model
training<-Survey_Response[Train_Model,]
testing<-Survey_Response[-Train_Model,]
nrow(training)
nrow(testing)
ctrl<-trainControl(method = "repeatedcv",#This_is_CROSS_VALIDATION
                   number = 10,
                   repeats = 10,
                   verboseIter = TRUE)#,#verbose =TRUE
                   #classProbs = TRUE,
                   #summaryFunction = twoClassSummary)
#RUN_MODEL
set.seed(111)
knnFit<-train(brand~salary+educ+credit+age,#alternative_being:_(brand~.,)
              data = training,
              method = "knn",
              trControl=ctrl,
              tuneLength = 15,
              preProc = c("center", "scale"))
knnFit
#Predict
kNN_Predict<-predict(knnFit,newdata = testing)
str(kNN_Predict)
plot(knnFit)
kNN_Predict
kNN_Probs<-predict(knnFit,newdata = testing,type = "prob")
head(kNN_Probs)
confusionMatrix(data = kNN_Predict,testing$brand)
#
postResample(kNN_Predict,testing$brand)
#__GG_plot
install.packages("ggplot2")
