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
str(Survey_Response)
#Survey_Response$age<-as.integer(Survey_Response$age)
Survey_Response$brand<-as.factor(Survey_Response$brand)
#Survey_Response$brand<-as.integer(Survey_Response$brand)
Survey_Response$elevel<-as.factor(Survey_Response$elevel)
Survey_Response$car<-as.factor(Survey_Response$car)
colnames(Survey_Response)[3]<-"educ"
Survey_Response$educ<-as.factor(Survey_Response$educ)
Survey_Response$zipcode<-as.factor(Survey_Response$zipcode)

levels(Survey_Response$brand)<-c(0,1)

#Assign_2_Level_Factor
#Survey_Response$brand<-factor(c("1","2"))
#MODEL_CREATION
set.seed(123)
Train_Model<-createDataPartition(y=Survey_Response$brand,p=.75,list = FALSE)
str(Train_Model)
#TRAIN_Model
training<-Survey_Response[Train_Model,]
testing<-Survey_Response[-Train_Model,]
nrow(training)
nrow(testing)
ctrl_RF<-trainControl(method = "repeatedcv",#This_is_CROSS_VALIDATION
                   number = 10,
                   repeats = 10,
                   verboseIter = TRUE)#,#verbose =TRUE
#classProbs = TRUE,
#summaryFunction = twoClassSummary)
#RUN_MODEL
set.seed(111)
RF_Fit<-train(brand~salary+educ+age,#alternative_being:_(brand~.,)
              data = training,
              method = "rf",
              ntree = 100,
              #metric = "Accuracy",
              trControl=ctrl_RF,
              tuneLength = 3,
              preProc = c("center", "scale"))
RF_Fit
plot(RF_Fit)
#Predict
RF_Predict<-predict(RF_Fit,newdata = testing)
str(RF_Predict)
RF_Probs<-predict(RF_Fit,newdata = testing,type = "prob")
head(RF_Probs)
confusionMatrix(data = RF_Predict,testing$brand)
#
postResample(RF_Predict,testing$brand)
print(RF_Fit)
####
#####APPLY_MODEL_TO_NEW_DATA
####
Survey_Incomplete<-read.csv(file="C:/Users/alexw/Desktop/Ubiqum/Section2/Task2/SurveyIncomplete.csv",header = TRUE)
str(Survey_Incomplete)
Survey_Incomplete$brand<-as.factor(Survey_Incomplete$brand)
#Survey_Response$brand<-as.integer(Survey_Response$brand)
colnames(Survey_Incomplete)[3]<-"educ"
Survey_Incomplete$educ<-as.factor(Survey_Incomplete$educ)
Survey_Incomplete$car<-as.factor(Survey_Incomplete$car)
Survey_Incomplete$educ<-as.factor(Survey_Incomplete$educ)
Survey_Incomplete$zipcode<-as.factor(Survey_Incomplete$zipcode)
levels(Survey_Response$brand)<-c("Acer","Sony")
#Predict_with_new_dataset
###_PLOT_THE_DIGRAMM
RF_newData_predict<-predict(RF_Fit,Survey_Incomplete)
RF_newData_predict
plot1<-plot(RF_newData_predict,
            col=c("green","grey30"),
            xlab="Brand",
            ylab="Observations",
            legend=TRUE,
            args.legend=list(x=0,y=3000),
            ylim=c(0,4000))
plot1
col=c("green10","grey10")
str(RF_newData_predict)
head(RF_newData_predict)
confusionMatrix(data = RF_newData_predict,testing$brand)
library(ggplot2)
ggplot_graph_2<-ggplot(Survey_Response,aes(x=salary,y=age,color=Survey_Response$brand))+geom_point(shape=1)
ggplot_graph_2
ggplot()+geom_point(data = Survey_Response, aes(x=Survey_Response$credit,y=Survey_Response$age, color=Survey_Response$brand))
