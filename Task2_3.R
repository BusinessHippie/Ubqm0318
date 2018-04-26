Products<-read.csv(file="C:/Users/alexw/Desktop/Ubiqum/Section2/Task3/existingproductattributes2017.2.csv",header = TRUE)
Products
library(caret)
dfProducts<-dummyVars("~.",data = Products)#CREATING_DUMMY_VARIABLES
readyData<-data.frame(predict(dfProducts,newdata = Products))
str(Products)
na.omit(Products$BestSellersRank)
as.integer(Products$ProductType)

# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(readyData1[,1:4], method=c("range"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams,readyData1[,1:4])
# summarize the transformed dataset
summary(transformed)

#__CORRELATION_MATRIX
corrData<-cor(readyData)
corrData
#install corrplot for HEATMAP
install.packages("corrplot")
library(corrplot)
corrplot(corrData, method = "color")
readyData$BestSellersRank<- NULL
str(readyData)
#Omit some attributes/ Attirubte selection for regression
readyData<-readyData[,-c("x1StarReviews","x2StarReviews")]
readyData1<-subset(readyData,select = c(x4StarReviews,Price,
                                        Recommendproduct,
                                        Volume,
                                        ShippingWeight,
                                        PositiveServiceReview))#Selecting the attributes
str(readyData1)
corrData1<-cor(readyData1)
corrData1
corrplot(corrData1,method = "number")
#
#
###################______________________________Build LINEAR Model________________________________________________
set.seed(123)
trainsize<-round(nrow(readyData1)*0.75)
testsize<-nrow(readyData1)-trainsize
trainsize
testsize
training_indices<-sample(seq_len(nrow(readyData1)),size = trainsize)
trainSet<-readyData1[training_indices, ]
testSet<-readyData1[-training_indices, ]
#
set.seed(111)
trainSet<-readyData1[training_indices, ]
testSet<-readyData1[-training_indices, ]
LinearModel<-lm(Volume~.,as.data.frame(readyData1))
LinearModel
summary(LinearModel)
#
plot(LinearModel)
#Predict
LM_Predict<-predict(LinearModel,newdata = testing)
LM_Predict

postResample(LM_Predict,testing$Volume)
plot(LM_Predict)
lines(testing$Volume)
#
#
###################______________________________Create SVM Model________________________________________________
set.seed(123)
trainSet<-createDataPartition(y=readyData1$Volume,p=.75,list = FALSE)
#TRAIN_Model
training<-readyData1[trainSet,]
testing<-readyData1[-trainSet,]
nrow(training)
nrow(testing)
ctrl<-trainControl(method = "repeatedcv",#This_is_CROSS_VALIDATION
                   number = 10,
                   repeats = 10,
                   verboseIter = TRUE)#,#verbose =TRUE
set.seed(111)
SVM_Fit<-train(Volume~x4StarReviews+Price+PositiveServiceReview,#alternative_being:_(Volume~.,)
              data = training,
              method = "svmLinear",
              trControl=ctrl,
              tuneLength = 15,
              preProc = c("center", "scale"))
SVM_Fit
#Predict
SVM_Predict<-predict(SVM_Fit,newdata = testing)

postResample(SVM_Predict,testing$Volume)

plot(SVM_Predict)
lines(testing$Volume)
#
#
###################______________________________Create RANDOM FOREST Model_____________________________________
set.seed(123)
trainSet<-createDataPartition(y=readyData1$Volume,p=.75,list = FALSE)
#TRAIN_Model
training<-readyData1[trainSet,]
testing<-readyData1[-trainSet,]
nrow(training)
nrow(testing)
ctrl<-trainControl(method = "repeatedcv",#This_is_CROSS_VALIDATION
                   number = 10,
                   repeats = 10,
                   verboseIter = TRUE)#,#verbose =TRUE
set.seed(111)
RF_Fit<-train(Volume~x4StarReviews+Price+PositiveServiceReview,#alternative_being:_(Volume~.,)
               data = training,
               method = "rf",
               trControl=ctrl,
               tuneLength = 5,
               preProc = c("center", "scale"))
RF_Fit
#Predict
RF_Predict<-predict(RF_Fit,newdata = testing)

postResample(RF_Predict,testing$Volume)
plot(RF_Predict)
lines(testing$Volume)
#
#
###################______________________________Create GBM MODEL_______________________________________________
set.seed(123)
trainSet<-createDataPartition(y=readyData1$Volume,p=.75,list = FALSE)
#TRAIN_Model
training<-readyData1[trainSet,]
testing<-readyData1[-trainSet,]
nrow(training)
nrow(testing)
ctrl<-trainControl(method = "repeatedcv",#This_is_CROSS_VALIDATION
                   number = 10,
                   repeats = 10,
                   verboseIter = TRUE)#,#verbose =TRUE
set.seed(111)
GBM_Fit<-train(Volume~x4StarReviews+Price+PositiveServiceReview,#alternative_being:_(Volume~.,)
              data = training,
              method = "gbm",
              trControl=ctrl,
              tuneLength = 5,
              preProc = c("center", "scale"))
GBM_Fit
head(GBM_Fit)
#Predict
GBM_Predict<-predict(GBM_Fit,newdata = testing)
str(GBM_Predict)

postResample(GBM_Predict,testing$Volume)
plot(GBM_Predict)
lines(testing$Volume)
####
#
#___The two outlying values should have been taken out. Sidenote for the next time.
#
#
readyData1$Volume
View(readyData1$Volume)
#
#___________________________________MAKE PREDICTIONS ON NEW DATASET_____________________________________________________
New_Products<-read.csv(file="C:/Users/alexw/Desktop/Ubiqum/Section2/Task3/newproductattributes2017.2.csv",header = TRUE)
New_Products
dfProducts_new<-dummyVars("~.",data = New_Products)#CREATING_DUMMY_VARIABLES

readyData_new<-data.frame(predict(dfProducts_new,newdata = New_Products))
str(New_Products)
na.omit(New_Products$BestSellersRank)
as.integer(New_Products$ProductType)
readyData_new1<-subset(readyData_new,select = c(x4StarReviews,
                                                Price,
                                                Recommendproduct,
                                                Volume,
                                                ShippingWeight,
                                                PositiveServiceReview))#Selecting the attributes
SVM_newData_predict<-predict(SVM_Fit,readyData_new1)
SVM_newData_predict
#
postResample(SVM_newData_predict,testing$Volume)
#
plot_SVM<-plot(SVM_newData_predict,
            col=c("green"),
            xlab="Observations",
            ylab="Volume",
            legend=TRUE,
            args.legend=list(x=0,y=3000),
            ylim=c(0,3500))
plot1
View(SVM_newData_predict)
View(New_Products)
#________________________________Convert Vector to Data Frame/Convert_Rows_to_Column_______________________
PlayWithData<-as.data.frame(SVM_newData_predict)#optionally as.data.frame(t(SVM_newData_predict))
#__________________________________________________________________________________________________________
str(PlayWithData)
#_____________Assign ID Column
PlayWithData$ID <- c("ID")[1]
PlayWithData$ID<- seq.int(nrow(PlayWithData))
View(PlayWithData)
names(PlayWithData)[1]<-c("Sales_Predict")
#_____names(PlayWithData)[names(PlayWithData) == paste(PlayWithData$SVM_newData_Predict)] <- 'Volume_Predict'

New_Products$ID<-seq.int(nrow(New_Products))
View(New_Products)
#
Final.data.frame<-merge(PlayWithData[,1:2],New_Products[,c(1,2,19)],by = "ID")
#                        #all.SVM_newData_predict=FALSE,
#                       #incomparables=NULL)
Final.data.frame
View(Final.data.frame)
#
New_Products %>% filter (sfsfds) %>% 
#
a <- c(1,2,19)
mean(a)
write.csv(Final.data.frame,"C:/Users/alexw/Desktop/Ubiqum/Section2/Task3/FinalProjectData.csv")
#_______________________>>>>>_Alternative for the MERGE Function would be the CBIND function___________________
