install.packages(pkgs = "caret", dependencies = c("Depends", "Imports"))
library(caret)
#install.packages("caret", dependencies = c("Depends", "Suggests")) 
#install.packages(glue)
library(mlbench)
data("Sonar")
set.seed(107)
#Split the data into two groups: Training and test set
intrain<-createDataPartition(y = Sonar$Class,
                             p=.75,
                             list = FALSE)
str(intrain)
#Partition the data
training<- Sonar[intrain,]
testing<-Sonar[-intrain,]
nrow(training)
nrow(testing)
#Tuning a Model using an Algorithm -> 'Train' Function
#PLSDA (partial least squares discriminant analysis) model
#This model will be tuned over the number of PLS components.
#The 'ctrl' Function is a trainControl function
#The Set(.seed) function basically locks the dataset determined for the model
set.seed(123)
ctrl<-trainControl(method = "repeatedcv",
                   repeats = 3,
                   classProbs = TRUE,
                   summaryFunction = twoClassSummary)
plsfit<-train(Class~.,
              data = training,
              method = "pls",
              tuneLength = 15,
              trControl = ctrl,
              metric = "ROC",
              preproc = c("center","scale"))
plsfit
#Based on the outcome value (ncomp) a PLS Model is fit to the whole data set.
#Visualisation with the 'plot' function.

plsClasses<-predict(plsfit,newdata = testing)
str(plsClasses)
plot(plsfit)
plsProbs<-predict(plsfit,newdata = testing,type = "prob")
head(plsProbs)
confusionMatrix(data = plsClasses, testing$Class)
#differentModel
library(klaR)
rdaGrid=data.frame(gamma = (0:4)/4, lambda = 3/4)
set.seed(123)
rdaFit<-train(Class ~.,
              data = training,
              method = "rda",
              tuneGrid = rdaGrid,
              trControl = ctrl,
              metrix = "ROC")
rdaFit
#
rdaClasses<-predict(rdaFit,newdata = testing)
confusionMatrix(rdaClasses, testing$Class)
#
resamps<-resamples(list(pls=plsfit, rda=rdaFit))
summary(resamps)                   
#
diffs<-diff(resamps)
summary(diffs)
xyplot(resamps,what = "BlandAltman")
#read.csv(C:Users/alexw/Desktop/Ubiqum/Section 2/Task 2/Survey_Key_and_Complete_Responses_excel)
