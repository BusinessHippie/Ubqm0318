library(caret)
library(dplyr)
library(ggplot2)
library(ggplot)
library(corrplot)
library(stats)
library(arules)
library(reshape2)
library(tidyverse)
library(randomForest)

Wine <- read.csv("C:/Users/alexw/Desktop/Practice_Datasets/Wine quality/winequality-red.csv", header = T)

#_______________________________________________________Check for DUPLICATED VALUES
table(duplicated(Wine))
which(duplicated(Wine) == TRUE)

Wine <- Wine[-which(duplicated(Wine) == TRUE),]

#________________________________________________________Check for NA's
table(is.na(Wine))


boxplot(Wine)

#________________________________________________________Check for Correlation
corrplot(cor(Wine),type = "upper", method = "number")

hist(Wine$quality)

plot(x=Wine$fixed.acidity,y=Wine$quality)
barplot(x=Wine$fixed.acidity,y=Wine$quality)

'_________________________________________________________________PLOT variables against one another'
'_________________________________________________to inspect relationships and correlation'
pairs(Wine)


Wine %>%
  gather(-quality_factor, key = "variable", value = "value") %>%
  ggplot(aes(x = quality_factor, y = value)) +
  geom_point() +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()

#_________BAR_CHART_WITH SUMS
Wine %>%
  gather(-quality_factor, key = "variable", value = "value") %>%
  ggplot(aes(x = quality_factor, y = value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()

#_________BAR_CHART_WITH MEAN
Wine %>%
  gather(-quality_factor, key = "variable", value = "value") %>%
  ggplot(aes(x = quality_factor, y = value)) +
  geom_bar(stat = "summary",fun.y = "mean") + # Use Summary instead of identity to get the mean instead of the sum
  facet_wrap(~ variable, scales = "free") +
  theme_bw()

#ggplot(data = Wine, aes(x = quality_factor, y = mean(alcohol)))+
#  geom_bar()

Wine_mean <- Wine %>% group_by(quality_factor) %>% summarise_all(funs(mean))
Wine_mean[,-1] <- round(Wine_mean[,-1],digits = 2)

Wine_mean %>%
  gather(-quality_factor, key = "variable", value = "value") %>%
  ggplot(aes(x = quality_factor, y = value)) +
  geom_bar(stat = "identity")+
  facet_wrap(~ variable, scales = "free")+
  theme_bw()



boxplot(Wine[,c(1,2,3,5,7,8,9,10,11,12)])

Wine$quality_factor <- as.factor(Wine$quality)

ggplot(data= Wine)+
  geom_boxplot()+
  facet_wrap(~ variable,scales = "free")

#___Wine1 for the linear model
Wine1 <- Wine[,c(2,3,5,10,11,12)]
corrplot(cor(Wine1),type = "upper", method = "number")

trainrows <- sample(nrow(Wine1),nrow(Wine1)*.75)

trainSet <- data.frame(Wine1[trainrows,])
testSet <- data.frame(Wine1[-trainrows,])

#____________________________________Default Code to create a LINEAR MODEL
set.seed(123)
Linear_model <- lm(formula=trainSet$quality_factor ~ . , data=trainSet)

summary(Linear_model)
#predictions <- predict(Linear_model, testSet,
#                       interval="predict", level=.95)
#To see the actual and predicted values side by side so we can easily compare them, you can type in the following lines of code:
#comparison <- cbind(testSet$mpg, predictions[,1])
#colnames(comparison) <- c("actual", "predicted")

Wine2 <- Wine[,c(2,3,5,10,11,13)]

trainrows <- sample(nrow(Wine2),nrow(Wine2)*.75)

trainSet <- data.frame(Wine2[trainrows,])
testSet <- data.frame(Wine2[-trainrows,])

set.seed(111)
Model_control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 5,
                              verboseIter = T)
#________________________________________________________________________________KNN
knn_Model <- train(quality_factor ~ .,
                   data = trainSet,
                   method = "knn",
                   trControl= Model_control,
                   tuneLength = 10,
                   preProc = c("center", "scale"))

confusionMatrix(knn_Model)

#________________________________________________________________________________Random Forest Caret
rf_Model <- train(quality_factor ~ .,
                   data = trainSet,
                   method = "rf",
                   trControl= Model_control,
                   tuneLength = 20,
                   preProc = c("center", "scale"))

caret::confusionMatrix(rf_Model)

rf_predict <- predict(rf_Model, newdata = testSet)
rf_model_comparison <- data.frame(cbind(testSet$quality_factor, rf_predict))
colnames(rf_model_comparison) <- c("actual", "predicted")

caret::confusionMatrix(rf_predict,testSet$quality_factor)


#Predict completely new number based on random figures

predict(rf_Model, list(volatile.acidity = 0.26, citric.acid = 0.54, chlorides = 0.083, sulphates = 0.63, alcohol = 11.8))

#________________________________________________________________________________Random Forest package
trainrows <- sample(nrow(Wine2),nrow(Wine2)*.75)
trainSet <- data.frame(Wine2[trainrows,])
testSet <- data.frame(Wine2[-trainrows,])

rf_Model_alt <- randomForest(formula = quality_factor ~ . , data = trainSet, ntree = 200)
#rf_Model_alt <- randomForest(formula = quality_factor ~ . -quality, data = trainSet, ntree = 200)

#________________________________________________________________________________Support Vector Machine
svm_Model <- train(quality_factor ~ .,
                  data = trainSet,
                  method = "svmLinear",
                  trControl= Model_control,
                  tuneLength = 10,
                  preProc = c("center", "scale"))

confusionMatrix(svm_Model)
