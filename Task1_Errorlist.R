install.packages("readr")
library(readr)
IrisDataSet<-read.csv("C:/Users/alexw/Desktop/Ubiqum/Section 2/R Tutorial Data Sets/iris.csv")
attributes(IrisDataSet)
summary(IrisDataSet)
str(IrisDataSet)
names(IrisDataSet)
IrisDataSet$Species<- as.numeric(IrisDataSet$Species)
hist(IrisDataSet$Species)
plot(IrisDataSet$Sepal.Length)
na.exclude(IrisDataSet)
  qqnorm(IrisDataSet$Sepal.Length)
set.seed(123)
trainsize<-round(nrow(IrisDataSet)*0.2)
testsize<-nrow(IrisDataSet)-trainsize
trainsize
testsize
trainSet<-IrisDataSet[training_indices, ]
testSet<-IrisDataSet[-training_indices, ]
set.seed(405)
trainSet<-IrisDataSet[training_indices, ]
testSet<-IrisDataSet[-training_indices, ]
LinearModel<-lm(trainSet$Petal.Length~ trainSet$Petal.Width)
summary(LinearModel)install.packages("caret", dependencies = c("Depends", "Suggests"))

prediction<-predict(LinearModel,testSet)
#Warning message:
#'newdata' had 115 rows but variables found have 35 rows
prediction
