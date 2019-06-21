library(caret)
library(ggplot2)
library(dplyr)
library(stats)
library(lubridate)
library(reshape2)
library(tidyverse)
library(corrplot)

Mall <- read.csv("C:/Users/alexw/Documents/R/Mall_customer_segmentation/Mall_Customers.csv", header = T)


summary(Mall)
str(Mall)
nrow(Mall)

colnames(Mall)[4] <- "Annual_income"
colnames(Mall)[5] <- "Spending_Score"

#Check for missing values
table(is.na(Mall))

boxplot(Mall$Annual_income)$out
hist(Mall$Spending_Score)

#Check for correlation among variables
cor(Mall[c(3,4,5)])
#Age seems to have the highest (negative) correlation with the Spending Score
summary(Mall$Age)

Mall <- Mall %>% mutate(Age_cat = ifelse(18 <= Mall$Age & Mall$Age <= 30, "Youngster",
                                         ifelse(30 <= Mall$Age & Mall$Age <= 50, "MiddleAger","Oldie")))
Mall$Age_cat <- as.factor(Mall$Age_cat)
Mall$Age_cat <- ordered(Mall$Age_cat, levels = c("Youngster","MiddleAger","Oldie"))

Mall <- Mall %>% mutate(Inc_cat = ifelse(15 <= Mall$Annual_income & Mall$Annual_income <= 41.50, "Low income",
                                         ifelse(41.50 < Mall$Annual_income & Mall$Annual_income <= 61.50, "Lower middle",
                                                ifelse(61.50 < Mall$Annual_income & Mall$Annual_income <= 78,"Upper middle","Rich bitch"))))
Mall$Inc_cat <- as.factor(Mall$Inc_cat)
#Reorder the factors
Mall$Inc_cat <- ordered(Mall$Inc_cat, levels = c("Low income","Lower middle","Upper middle","Rich bitch"))

Mall <- Mall %>% mutate(Gender_dummy = ifelse(Mall$Gender == "Male",1,2))

#Plotting Gender distribution
as.data.frame(table(Mall$Gender)) %>% ggplot(aes(x=Var1, y=Freq))+
  geom_bar(stat = "identity", fill = "purple")+
  geom_text(aes(label=Freq), colour = "white",position = position_stack(vjust = 0.5))

ggplot(Mall, aes(x=Age, fill = Gender))+
    geom_density(alpha = 0.4)+
    ggtitle("Density Plot Age Distribution")#+
    #coord_cartesian(ylim = c(0,0.05))



ggplot(Mall, aes(x=Annual_income))+ 
    geom_density(alpha = 0.4)+
    ggtitle("Density Plot Annual Income")+
    ylab("Density")

ggplot(data = Mall, aes(x = Annual_income, y = Spending_Score))+
  geom_point(color = "orange")+
  xlab("Income category")+
  ylab("Spending Score")


#_________________________________________________________________ IMPORTANT PLOT_______________________

ggplot(data = Mall, aes(x = Annual_income, y = Spending_Score))+
        geom_point(aes(color = Age_cat))+
        xlab("Income category")+
        ylab("Spending Score")
#_______________________________________________________________________________________________________


as.data.frame(table(Mall$Inc_cat)) %>% ggplot(aes(x=Var1, y=Freq))+
  geom_bar(stat = "identity", fill = "dark green")+
  geom_text(aes(label=Freq), colour = "white",position = position_stack(vjust = 0.5))


ggplot(data = Mall, aes(x = Age, y = Spending_Score))+
  geom_point(color = "red")+
  xlab("Age category")+
  ylab("Spending Score")

Mall$Spending_Score[which(Mall$Inc_cat == "Rich bitch" & Mall$Age_cat == "Youngster")]
mean(Mall$Spending_Score[which(Mall$Inc_cat == "Rich bitch" & Mall$Age_cat == "Youngster")])

Mall$Spending_Score[which(Mall$Inc_cat == "Rich bitch" & Mall$Age_cat == "MiddleAger")]
mean(Mall$Spending_Score[which(Mall$Inc_cat == "Rich bitch" & Mall$Age_cat == "MiddleAger")])

mean(Mall$Spending_Score[which(Mall$Age_cat == "Oldie")])
#37.5

Age_Income <- reshape2::dcast(Mall,Inc_cat ~ Age_cat, fun.aggregate = mean,value.var = "Annual_income")

ggplot(data = Mall, aes(x = Age_cat, y = Inc_cat))+
  geom_point(aes(size = Spending_Score, color = Spending_Score))+
  xlab("Age category")+
  ylab("Income level")+
  scale_size_continuous(range = c(0,16))+
  ggtitle("Relational Spending vs. Income based on Age")

ggplot(data = Mall, aes(x = Age_cat, y = Inc_cat))+
  geom_tile(aes(fill = Spending_Score), color = "yellow")+
  xlab("Age category")+
  ylab("Income level")+
  scale_size_continuous(range = c(0,16))+
  ggtitle("Relational Spending vs. Income based on Age")+
  scale_fill_gradient(low = "white", high = "red")

ggplot(data = Mall, aes(x = Gender, y = Age_cat))+
  geom_tile(aes(fill = Spending_Score), color = "yellow")+
  geom_text(aes(label = round(Spending_Score, 2)))+
  xlab("Gender")+
  ylab("Age")+
  scale_size_continuous(range = c(0,16))+
  ggtitle("Relational Spending vs. Age based on Gender")+
  scale_fill_gradient(low = "white", high = "red")


ggplot(data = Mall, aes(x=Gender, y=Inc_cat))+
      geom_count(color = "blue")+
      scale_size_continuous(range = c(0,12))
      
#count function -> length
length(which(Mall$Gender == "Male" & Mall$Inc_cat == "Low income"))

ggplot(data = Mall, aes(x=Gender, y=Inc_cat, color = Age_cat))+
  geom_point()#+
  #scale_size_continuous(range = c(0,12))

#Train ____ LINEAR MODEL####_______________________________________________________________

#Rows for train_set, using the sample function (alternative would be the 'createDataPartition' function)
'train_rows <- sample(nrow(Mall),nrow(Mall)*.75)
train_set <- Mall[train_rows,]
test_set <- Mall[-train_rows,]
test_set %in% train_set
#Create test_set without the Spending_Score
test_set1 <- test_set[,-which(names(test_set) == "Spending_Score")]

#Create Linear Model
Linear_Model <- lm(Spending_Score~Age+Annual_income+Gender_dummy, train_set)
#Apply Linear Model to testing set
LM_Predict <- predict(Linear_Model,newdata = test_set)
LM_Predict

postResample(LM_Predict,test_set$Spending_Score)
plot(LM_Predict)
lines(test_set$Spending_Score)'

Cluster <- kmeans(Mall[,c(4,5)],5)

ggplot(data = Mall[,c(4,5)], aes(x= Annual_income, y=Spending_Score))+
    geom_point(stat= "identity", aes(color= as.factor(Cluster$cluster)))+
    scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")) +
    ggtitle("Mall Customer Segmens", subtitle = "K-means Clustering")
