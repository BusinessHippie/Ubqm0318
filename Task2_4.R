#install.packages("arules")
#install.packages("arulesViz")
#### libraries ####

library(arules)
library(arulesViz)
Market_Basket_df<-read.csv(file = "C:/Users/alexw/Desktop/Ubiqum/Section2/Task4/ElectronidexTransactions2017.csv",header = FALSE)
Market_Basket_Transact<-read.transactions(file = "C:/Users/alexw/Desktop/Ubiqum/Section2/Task4/ElectronidexTransactions2017.csv",
                                 sep = ",",
                                 rm.duplicates = TRUE)
#
#### Take out single values ####
Market_Basket_df[Market_Basket_df == "iMac"]<-""
Market_Basket_df[Market_Basket_df == "HP Laptop"]<-""  #2
View(Market_Basket_df)
Market_Basket_df<-Market_Basket_df[!(Market_Basket_df$V1== ""),]
write.csv(Market_Basket_df,"C:/Users/alexw/Desktop/Ubiqum/Section2/Task4/ElectronidexModifiedTransact_ex_iMac_HPLapt.csv",
                            col.names = FALSE,
                            row.names = FALSE)
Market_Basket_Transact<-read.transactions(file = "C:/Users/alexw/Desktop/Ubiqum/Section2/Task4/ElectronidexModifiedTransact.csv",
                                          sep = ",",
                                          rm.duplicates = TRUE)
Market_Basket_Transact_ex_iMac_HPLapt<-read.transactions(file = "C:/Users/alexw/Desktop/Ubiqum/Section2/Task4/ElectronidexModifiedTransact_ex_iMac_HPLapt.csv",
                                          sep = ",",
                                          rm.duplicates = TRUE)

#Market_Basket_df<-Market_Basket_df[!(Market_Basket_df$V1== "iMac"& Market_Basket_df$V2== " "),]

#levels(Market_Basket_df$Acer.Aspire)<-c(levels(Market_Basket_df$Acer.Aspire),"NA")
#View(Market_Basket_df[Market_Basket_df[,1
#### Experiment with Data ####                                       
Market_Basket_df[Market_Basket_df == "",1]
Market_Basket_df[,1][,Market_Basket_df==""]<-NA
Market_Basket_df[!apply(is.na(Market_Basket_df) | Market_Basket_df == " ", 1, all),]                                      

summary(Market_Basket)
inspect(Market_Basket)
length(Market_Basket)
size(Market_Basket)
LIST(Market_Basket)
itemLabels(Market_Basket)
itemFrequencyPlot(Market_Basket_Transact,
                  support = 0.1,
                  topN = 10,
                  border="blue", 
                  col="green",
                  type = "absolute",
                  popCol = "black",
                  ylab = "Amount")
itemFrequencyPlot(Market_Basket_Transact_ex_iMac_HPLapt,
                  support = 0.1,
                  topN = 10,
                  type = "absolute",
                  popCol = "black",
                  ylab = "Amount")
?itemFrequencyPlot
image(sample(Market_Basket_Transact,200))
image(sample(Market_Basket_Transact_ex_iMac_HPLapt,200))
#?image

#### Apriori Algor. ####
#
# The first parameter SUPPORT measures the amount of rules (same itemsets) wihtin the dataset
# The second parameter CONFIDENCE measures the accuracy of the SUPPORT
Assoc.Rules_1<- apriori(Market_Basket_Transact,parameter = list(supp = 0.01, conf = 0.5,minlen=2,maxlen=5))
Assoc.Rules_2<- apriori(Market_Basket_Transact_ex_iMac_HPLapt,parameter = list(supp = 0.008, conf = 0.35,minlen=2,maxlen=5))
                        #appearance = list (default="lhs",rhs="Dell Desktop"))
                        #appearance = list(default="lhs",rhs="ASUS Monitor"))#in order to see what cust. bought after buying..
options(digits = 2)
inspect(Assoc.Rules_1)
inspect(Assoc.Rules_2)
summary(Assoc.Rules_1)
#### Subset Association Rules_1 ####
New_Rules<-Assoc.Rules_1[which(Assoc.Rules_1!="iMac"),]
#### Remove redundant rules ####

subsetRules <- which(colSums(is.subset(Assoc.Rules_1, Assoc.Rules_1)) > 1) # get subset rules in vector
length(subsetRules)
Assoc.Rules_1 <- Assoc.Rules_1[-subsetRules] # remove subset rules.
#________________________Sort by confidence______________________
rules_conf <- sort (Assoc.Rules_1, by="confidence", decreasing=TRUE)
inspect(head(rules_conf))
#________________________Sort by lift______________________
rules_lift <- sort (Assoc.Rules_1, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_lift)) # show the support, lift and confidence for all rules
#ASUS_Rules
#ASUS-Rules<-subset(Assoc.Rules_1,items %in% "ASUS Monitor")#
Final_Rules<-subset(Assoc.Rules_1[1:10])
inspect(Final_Rules)
ElectronicsVisual<-plot(Assoc.Rules_1,method = "graph",
                        control=list(type="items"),
                        interactive = NULL,
                        measure = "support", 
                        shading = "lift")
  
ElectronicsVisual
?plot
plot(method = "grouped",Final_Rules,interactive = TRUE)
#### Final analysis ####
#______________________Here it is important to conduct some LHS and RHS analyses to see causations