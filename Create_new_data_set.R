MyFactor<-factor(c("Northern_Europe","Eastern_Europe","Southern_Europe","Western_Eruope"))
print(MyFactor)
Regions<-c("Northern_Europe","Eastern_Europe","Southern_Europe","Western_Eruope")
Inhabitants<-c("Norwegians","Russians","Spanish","Dutch")
Weather<-c("Seasonal","Cold","Hot","Seasonal")
MyDataFrame<-data.frame(Regions,Inhabitants,Weather,stringsAsFactors = FALSE)
MyDataFrame
Regions
MyDataFrame[2]#2 indicates index number
MyDataFrame[2,]#the , indictes the row_number
str(MyDataFrame)
#Add another column to the data.frame
Language<-c("Norwegian","Russian","Spanish","Dutch")
MyDataFrame$Language<-Language
View(MyDataFrame)
MyDataFrame$Numbers1<-(c(2,3,4,5))
MyDataFrame$Numbers2<-(c(6,13,14,25))
#MyDataFrame$NumbersMult<-
if(find(MyDataFrame$Weather == "Hot")) 
      {print("it's true")
        }
#      
if(MyDataFrame$Weather == "hallo") 
  {MyDataFrame$Numbers3<-MyDataFrame$Numbers1*MyDataFrame$Numbers2
  }
MyDataFrame$Numbers3
#    
if(is.factor(MyDataFrame$Regions))
{ print("It's a factor")
} else
{ as.factor(MyDataFrame$Regions)
}

MyDataFrame
library(xlsx)
write.csv(Final.data.frame,"C:/Users/alexw/Desktop/Ubiqum/Section2/Task3/FinalProjectData.csv")
