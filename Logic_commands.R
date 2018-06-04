####load dataset####
Alex<-read.csv("C:/Users/alexw/Desktop/Index_Match_Match_Example2.csv",header = TRUE)

#### Delete row conditionally ####

df <- df[!df$Colx == "is not" | df$Coly == "is also not",]
# The _ ! _ basically specifies "if is not... then..."
# -> If in Colx it says 'is not' OR in Coly it says 'is also not' then omit the row, 
# or in other words, dataframe df is defined as dataframe df without the specified arguments.
# [row,column]
#
df <- df[!df$Colx == "is not" & df$Coly == "is also not",]

data()

which(df$Colx != "is not")


a <- c(0,1,2,1,2,1,2,3,2,3,0,1)
which(a != 0 & a == 2)
####Delete row####
df<-df[c(1,2,3),]
####Delete col.####
df$Colx <- NULL

####Change row order####
df<- rbind(df[2:12,],df[1,])# assuming the df has 12 rows, the first line is placed towards the end of the dataframe
df[c("X2","X1"),]#by row names
df[c(2,1),]#by row indices

####change col.order####
df[,c("X2","X1")]#by col. names
df[,c(2,1)]#by col. indices
####Create function sample####
#Function Eli
locate <- function(df, id, rows, cols){
  df <- as.data.frame(df)
  id.row <- which(colnames(df) %in% id)
  pos.rows <- which(df[, id.row] %in% rows)
  pos.cols <- which(colnames(df) %in% cols)
  return(df[pos.rows, c(id.row, pos.cols)])
}
#
Alex1<-locate(Alex, "Year", c("1904", "1908", "1918", "1919"),
              c("House4", "House7", "House9"))
#My own function to subset DF
Function1<-function(df,id,rows,cols){
  df<-as.data.frame(df)
  df2<-df[which(df[,id] %in% rows),
          which(colnames(df) %in% cols)]
  #rownames(df2)<-as.character(rows)
}
#Run function
Alex1<-Function1(Alex,"Year",c(1904,1908,1918,1919),c("Year","House4","House7","House9"))
#Alternative 2: Which, which
Alex1<-Alex[which(Alex$Year %in% c("1904","1908","1918","1919")),
            which(colnames(Alex) %in% c("Year","House4","House7","House9"))]
#Alternative 3: Subset
#Define parameters
x<-c(1904,1908,1918,1919)
y<-c("Year","House4","House7","House9")
Alex1<-subset(Alex,Year %in% x,select = y)
#
#Omit columns that have the same value (100) in all rows
Building0<-subset(Wifi,BUILDINGID ==0)[,sapply(subset(Wifi,BUILDINGID ==0),function(x) !all(x==100))]
#Omit rows that have the same value (100) in all columns
Building0<-subset(Wifi,BUILDINGID ==0)[sapply(subset(Wifi,BUILDINGID ==0),function(x) !all(x==100)),]
#
#### The loop if####
#Delete all the columns where condition applies
to_del<-c()
for(i in 1:518){
  if(all(df[i] < (-80))){
    to_del<-cbind(to_del,i)
  }
}
df1 <- as.data.frame(Wifi[,-to_del])