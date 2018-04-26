Betiply_stats<-read.csv(file = "C:/Users/alexw/Desktop/Betiply_stats_applied.csv",header = TRUE)
Betiply_stats
str(Betiply_stats)
gsub(" ","_",Betiply_stats)
#__The problem to be tackled was to remove the spaces within all the data of the dataset