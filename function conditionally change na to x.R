function(df){
  NA<-which(is.na(df))
  while(
    NA[i]+1 == NA[i+1]){#value of the position is equal to the next position
    i<-i+1
    c<-c+1
    if(c<=20){
     df$col[NA[i-c]:NA[i]]<-df$col[NA[i-c]-1] 
    }
   } 
  }