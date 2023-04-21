library(readr)
library(tidyverse)

removeUT = function(data1){
  for (i in c(1:nrow(data1))) {
    if (substring(data1$Utterance[i],nchar(data1$Utterance[i])-2,nchar(data1$Utterance[i])-2) == ' '){
      data1$Utterance[i]= paste0(substring(data1$Utterance[i],
                                           1,
                                           nchar(data1$Utterance[i])-2))
    }
    if (substring(data1$Utterance[i],nchar(data1$Utterance[i])-1,nchar(data1$Utterance[i])-1) == ' '){
      data1$Utterance[i]= paste0(substring(data1$Utterance[i],
                                           1,
                                           nchar(data1$Utterance[i])-1))
    }
  }
  return(data1)
}


compareDiff = function(data1,data2){
  data1=removeUT(data1)
  data2=removeUT(data2)
  d1=c()
  d2=c()
  i=1
  j=1
  while (i<= min(nrow(data1),nrow(data2))){
    if (data1$Utterance[i]!=data2$Utterance[j]){
      if(grepl(data1$Utterance[i], data2$Utterance[j], fixed = TRUE)){  #i in j
        a=FALSE
        b=TRUE
        k=0
        while (b==TRUE){
          k=k+1
          if(grepl(data1$Utterance[i+k], data2$Utterance[j], fixed = TRUE)){  # i+k in j
            temp1= paste0(data1$Utterance[i],data1$Utterance[i+k])
            if(temp1==data2$Utterance[j]){
              i=i+k+1
              j=j+1
              a=TRUE
              b=FALSE
            }
          }else{
            b=FALSE
          }
        }
        if (a==FALSE){
          d1=c(d1,paste0(data1$Turn[i],data1$Utterance[i]))
          d2=c(d2,paste0(data1$Turn[j],data2$Utterance[j]))
          for (x in c(1:k)){
            d1=c(d1,paste0(data1$Turn[i+x],data1$Utterance[i+x]))
            d2=c(d2,paste0(data1$Turn[j],data2$Utterance[j]))
          }
          i=i+k+1
          j=j+1
        }
      }else{
        if(grepl(data2$Utterance[j], data1$Utterance[i], fixed = TRUE)){  #j in i

          a=FALSE
          b=TRUE
          k=0
          while (b==TRUE){
            k=k+1
            if(grepl(data2$Utterance[j+k], data1$Utterance[i], fixed = TRUE)){  # j+k in i
              temp1= paste0(data2$Utterance[j],data2$Utterance[j+k])
              if(temp1==data1$Utterance[i]){
                j=j+k+1
                i=i+1
                a=TRUE
                b=FALSE
              }
            }else{
              b=FALSE
            }
          }
          if (a==FALSE){
            d1=c(d1,paste0(data1$Turn[i],data1$Utterance[i]))
            d2=c(d2,paste0(data1$Turn[j],data2$Utterance[j]))
            for (x in c(1:k)){
              d1=c(d1,paste0(data1$Turn[i],data1$Utterance[i]))
              d2=c(d2,paste0(data1$Turn[j+x],data2$Utterance[j+x]))
            }
            j=j+k+1
            i=i+1
          }
        }else{
          d1=c(d1,paste0(data1$Turn[i],data1$Utterance[i]))
          d2=c(d2,paste0(data1$Turn[j],data2$Utterance[j]))
          j=j+1
          i=i+1
        }
      }
    }else{
      i=i+1
      j=j+1
    }
  }
  q=cbind(d1,d2)
  return(q)
}



