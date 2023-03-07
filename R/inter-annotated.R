library(data.table)
library(readr)
library(reshape)
library(tidyverse)
library(dplyr)
## only need to change file name in line 406 407

numBd <- function(bd, order){ # Used to get the number of each kind's boundry in data.bd is the number of boundry
  result = rep(0,length(order))
  n=0  # number of boundries in a list
  l=0  # temperate variable for counting
  for (s in length(bd)){  # for each speaker why length (bd) mens speaker here?
    for (i in seq(1,nchar(bd[s]))){  # for index of element in each list
      e =substring(bd[1,s],i,i)
      for (j in seq(1,length(order))){
        if (substring(bd[1,s],i,i) == order[j]){
             result[j] = result[j] + 1
        }
      }

      # if (substring(bd[1,s],i,i) == ","){
      #   commaNum = commaNum + 1
      # }
      # if (substring(bd[1,s],i,i) == "."){
      #   periodNum = periodNum + 1
      # }
      # if (substring(bd[1,s],i,i) == "?"){
      #   questionNum = questionNum + 1
      # }
      # if (substring(bd[1,s],i,i) == "+"){
      #   dashNum = dashNum + 1
      # }
      l=l+1
    }
    n=n+l
    l=0
  }
  #print(typeof(Num))
  return(result)


}
gePlace <- function(num,number){ # generate a place to insert boundry
  # creating a list with sample function
  lst = list(sample(1:num, size = number, replace = T))
  return (lst)
}
insertBd <- function(place,numBd,number,num,bd){ #insert boundry based on gePlace
  boundryList = rep(" ",num)
  for (i in seq(1,num)){#use lengths to get the length of each data in ist
    for (j in seq(1,length(bd))){
      if (i %in% data1Place[[1]]){
        prob = floor(runif(1,min=1,max=number))
        if (1<=prob && prob<numBd[1]){ #if the number is in range of comma
          data[[j]][] = ","
        }
        if (numBd[1]<=prob && prob<(numBd[2]+numBd[1])){ #if the number is in range of period
          boundryList[i] = "."
        }
        if ((numBd[2]+numBd[1])<=prob && prob<(numBd[2]+numBd[3]+numBd[1])){ #if the number is in range of ?
          boundryList[i] = "?"
        }
        if ((numBd[2]+numBd[3]+numBd[1])<=prob && prob<(numBd[2]+numBd[3]+numBd[4]+numBd[1])){ #if the number is in range of ?
          boundryList[i] = "+"
        }
      }
    }
  }
  boundryList = paste(boundryList, collapse = "")
  return (boundryList)
}
speaker_num <- function(bd,dataPlace,number,numBd,num,order){ #generate random boundryList
  #allP=c(",",".","?","+")
  #boundryList = rep(" ",num)
  cumNumList = c(1, cumsum(numBd))
  for (i in seq(1,length(bd))){
    for (j in seq(1,nchar(bd[[i]]))){
      prob = floor(runif(1,min=1,max=num))

      for (k in seq(1,length(order))){
          if (prob>=cumNumList[k] && prob<cumNumList[k+1]){
            substring(bd[[i]],j,j) = order[k]
          }
      }
      # if (1<=prob && prob<numBd[1]){#if the number is in range of comma
      #   substring(bd[[i]],j,j) = ","
      # }
      # if (numBd[1]<=prob && prob<(numBd[2]+numBd[1])){ #if the number is in range of period
      #   substring(bd[[i]],j,j) = "."
      # }
      # if ((numBd[2]+numBd[1])<=prob && prob<(numBd[2]+numBd[3]+numBd[1])){ #if the number is in range of ?
      #   substring(bd[[i]],j,j) = "."
      # }
      # if ((numBd[2]+numBd[3]+numBd[1])<=prob && prob<(numBd[2]+numBd[3]+numBd[4]+numBd[1])){ #if the number is in range of +
      #   substring(bd[[i]],j,j) = "."
      # }
    }
  }
  return (bd)
}

createBD <-function(d, boundaries,noboundary){
  d1=reDS(d)
  d1=reNA(d1)
  se1=sepSpeaker(d1)
  bdlist1=genBdV2(d1,boundaries,noboundary)
  return(bdlist1)
}

#' Inter-annotator agreement
#'
#' @param data1 annotation_1 from read_csv, there are three columns in the csv files (Turn Speaker Utterance), each line is an Intonation Unit, space is used for tokenization, 'boundry' are IU boundaries and should go to the end of each IU
#' @param data2 annotation_2 from read_csv, there are three columns in the csv files (Turn Speaker Utterance), each line is an Intonation Unit, space is used for tokenization, 'boundry' are IU boundaries and should go to the end of each IU
#' @param K Number of iterations
#' @param ... Parameters passed to calcost1V2
#'
#' @return IAA value for Inter-annotator agreement
#' @export
#'
IAA <- function(data1,data2, record = FALSE, m = NA,
                transCost=0.5,
                boundaries = c(",", ".", "?", "-", "+"),
                noboundary = ";",
                trans = TRUE,
                K = 100){

  asim=sim_Score(data1,data2, m = m, boundaries = boundaries, noboundary = noboundary, transCost = transCost, trans  = trans, record = F)#check similarity score of two data input

  #Remove all double spaces
  d1 = data1 %>% mutate(Utterance = str_replace_all(Utterance, " +", " "))
  d2 = data2 %>% mutate(Utterance = str_replace_all(Utterance, " +", " "))

  if (any(is.na(m))){
    m=diag(length(boundaries)+2) # no endnote and no boundry
  }

  d1=reNA(d1) %>% reDS
  se1=sepSpeaker(d1)
  #bdlist1=genBd(d1,se1,boundaries,noboundary)
  bd1=genBdV2(d1,boundaries,noboundary)

  d2=reNA(d2) %>% reDS
  se2=sepSpeaker(d2)
  #bdlist2=genBd(d2,se2,boundaries,noboundary)
  bd2=genBdV2(d2,boundaries,noboundary)
  #calCost1V2(bd1, bd2, m, order, transCost)

  order = c(boundaries,noboundary,' ')

  # deal with transcost
  transCost=expandTrans(transCost,order)

  # for inter-annotated agreement
  data1Num = bdNum(bd1)  # calculate the number of boundries for a file
  data2Num = bdNum(bd2)  # calculate the number of boundries for a file

  data1numBd = numBd(bd1, order)#number of boundry in each kind
  data1Number = sum(data1numBd) #get total number of boundry
  data1Place = gePlace(data1Num, data1Number)#num is total number of boundry, number is number of boundry

  data2numBd = numBd(bd2, order)
  data2Number = sum(data2numBd) #get total number of
  data2Place = gePlace(data2Num, data2Number)#num is total number of boundry, number is number of boundry

  #main function
  tsim_N = numeric(K)
  tsim_B = numeric(K)
  bdNumber=bdNumV2(bd1)
  for (i in seq(1,K)){
    message(paste0("Doing iteration ", i))
    dataBd1 = speaker_num(bd1,data1Place,data1Number,data1numBd,data1Num,order)
    dataBd2 = speaker_num(bd2,data2Place,data2Number,data2numBd,data2Num,order)
    cost = suppressMessages(calCost1V2(dataBd1,dataBd2, m, order, transCost))#If only need to use calculate cost without using simularity score, use calCost1
    sim_N =simScore(bdNumber,cost[1])
    sim_B =simScore(cost[2],cost[1])
    tsim_N[i] = sim_N
    tsim_B[i] = sim_B
  }

  tsim_avg =c(mean(tsim_N),mean(tsim_B))

  Iaa=c((asim-tsim_avg)/(1-tsim_avg))#calculate IAA

  return(Iaa)

}


