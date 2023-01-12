library(data.table)
library(readr)
library(reshape)
library(tidyverse)
library(dplyr)
## only need to change file name in line 406 407

puncNum <- function(bd){ # Used to get the number of each kind's punctuation in data.bd is the number of punctuation
  commaNum = 0
  periodNum = 0
  questionNum = 0
  dashNum = 0
  n=0  # number of boundries in a list
  l=0  # temperate variable for counting
  for (s in length(bd)){  # for each speaker why length (bd) mens speaker here?
    for (i in seq(1,nchar(bd[s]))){  # for index of element in each list
      e =substring(bd[1,s],i,i)
      if (substring(bd[1,s],i,i) == ","){
        commaNum = commaNum + 1
      }
      if (substring(bd[1,s],i,i) == "."){
        periodNum = periodNum + 1
      }
      if (substring(bd[1,s],i,i) == "?"){
        questionNum = questionNum + 1
      }
      if (substring(bd[1,s],i,i) == "+"){
        dashNum = dashNum + 1
      }
      l=l+1
    }
    n=n+l
    l=0
  }
  Num = c(commaNum,periodNum,questionNum,dashNum)
  #print(typeof(Num))
  return(Num)
}
gePlace <- function(num,number){ # generate a place to insert punctuation
  # creating a list with sample function
  lst = list(sample(1:num, size = number, replace = T))
  return (lst)
}
insertPunc <- function(place,puncnum,number,num,bd){ #insert punctuation based on gePlace
  puncList = rep(" ",num)
  for (i in seq(1,num)){#use lengths to get the length of each data in ist
    for (j in seq(1,length(bd))){
      if (i %in% data1Place[[1]]){
        prob = floor(runif(1,min=1,max=number))
        if (1<=prob && prob<puncnum[1]){ #if the number is in range of comma
          data[[j]][] = ","
        }
        if (puncnum[1]<=prob && prob<(puncnum[2]+puncnum[1])){ #if the number is in range of period
          puncList[i] = "."
        }
        if ((puncnum[2]+puncnum[1])<=prob && prob<(puncnum[2]+puncnum[3]+puncnum[1])){ #if the number is in range of ?
          puncList[i] = "?"
        }
        if ((puncnum[2]+puncnum[3]+puncnum[1])<=prob && prob<(puncnum[2]+puncnum[3]+puncnum[4]+puncnum[1])){ #if the number is in range of ?
          puncList[i] = "+"
        }
      }
    }
  }
  puncList = paste(puncList, collapse = "")
  return (puncList)
}
speaker_num <- function(bd,dataPlace,number,puncnum,num){ #generate random puncList
  allP=c(",",".","?","+")
  puncList = rep(" ",num)
  for (i in seq(1,length(bd))){
    for (j in seq(1,nchar(bd[[i]]))){
      prob = floor(runif(1,min=1,max=num))
      if (1<=prob && prob<puncnum[1]){#if the number is in range of comma
        substring(bd[[i]],j,j) = ","
      }
      if (puncnum[1]<=prob && prob<(puncnum[2]+puncnum[1])){ #if the number is in range of period
        substring(bd[[i]],j,j) = "."
      }
      if ((puncnum[2]+puncnum[1])<=prob && prob<(puncnum[2]+puncnum[3]+puncnum[1])){ #if the number is in range of ?
        substring(bd[[i]],j,j) = "."
      }
      if ((puncnum[2]+puncnum[3]+puncnum[1])<=prob && prob<(puncnum[2]+puncnum[3]+puncnum[4]+puncnum[1])){ #if the number is in range of +
        substring(bd[[i]],j,j) = "."
      }
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
#' @param data1 annotation_1 from read_csv, there are three columns in the csv files (Turn Speaker Utterance), each line is an Intonation Unit, space is used for tokenization, 'punctuation' are IU boundaries and should go to the end of each IU
#' @param data2 annotation_2 from read_csv, there are three columns in the csv files (Turn Speaker Utterance), each line is an Intonation Unit, space is used for tokenization, 'punctuation' are IU boundaries and should go to the end of each IU
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

  data1Puncnum = puncNum(bd1)#number of punctuation in each kind
  data1Number = data1Puncnum[1]+data1Puncnum[2]+data1Puncnum[3]+data1Puncnum[4] #get total number of punctuation
  data1Place = gePlace(data1Num, data1Number)#num is total number of boundry, number is number of punctuation

  data2Puncnum = puncNum(bd2)
  data2Number = data2Puncnum[1]+data2Puncnum[2]+data2Puncnum[3]+data2Puncnum[4] #get total number of punctuation
  data2Place = gePlace(data2Num, data2Number)#num is total number of boundry, number is number of punctuation

  #main function
  tsim_N = numeric(K)
  tsim_B = numeric(K)
  bdNumber=bdNumV2(bd1)
  for (i in seq(1,K)){
    message(paste0("Doing iteration ", i))
    dataPunc1 = speaker_num(bd1,data1Place,data1Number,data1Puncnum,data1Num)
    dataPunc2 = speaker_num(bd2,data2Place,data2Number,data2Puncnum,data2Num)
    cost = suppressMessages(calCost1V2(dataPunc1,dataPunc2, m, order, transCost))#If only need to use calculate cost without using simularity score, use calCost1
    sim_N =simScore(bdNumber,cost[1])
    sim_B =simScore(cost[2],cost[1])
    tsim_N[i] = sim_N
    tsim_B[i] = sim_B
  }

  tsim_avg =c(mean(tsim_N),mean(tsim_B))

  Iaa=c((asim-tsim_avg)/(1-tsim_avg))#calculate IAA

  return(Iaa)

}

