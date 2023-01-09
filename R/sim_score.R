library(data.table)
library(readr)
library(reshape)
library(tidyverse)
library(dplyr)

source("R/preprocessing.R")
source("R/calCost_noRecord.R")
source("R/calCost_Record.R")
source("R/helperFunctions.R")

#' Similarity score calculation
#'
#' @param d1 annotation_1 from read_csv, there are three columns in the csv files (Turn Speaker Utterance), each line is an Intonation Unit, space is used for tokenization, 'punctuation' are IU boundaries and should go to the end of each IU
#' @param d2 annotation_2 from read_csv, there are three columns in the csv files (Turn Speaker Utterance), each line is an Intonation Unit, space is used for tokenization, 'punctuation' are IU boundaries and should go to the end of each IU
#' @param record whether you want to get the step of transformation (slow process)
#' @param m similarity matrix to customize substitution cost
#' @param transCost a transposition cost: either a single value, or a vector with the same length as the
#' @param boundaries a list of boundary symbols that will exist in the data
#' @param noboundary assign a symbol for no boundary
#' @param trans choose to enable transposition action or not
#'
#' @return similarity score
#' @export
#'
#' @examples
#' sim_Score(nccu_t049_1, nccu_t049_2, record = T)
sim_Score<-function(d1,d2, record = FALSE, m = NA,
                    transCost=0.5,
                    boundaries = c(",", ".", "?", "-", "+"),
                    noboundary = ";",
                    trans = TRUE){

  #Remove all double spaces
  d1 = d1 %>% mutate(Utterance = str_replace_all(Utterance, " +", " "))
  d2 = d2 %>% mutate(Utterance = str_replace_all(Utterance, " +", " "))

  if (any(is.na(m))){
    m=diag(length(boundaries)+2) # no endnote and no boundry
  }

  d1=reNA(d1) %>% reDS
  se1=sepSpeaker(d1)
  #bdlist1=genBd(d1,se1,boundaries,noboundary)
  bdlist1=genBdV2(d1,boundaries,noboundary)

  d2=reNA(d2) %>% reDS
  se2=sepSpeaker(d2)
  #bdlist2=genBd(d2,se2,boundaries,noboundary)
  bdlist2=genBdV2(d2,boundaries,noboundary)

  order = c(boundaries,noboundary,' ')

  # deal with transcost
  transCost=expandTrans(transCost,order)


  if (trans == TRUE){
    if (record == TRUE){
      cost=calCostV2(bdlist1,bdlist2,m,order, transCost)
      #cost=calCostV2(bdlist1,bdlist2,m,order)
      bdNumber=bdNumV2(bdlist1)
      sim=simScore(bdNumber,as.numeric(cost[1]))
      return(c(cost,sim))
    }else{
      cost=calCost1V2(bdlist1,bdlist2,m,order,transCost) #returns cost + no. of ops
      bdNumber=bdNumV2(bdlist1)
      sim_N =simScore(bdNumber,cost[1])
      sim_B =simScore(cost[2],cost[1])
      return(c(sim_N,sim_B))
    }
  } else{
    if (record == TRUE){
      cost=calCostNoTrans(bdlist1,bdlist2,m,order)
      bdNumber=bdNumV2(bdlist1)
      sim=simScore(bdNumber,as.numeric(cost[1]))
      return(c(cost,sim))
    }else{
      cost=calCostNoTrans1(bdlist1,bdlist2,m,order)
      bdNumber=bdNumV2(bdlist1)
      sim=simScore(bdNumber,cost)
      return(sim)
    }

  }

}

