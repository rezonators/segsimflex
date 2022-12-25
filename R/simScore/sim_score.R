library(data.table)
library(readr)
library(reshape)
library(tidyverse)
library(dplyr)

source("R/simScore/preprocessing.R")
source("R/simScore/calCost_noRecord.R")
source("R/simScore/calCost_Record.R")
source("R/simScore/helperFunctions.R")


sim_Score<-function(d1,d2, record = FALSE, m=matrix(data =c(1,0,0,0,0,0,0,
                                                            0,1,0,0,0,0,0,
                                                            0,0,1,0,0,0,0,
                                                            0,0,0,1,0,0,0,
                                                            0,0,0,0,1,0,0,
                                                            0,0,0,0,0,1,0,
                                                            0,0,0,0,0,0,1), nrow=7),
                    transCost=0.5,
                    boundaries = c(",", ".", "?", "-", "+"),
                    noboundary = ";",
                    trans = TRUE){

  #Remove all double spaces
  d1 = d1 %>% mutate(Utterance = str_replace_all(Utterance, " +", " "))
  d2 = d2 %>% mutate(Utterance = str_replace_all(Utterance, " +", " "))

  # check length of prefixed boundaries and matrix
  if (dim(m)[1] != length(boundaries)+2){
    stop ("Please keep the dimension of the matrix and boundary list the same")
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
      cost=calCost(bdlist1,bdlist2,m,order)
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

