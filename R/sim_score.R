library(readr)
library(reshape)
library(dplyr)

source("R/preprocessing.R")
source("R/calCost_noRecord.R")
source("R/calCost_Record.R")
source("R/helperFunctions.R")

#' Similarity score calculation
#'
#' @param d1 A data.frame of the first annotator's annotation. Each line represents a segment. Space is used for tokenisation, which may be spaces in the case of intonation unit segmentation, turn constructional units for turn segmentation, and so on.
#' @param d2 A data.frame of the second annotator's annotation, similar to `d1`.
#' @param record Whether you want to get the step of transformation (slow process!).
#' @param m A similarity matrix to customize substitution cost. The size of the matrix should either be the number of boundary types in `boundaries` plus two, if `noboundary` has been set, or the number of boundary types in `boundaries` plus one, otherwise. In both cases, the final column gives deletion cost, and the final row gives insertion cost. In the first case, the second-last row and column are for unclassified boundaries.
#' @param transCost a transposition cost: either a single value, or a vector with the same length as the number of rows/columns in `m`.
#' @param boundaries A vector of boundary symbols that will exist in the data.
#' @param noboundary A symbol assigned for unclassified boundary types. This will be appended to lines that do not end in any symbol found in `boundaries`. Use "" if unclassified boundaries are not allowed; lines not ending with a defined boundary type will then be treated as not ending in a boundary.
#' @param trans If `TRUE`, the transposition operation will be performed..
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
    if(noboundary != ""){
      m=diag(length(boundaries)+2) # no endnote and no boundary
    } else {
      m=diag(length(boundaries)+1)
    }
  }

  boundaries_singularised = find_multiBD(boundaries)
  boundaries[boundaries == names(boundaries_singularised)] = boundaries_singularised

  d1=reNA(d1) %>% replace_multiBD(boundaries_singularised)
  se1=sepSpeaker(d1)
  #bdlist1=genBd(d1,se1,boundaries,noboundary)
  bdlist1=genBdV2(d1,boundaries,noboundary)

  d2=reNA(d2) %>% replace_multiBD(boundaries_singularised)
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
      sim=costToScore(bdNumber,as.numeric(cost[1]))
      return(c(cost,sim))
    }else{
      cost=calCost1V2(bdlist1,bdlist2,m,order,transCost) #returns cost + no. of ops
      bdNumber=bdNumV2(bdlist1)
      sim_N =costToScore(bdNumber,cost[1])
      sim_B =costToScore(cost[2],cost[1])
      return(c(sim_N,sim_B))
    }
  } else{
    if (record == TRUE){
      cost=calCostNoTrans(bdlist1,bdlist2,m,order)
      bdNumber=bdNumV2(bdlist1)
      sim=costToScore(bdNumber,as.numeric(cost[1]))
      return(c(cost,sim))
    }else{
      cost=calCostNoTrans1(bdlist1,bdlist2,m,order)
      bdNumber=bdNumV2(bdlist1)
      sim=costToScore(bdNumber,cost)
      return(sim)
    }

  }

}



#' IU segmentations of squared-numbered texts in the NCCU Taiwan Mandarin Corpus
#'
#' @format ## `nccu_squareno`
#' A list of 7 items, each of which contains two items (annotations from each transcriber).
"nccu_squareno"
