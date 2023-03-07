library(data.table)
library(readr)
library(reshape)
library(tidyverse)
library(dplyr)

source("R/preprocessing.R")
source("R/helperFunctions.R")




parDist1V3 <- function(t1,t2, m_cost , order, transCost, max = Inf, costSoFar = 0, cumulActions = 0){
  if(costSoFar < max){
    t1_trailing_sp = str_extract_last(t1, " +") %>% nchar %>% replace_na(0)
    t2_trailing_sp = str_extract_last(t2, " +") %>% nchar %>% replace_na(0)
    trailing_sp = min(t1_trailing_sp, t2_trailing_sp)

    t1_leading_sp = str_extract_first(t1, " +") %>% nchar %>% replace_na(0)
    t2_leading_sp = str_extract_first(t2, " +") %>% nchar %>% replace_na(0)
    leading_sp = min(t1_leading_sp, t2_leading_sp)

    t1 = substring(t1, 1 + leading_sp, nchar(t1) - trailing_sp)
    t2 = substring(t2, 1 + leading_sp, nchar(t2) - trailing_sp)

    # indent = rep(">", cumulActions) %>% paste0(collapse="")
    # print(paste0(indent, "t1:'", t1, "'; t2:'", t2, "'; Max:", max, "; costSoFar:", costSoFar, "; cumulActions:", cumulActions))
    #transCost=0.5
    e1 =substring(t1,1,1)
    e2 =substring(t2,1,1)
    f1 =substring(t1,nchar(t1),nchar(t1))
    f2 =substring(t2,nchar(t2),nchar(t2))
    s1=substring(t1,2,nchar(t1))
    s2=substring(t2,2,nchar(t2))

    t1_list = strsplit(t1, "")[[1]]
    t2_list = strsplit(t2, "")[[1]]
    matches = which(t1_list != " " & t2_list != " ")

    t1_first_nonsp = str_locate(t1, "[^ ]")[1]
    if(!is.na(t1_first_nonsp) & t1_first_nonsp > 1){
      tback=transpose(t2, 1, t1_first_nonsp)
    }
    t2_first_nonsp = str_locate(t2, "[^ ]")[1]
    if(!is.na(t2_first_nonsp) & t2_first_nonsp > 1){
      tfor=transpose(t2, 1, t2_first_nonsp)
    }

    if (nchar(t1)<=1){
      if (e1==e2){
        result = c(0, cumulActions)
      }else{
        cumulActions = cumulActions + 1
        result = c(m_cost [which (order == e1),which (order == e2)], cumulActions)
      }
    }else{
      if (e1==e2){
        result = parDist1V3(s1,s2,m_cost , order, transCost, max, costSoFar, cumulActions)
      }else{
        if(e1!=" " & e2!=" "){
          #Reason why line below is commented out:
          #At this stage if there's still such substitution,
          #There must have been a transposition already.
          #cumulActions = cumulActions + 1
          opCost = m_cost [which (order == e1),which (order == e2)]
          result =(c(opCost, 0) + parDist1V3(s1,s2,m_cost , order, transCost, max, costSoFar + opCost, cumulActions))
        } else if(length(matches) > 0){
          opCost = m_cost [which (order == t1_list[matches[1]]),which (order == t2_list[matches[1]])]
          #Reason why line below is commented out:
          #At this stage if there's still such substitution,
          #There must have been a transposition already.
          #if(t1_list[matches[1]] != t2_list[matches[1]]) cumulActions = cumulActions + 1
          t1_p1 = substring(t1, 1, matches[1] - 1)
          t1_p2 = substring(t1, matches[1] + 1, nchar(t1))
          t2_p1 = substring(t2, 1, matches[1] - 1)
          t2_p2 = substring(t2, matches[1] + 1, nchar(t2))
          result =(c(opCost, -cumulActions) + #Because we're adding tgt two operations, there will be an extra copy of cumulActions, which we delete here
                     parDist1V3(t1_p1,t2_p1,m_cost , order, transCost, max, costSoFar + opCost, cumulActions) +
                     parDist1V3(t1_p2,t2_p2,m_cost , order, transCost, max, costSoFar + opCost, cumulActions))
        } else {
          if (!is.na(t1_first_nonsp) & t1_first_nonsp > 1){ #Moving the first char of t2 back or substituting
            cumulActions = cumulActions + 1
            option1 = c(m_cost [which (order == e1),which (order == e2)], 0) +
              parDist1V3(s1,s2,m_cost ,order, transCost, max,
                        costSoFar + m_cost [which (order == e1),which (order == e2)], cumulActions)
            option2 = c(transCost[which (order == t1_list[t1_first_nonsp])] * (t1_first_nonsp - 1), 0) +
              parDist1V3(t1,tback,m_cost , order, transCost,
                        max = min(max, costSoFar + option1[1], na.rm = T),
                        costSoFar + transCost[which (order == t1_list[t1_first_nonsp])] * (t1_first_nonsp - 1), cumulActions)
            if(any(is.na(option1)) & any(is.na(option2))){
              result = c(Inf, cumulActions)
            } else if (any(is.na(option1)) | ((option2[1] < option1[1]) %>% replace_na(F))){
              result = option2
            } else {
              result = option1
            }
          } else if(!is.na(t2_first_nonsp) & t2_first_nonsp > 1) { #Moving the first nonspace char of t2 front or substituting
            cumulActions = cumulActions + 1
            option1 = c(m_cost [which (order == e1),which (order == e2)], 0) +
              parDist1V3(s1,s2,m_cost ,order, transCost,max,
                        costSoFar + m_cost [which (order == e1),which (order == e2)], cumulActions)
            option2 = c(transCost[which (order == t2_list[t2_first_nonsp])] * (t2_first_nonsp - 1), 0) +
              parDist1V3(t1,tfor,m_cost ,order, transCost,
                        max = min(max, costSoFar + option1[1], na.rm = T),
                        costSoFar + transCost[which (order == t2_list[t2_first_nonsp])] * (t2_first_nonsp - 1), cumulActions)
            if(any(is.na(option1)) & any(is.na(option2))){
              result = c(Inf, cumulActions)
            } else if (any(is.na(option1)) | ((option2[1] < option1[1]) %>% replace_na(F))){
              result = option2
            } else {
              result = option1
            }
          } else {
            cumulActions = cumulActions + 1
            opCost = m_cost [which (order == e1),which (order == e2)]
            result =(c(opCost, 0) + parDist1V3(s1,s2,m_cost , order, transCost, max, costSoFar + opCost, cumulActions))
          }
        }
      }
    }
    result
    # if(!is.na(result)){
    #   if(result < max){
    #     result
    #   } else NA
    # } else NA
  } else NA
}  # input two sub boundary list

parDist1V4 <- function(t1,t2, m_cost , order, transCost, max = Inf, costSoFar = 0, cumulActions = 0, prevResultsEnv = NA){
  origCumulActions = cumulActions
  if(costSoFar < max){
    prevResultExists = F
    if(t1 %in% names(prevResultsEnv$result)){
      if(t2 %in% names(prevResultsEnv$result[[t1]])){
        prevResultExists = T
      }
    }
    if(!prevResultExists){
      t1_trailing_sp = str_extract_last(t1, " +") %>% nchar %>% replace_na(0)
      t2_trailing_sp = str_extract_last(t2, " +") %>% nchar %>% replace_na(0)
      trailing_sp = min(t1_trailing_sp, t2_trailing_sp)

      t1_leading_sp = str_extract_first(t1, " +") %>% nchar %>% replace_na(0)
      t2_leading_sp = str_extract_first(t2, " +") %>% nchar %>% replace_na(0)
      leading_sp = min(t1_leading_sp, t2_leading_sp)

      t1 = substring(t1, 1 + leading_sp, nchar(t1) - trailing_sp)
      t2 = substring(t2, 1 + leading_sp, nchar(t2) - trailing_sp)

      # indent = rep(">", cumulActions) %>% paste0(collapse="")
      # print(paste0(indent, "t1:'", t1, "'; t2:'", t2, "'; Max:", max, "; costSoFar:", costSoFar, "; cumulActions:", cumulActions))
      #transCost=0.5
      e1 =substring(t1,1,1)
      e2 =substring(t2,1,1)
      f1 =substring(t1,nchar(t1),nchar(t1))
      f2 =substring(t2,nchar(t2),nchar(t2))
      s1=substring(t1,2,nchar(t1))
      s2=substring(t2,2,nchar(t2))

      t1_list = strsplit(t1, "")[[1]]
      t2_list = strsplit(t2, "")[[1]]
      matches = which(t1_list != " " & t2_list != " ")

      t1_first_nonsp = str_locate(t1, "[^ ]")[1]
      if(!is.na(t1_first_nonsp) & t1_first_nonsp > 1){
        tback=transpose(t2, 1, t1_first_nonsp)
      }
      t2_first_nonsp = str_locate(t2, "[^ ]")[1]
      if(!is.na(t2_first_nonsp) & t2_first_nonsp > 1){
        tfor=transpose(t2, 1, t2_first_nonsp)
      }

      if (nchar(t1)<=1){
        if (e1==e2){
          result = c(0, cumulActions)
        }else{
          cumulActions = cumulActions + 1
          result = c(m_cost [which (order == e1),which (order == e2)], cumulActions)
        }
      }else{
        if (e1==e2){
          result = parDist1V4(s1,s2,m_cost , order, transCost, max, costSoFar, cumulActions, prevResultsEnv)
        }else{
          if(e1!=" " & e2!=" "){
            #Reason why line below is commented out:
            #At this stage if there's still such substitution,
            #There must have been a transposition already.
            #cumulActions = cumulActions + 1
            opCost = m_cost [which (order == e1),which (order == e2)]
            result =(c(opCost, 0) + parDist1V4(s1,s2,m, order, transCost, max, costSoFar + opCost, cumulActions, prevResultsEnv))
          } else if(length(matches) > 0){
            opCost = m_cost [which (order == t1_list[matches[1]]),which (order == t2_list[matches[1]])]
            #Reason why line below is commented out:
            #At this stage if there's still such substitution,
            #There must have been a transposition already.
            #if(t1_list[matches[1]] != t2_list[matches[1]]) cumulActions = cumulActions + 1
            t1_p1 = substring(t1, 1, matches[1] - 1)
            t1_p2 = substring(t1, matches[1] + 1, nchar(t1))
            t2_p1 = substring(t2, 1, matches[1] - 1)
            t2_p2 = substring(t2, matches[1] + 1, nchar(t2))
            result =(c(opCost, -cumulActions) + #Because we're adding tgt two operations, there will be an extra copy of cumulActions, which we delete here
                       parDist1V4(t1_p1,t2_p1,m_cost , order, transCost, max, costSoFar + opCost, cumulActions, prevResultsEnv) +
                       parDist1V4(t1_p2,t2_p2,m_cost , order, transCost, max, costSoFar + opCost, cumulActions, prevResultsEnv))
          } else {
            if (!is.na(t1_first_nonsp) & t1_first_nonsp > 1){ #Moving the first char of t2 back or substituting
              cumulActions = cumulActions + 1
              option1 = c(m_cost [which (order == e1),which (order == e2)], 0) +
                parDist1V4(s1,s2,m_cost ,order, transCost, max,
                          costSoFar + m_cost [which (order == e1),which (order == e2)], cumulActions, prevResultsEnv)
              option2 = c(transCost[which (order == t1_list[t1_first_nonsp])] * (t1_first_nonsp - 1), 0) +
                parDist1V4(t1,tback,m_cost , order, transCost,
                          max = min(max, costSoFar + option1[1], na.rm = T),
                          costSoFar + transCost[which (order == t1_list[t1_first_nonsp])] * (t1_first_nonsp - 1), cumulActions, prevResultsEnv)
              if(any(is.na(option1)) & any(is.na(option2))){
                result = c(Inf, cumulActions)
              } else if (any(is.na(option1)) | ((option2[1] < option1[1]) %>% replace_na(F))){
                result = option2
              } else {
                result = option1
              }
            } else if(!is.na(t2_first_nonsp) & t2_first_nonsp > 1) { #Moving the first nonspace char of t2 front or substituting
              cumulActions = cumulActions + 1
              option1 = c(m_cost [which (order == e1),which (order == e2)], 0) +
                parDist1V4(s1,s2,m_cost ,order, transCost,max,
                          costSoFar + m_cost [which (order == e1),which (order == e2)], cumulActions, prevResultsEnv)
              option2 = c(transCost[which (order == t2_list[t2_first_nonsp])] * (t2_first_nonsp - 1), 0) +
                parDist1V4(t1,tfor,m_cost ,order, transCost,
                          max = min(max, costSoFar + option1[1], na.rm = T),
                          costSoFar + transCost[which (order == t2_list[t2_first_nonsp])] * (t2_first_nonsp - 1), cumulActions, prevResultsEnv)
              if(any(is.na(option1)) & any(is.na(option2))){
                result = c(Inf, cumulActions)
              } else if (any(is.na(option1)) | ((option2[1] < option1[1]) %>% replace_na(F))){
                result = option2
              } else {
                result = option1
              }
            } else {
              cumulActions = cumulActions + 1
              opCost = m_cost [which (order == e1),which (order == e2)]
              result =(c(opCost, 0) + parDist1V4(s1,s2,m_cost , order, transCost, max, costSoFar + opCost, cumulActions, prevResultsEnv))
            }
          }
        }
      }
      if(!any(is.na(result))){
        if(t1 %in% prevResultsEnv[["result"]]){
          prevResultsEnv$result[[t1]] = list()
        }
        prevResultsEnv$result[[t1]][[t2]] = c(result[1], result[2] - origCumulActions)
      }
    } else {
      result = c(prevResultsEnv$result[[t1]][[t2]]) + c(0, cumulActions)
    }

    result
    # if(!is.na(result)){
    #   if(result < max){
    #     result
    #   } else NA
    # } else NA
  } else NA
}  # input two sub boundary list



# input two sub boundary list
# without trans
# no record
calCostNoTrans1 <- function(l1,l2, m, order){
  actions = 0
  m=1-m
  if (length(l1)!=length(l2)){
    return ("different speakers try again")
  }  # check speaker num
  for (s in seq(1,length(l1))){
    if (nchar(l1[s])!=nchar(l2[s])){
      return ("different length of elements")
    }
  }  # check element length

  cost=0
  for (s in seq(1,length(l1))){  # for each speacker
    for (i in seq(1,nchar(l1[s]))){  # for index of element in each list
      e1 =substring(l1[s],i,i)
      e2 =substring(l2[s],i,i)
      if (e1!=e2){
        cost=cost+(m[which (order == e1),which (order == e2)])
        actions=actions+1
      }
    }
  }
  return(c(cost, actions))
}


#' Calculate the cost of changing one annotation to the other
#'
#' @param l1 IU list1: a list of intonation units
#' @param l2 IU list1: a list of intonation units
#' @param m similarity matrix to customize substitution cost
#'
#' @return cost
#' @export
#'
#' @examples
#' m=matrix(data =
#' c(1,0,0,0,0,0,0,
#' 0,1,0,0,0,0,0,
#' 0,0,1,0,0,0,0,
#' 0,0,0,1,0,0,0,
#' 0,0,0,0,1,0,0,
#' 0,0,0,0,0,1,0,
#' 0,0,0,0,0,0,1), nrow=7)
calCost1V2 <- function(l1,l2,m_sim =matrix(data =c(1,0,0,0,0,0,0,
                                              0,1,0,0,0,0,0,
                                              0,0,1,0,0,0,0,
                                              0,0,0,1,0,0,0,
                                              0,0,0,0,1,0,0,
                                              0,0,0,0,0,1,0,
                                              0,0,0,0,0,0,1), nrow=7),order,transCost){
  subCost=1  # pre-set substitution cost
  # transCost=0.5  # pre-set transition cost of one space
  m_cost =1-m_sim #Similaritie sto differences

  if (length(l1)!=length(l2)){
    stop ("different speakers try again")
  }  # check speaker num
  for (s in seq(1,length(l1))){
    if (nchar(l1[s])!=nchar(l2[s])){
      stop ("different length of elements")
    }
  }  # check element length

  cost=0  # initialize the total cost
  actions = 0
  for (s in seq(1,length(l1))){  # for each speacker
    currBlist1 = l1[[s]]
    currBlist2 = l2[[s]]

    currBlistList1 = sapply(1:nchar(currBlist1), function(x) substring(currBlist1, x, x))
    currBlistList2 = sapply(1:nchar(currBlist2), function(x) substring(currBlist2, x, x))
    #First do all the straightforward substitutions
    posMatch = sapply(1:nchar(currBlist1), function(x) substring(currBlist1, x, x) != " " & substring(currBlist2, x, x) != " ")
    substPos = posMatch & sapply(1:nchar(currBlist1), function(x) substring(currBlist1, x, x) != substring(currBlist2, x, x))
    if(length(which(substPos)) > 0){
      cost = cost + sapply(which(substPos), function(x) m_cost [which(currBlistList1[x] == order), which(currBlistList2[x] == order)]) %>% sum
    }
    actions = actions + sum(posMatch)

    #Then get the areas between the places where both annotators put a boundary
    #transAreas = transitional areas between two places where both annotators
    #put a boundary
    transAreas1 = list()
    transAreas2 = list()
    if(posMatch[1]) currItem = 0 else {
      currItem = 1
      transAreas1[[currItem]] = character(0)
      transAreas2[[currItem]] = character(0)
    }
    for(i in 1:length(posMatch)){
      if(posMatch[i]){
        currItem = currItem + 1
        transAreas1[[currItem]] = character(0)
        transAreas2[[currItem]] = character(0)
      } else {
        transAreas1[[currItem]] = c(transAreas1[[currItem]], substring(currBlist1, i, i))
        transAreas2[[currItem]] = c(transAreas2[[currItem]], substring(currBlist2, i, i))
      }
    }

    spaceOnly = sapply(1:length(transAreas1), function(x) all(c(transAreas1[[x]], transAreas2[[x]]) == " "))
    transAreas1 = transAreas1[!spaceOnly]
    transAreas2 = transAreas2[!spaceOnly]

    #Now go through each of those non-matching transitional areas
    #And call parDist
    if(length(transAreas1) > 0){
      t1s = sapply(transAreas1, paste0, collapse = "")
      t2s = sapply(transAreas2, paste0, collapse = "")


      for(x in 1:length(t1s)){
        t1 = t1s[x]
        t2 = t2s[x]
        message(paste0("Speaker ", s, "/", length(l1), ", Segment ", x, "/", length(t1s), " - t1:", t1, "|t2:", t2))

        t1_trailing_sp = str_extract_last(t1, " +") %>% nchar %>% replace_na(0)
        t2_trailing_sp = str_extract_last(t2, " +") %>% nchar %>% replace_na(0)
        trailing_sp = min(t1_trailing_sp, t2_trailing_sp)

        t1_leading_sp = str_extract_first(t1, " +") %>% nchar %>% replace_na(0)
        t2_leading_sp = str_extract_first(t2, " +") %>% nchar %>% replace_na(0)
        leading_sp = min(t1_leading_sp, t2_leading_sp)

        t1 = substring(t1, 1 + leading_sp, nchar(t1) - trailing_sp)
        t2 = substring(t2, 1 + leading_sp, nchar(t2) - trailing_sp)

        t1_list = strsplit(t1, "")[[1]]
        t2_list = strsplit(t2, "")[[1]]
        if(all(t1_list == " ")){ #No need to consider transposing\
          for(i in which(t2_list != " ")){
            cost = cost + m_cost [which(order == " "), which(order == t2_list[i])]
            actions = actions + 1
          }
        } else if(all(t2_list == " ")){
          for(i in which(t1_list != " ")){
            cost = cost + m_cost [which(order == t1_list[i]), which(order == " ")]
            actions = actions + 1
          }
        } else {
          resultEnv = new.env()
          resultEnv$result = list()
          parDistResult = parDist1V4(t1,t2,m_cost ,order,transCost,prevResultsEnv = resultEnv)
          cost = cost + parDistResult[1]
          actions = actions + parDistResult[2]  # result of cost and record between two fixed boundaries
        }
      }
    }
  }
  return(c(cost, actions))
}  # input 2 genBd


