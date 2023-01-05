library(data.table)
library(readr)
library(reshape)
library(tidyverse)
library(dplyr)

source("R/simScore/preprocessing.R")
source("R/simScore/helperFunctions.R")

# input two sub boundary list
# without trans
calCostNoTrans <- function(l1,l2, m, order){
  actions = 0
  record=data.frame()
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
        new_row = c("Substitution", e1, e2)
        record = rbind(record,new_row)
      }
    }
  }

  colnames(record) = c("type", "e1", "e2")
  return(c(cost, actions,record))
}


parSimV3 <- function(t1,t2, m, order, transCost, max = Inf, costSoFar = 0, cumulActions = 0){
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
        result = c(m[which (order == e1),which (order == e2)], cumulActions)
      }
    }else{
      if (e1==e2){
        result = parSimV3(s1,s2,m, order, transCost, max, costSoFar, cumulActions)
      }else{
        if(length(matches) > 0){
          opCost = m[which (order == t1_list[matches[1]]),which (order == t2_list[matches[1]])]
          if(t1_list[matches[1]] != t2_list[matches[1]]) cumulActions = cumulActions + 1
          t1_p1 = substring(t1, 1, matches[1] - 1)
          t1_p2 = substring(t1, matches[1] + 1, nchar(t1))
          t2_p1 = substring(t2, 1, matches[1] - 1)
          t2_p2 = substring(t2, matches[1] + 1, nchar(t2))
          result =(c(opCost, -cumulActions) + #Because we're adding tgt two operations, there will be an extra copy of cumulActions, which we delete here
                     parSimV3(t1_p1,t2_p1,m, order, transCost, max, costSoFar + opCost, cumulActions) +
                     parSimV3(t1_p2,t2_p2,m, order, transCost, max, costSoFar + opCost, cumulActions))
        } else if(e1!=" " & e2!=" "){
          cumulActions = cumulActions + 1
          opCost = m[which (order == e1),which (order == e2)]
          result =(c(opCost, 0) + parSimV3(s1,s2,m, order, transCost, max, costSoFar + opCost, cumulActions))
        } else {
          if (!is.na(t1_first_nonsp) & t1_first_nonsp > 1){ #Moving the first char of t2 back or substituting
            #Old condition:  (substring(t2,1,1)!=" " & substring(t2,1,1)!=" ")
            cumulActions = cumulActions + 1
            option1 = c(m[which (order == e1),which (order == e2)], 0) +
              parSimV3(s1,s2,m,order, transCost, max,
                        costSoFar + m[which (order == e1),which (order == e2)], cumulActions)
            option2 = c(transCost[which (order == e1)] * (t1_first_nonsp - 1), 0) +
              parSimV3(t1,tback,m, order, transCost,
                        max = min(max, costSoFar + option1[1], na.rm = T),
                        costSoFar + transCost[which (order == e1)] * (t1_first_nonsp - 1), cumulActions)
            if(any(is.na(option1)) & any(is.na(option2))){
              result = c(Inf, cumulActions)
            } else if (any(is.na(option1)) | ((option2[1] < option1[1]) %>% replace_na(F))){
              result = option2
            } else {
              result = option1
            }
          } else if(!is.na(t2_first_nonsp) & t2_first_nonsp > 1) { #Moving the first nonspace char of t2 front or substituting
            cumulActions = cumulActions + 1
            option1 = c(m[which (order == e1),which (order == e2)], 0) +
              parSimV3(s1,s2,m,order, transCost,max,
                        costSoFar + m[which (order == e1),which (order == e2)], cumulActions)
            option2 = c(transCost[which (order == e2)] * (t2_first_nonsp - 1), 0) +
              parSimV3(t1,tfor,m,order, transCost,
                        max = min(max, costSoFar + option1[1], na.rm = T),
                        costSoFar + transCost[which (order == e2)] * (t2_first_nonsp - 1), cumulActions)
            if(any(is.na(option1)) & any(is.na(option2))){
              result = c(Inf, cumulActions)
            } else if (any(is.na(option1)) | ((option2[1] < option1[1]) %>% replace_na(F))){
              result = option2
            } else {
              result = option1
            }
          } else {
            cumulActions = cumulActions + 1
            opCost = m[which (order == e1),which (order == e2)]
            result =(c(opCost, 0) + parSimV3(s1,s2,m, order, transCost, max, costSoFar + opCost, cumulActions))
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


#calCost without report - alternative by Ryan
calCostV2 <- function(l1,l2,m=matrix(data =c(1,0,0,0,0,0,0,
                                              0,1,0,0,0,0,0,
                                              0,0,1,0,0,0,0,
                                              0,0,0,1,0,0,0,
                                              0,0,0,0,1,0,0,
                                              0,0,0,0,0,1,0,
                                              0,0,0,0,0,0,1), nrow=7),order,transCost){

  record = data.frame()
  subCost=1  # pre-set substitution cost
  # transCost=0.5  # pre-set transition cost of one space
  m=1-m #Similaritie sto differences

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
      cost = cost + sapply(which(substPos), function(x) m[which(currBlistList1[x] == order), which(currBlistList2[x] == order)]) %>% sum

      for (pos in which(substPos)){
        new_row = c("Substitution", substring(currBlist1, pos, pos), substring(currBlist2, pos, pos))
        record = rbind(record,new_row)
      }

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
    #And call parSim
    if(length(transAreas1) > 0){
      t1s = sapply(transAreas1, paste0, collapse = "")
      t2s = sapply(transAreas2, paste0, collapse = "")


      for(x in 1:length(t1s)){
        t1 = t1s[x]
        t2 = t2s[x]
        print(paste0("Speaker ", s, "/", length(l1), ", Segment ", x, "/", length(t1s), " - t1:", t1, "|t2:", t2))

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
            cost = cost + m[which(order == " "), which(order == t2_list[i])]
            actions = actions + 1
          }
        } else if(all(t2_list == " ")){
          for(i in which(t1_list != " ")){
            cost = cost + m[which(order == t1_list[i]), which(order == " ")]
            actions = actions + 1
          }
        } else {
          parSimResult = parSimV3(t1,t2,m,order,transCost)
          cost = cost + parSimResult[1]
          actions = actions + parSimResult[2]  # result of cost and record between two fixed boundaries
        }
      }
    }
  }

  colnames(record) = c("type", "e1", "e2")
  return(c(cost, actions,record))
}  # input 2 genBd


