library(data.table)
library(readr)
library(reshape)
library(tidyverse)
library(dplyr)

source("R/simScore/preprocessing.R")
source("R/simScore/helperFunctions.R")

parSimV2 <- function(t1,t2,m=matrix(data =c(1,0,0,0,0,0,0,
                                            0,1,0,0,0,0,0,
                                            0,0,1,0,0,0,0,
                                            0,0,0,1,0,0,0,
                                            0,0,0,0,1,0,0,
                                            0,0,0,0,0,1,0,
                                            0,0,0,0,0,0,1), nrow=7),order){
  transCost=0.5

  e1 =t1$e[1] # first element of t1
  e2 =t2$e[1] # first element of t2

  subCost = m[which (order == e1),which (order == e2)]

  s1=t1[2:nrow(t1),]  # sublist without first element
  s2=t2[2:nrow(t2),]

  t=rbind(t2[2,],t2[1,],t2[3:nrow(t1),])  # switch list of first two element

  sdf=paste0("s","#",t1$n[1],"#",e1,"#",t2$n[1],"#",e2,"&")  # substitution process record
  tdf=paste0("t","#",t1$n[1],"#",e1,"#",t1$n[2],"#",t1$e[2],"#",t2$n[1],"#",e2,"#",t2$n[2],"#",t2$e[2],"&") # substitution process record


  if (nrow(t1)<=1){
    if (e1==e2){
      return (c(0,''))
    }else{
      return (c(subCost,sdf))
    }
  }else{
    if (e1==e2){
      return(c(parSim(s1,s2,m,order)[1],
               paste0(parSim(s1,s2,m,order)[2])))
    }else{
      if(e1!=" " & e2!=" "){
        return(c(subCost+as.numeric(parSim(s1,s2,m,order)[1]),
                 paste0(parSim(s1,s2,m,order)[2],sdf))
        )
      }else{
        if ((t2$e[2]!=" " & t2$e[1]==" ")){
          subCase = parSim(s1,s2,m,order)
          transCase = parSim(t1,t,m,order)
          if (subCost+as.numeric(subCase[1])
              <transCost+as.numeric(transCase[1])){
            return(c(subCost+as.numeric(subCase[1]),
                     paste0(subCase[2],sdf))
            )
          }else{
            return(c(transCost+as.numeric(transCase),
                     paste0(transCase[2],tdf))
            )
          }
        }else{
          if((t2$e[2]==" " & t2$e[1]!=" ")){
            subCase = parSim(s1,s2,m,order)
            transCase = parSim(t1,t,m,order)
            if (subCost+as.numeric(subCase[1])
                <transCost+as.numeric(transCase[1])){
              return(c(subCost+as.numeric(subCase[1]),
                       paste0(subCase[2],sdf))
              )
            }else{
              return(c(transCost+as.numeric(transCase[1]),
                       paste0(transCase[2],tdf))
              )
            }
          }else{
            return(c(subCost+as.numeric(parSim(s1,s2,m,order)[1]),
                     paste0(parSim(s1,s2,m,order)[2],sdf))
            )
          }
        }
      }
    }
  }
}


transpose = function(string, pos1, pos2 = NA){
  if(is.na(pos2)) pos2 = pos1 + 1
  if(pos2 < pos1){
    pos2 = dummy
    pos2 = pos1
    pos1 = dummy
  }
  paste0(substring(string, 1, pos1-1),
         substring(string, pos2, pos2),
         substring(string, pos1 + 1, pos2 - 1),
         substring(string, pos1, pos1),
         substring(string, pos2 + 1, nchar(string)))
}


# input two sub boundary list
# without trans
# record
calCostNoTrans <- function(l1,l2, m, order){
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
        record=rbind(record, data.frame(e1= e1, e2=e2))
      }
    }
  }
  return(c(cost,record))
}

#calCost with report - alternative by Ryan
calCostV2 <- function(l1,l2,m=matrix(data =c(1,0,0,0,0,0,0,
                                             0,1,0,0,0,0,0,
                                             0,0,1,0,0,0,0,
                                             0,0,0,1,0,0,0,
                                             0,0,0,0,1,0,0,
                                             0,0,0,0,0,1,0,
                                             0,0,0,0,0,0,1), nrow=7),order){
  record=data.frame(speaker = integer(0),
                    type = character(0),
                    oldPosition = integer(0),
                    newPosition = integer(0),
                    oldChar = character(0),
                    newChar = character(0))  # result dataframe of record process
  subCost=1  # pre-set substitution cost
  transCost=0.5  # pre-set transition cost of one space
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
  for (s in seq(1,length(l1))){  # for each speacker
    currBlist1 = l1[[s]]
    currBlist2 = l2[[s]]

    currBlistList1 = sapply(1:nchar(currBlist1), function(x) substring(currBlist1, x, x))
    currBlistList2 = sapply(1:nchar(currBlist2), function(x) substring(currBlist2, x, x))
    #First do all the straightforward substitutions
    posMatch = sapply(1:nchar(currBlist1), function(x) substring(currBlist1, x, x) != " " & substring(currBlist2, x, x) != " ")
    substPos = posMatch & sapply(1:nchar(currBlist1), function(x) substring(currBlist1, x, x) != substring(currBlist2, x, x))


    substs = data.frame(speaker = s,
                        type = "s",
                        oldPosition = which(substPos),
                        newPosition = which(substPos),
                        oldChar= currBlistList1[substPos],
                        newChar= currBlistList2[substPos])
    record = rbind(record, substs)
    cost = cost + sapply(1:nrow(substs), function(x) m[which(substs$oldChar[x] == order), which(substs$newChar[x] == order)]) %>% sum

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

        t1_trailing_sp = str_extract_last(t1, " +") %>% nchar %>% replace_na(0)
        t2_trailing_sp = str_extract_last(t2, " +") %>% nchar %>% replace_na(0)
        trailing_sp = min(t1_trailing_sp, t2_trailing_sp)

        t1_leading_sp = str_extract_first(t1, " +") %>% nchar %>% replace_na(0)
        t2_leading_sp = str_extract_first(t2, " +") %>% nchar %>% replace_na(0)
        leading_sp = min(t1_leading_sp, t2_leading_sp)

        t1 = substring(t1, 1 + leading_sp, nchar(t1) - trailing_sp)
        t2 = substring(t2, 1 + leading_sp, nchar(t2) - trailing_sp)

        t11=data.frame(n=seq(nchar(t1)),e=c(sepChar(t1)[[1]]))
        t22=data.frame(n=seq(nchar(t2)),e=c(sepChar(t2)[[1]]))  # formatted dataframe of t1 t2 and length list

        if(all(strsplit(t1, " ")[[1]] == "")){ #No need to consider transposing\
          currCost = 0
          currRecord = ""
          for(i in which(t22$e != " ")){
            currCost = currCost + m[which(order == " "), which(order == t22$e[i])]
            currRecord = paste0(currRecord, "s#", t22$n[i], "# #", t22$n[i], "#", t22$e[i], "&")
          }
          paresult = c(as.character(currCost), currRecord)
        } else if(all(strsplit(t2, " ")[[1]] == "")){
          currCost = 0
          currRecord = ""
          for(i in which(t11$e != " ")){
            currCost = currCost + m[which(order == t11$e[i]), which(order == " ")]
            currRecord = paste0(currRecord, "s#", t11$n[i], "#", t11$e[i], "#", t11$n[i], "#", " &")
          }
          paresult = c(as.character(currCost), currRecord)
        } else {
          paresult=parSim(t11,t22,m,order)  # result of cost and record between two fixed boundaries
        }

        sd=sTOd(paresult[2])  # formatted process of change
        cost=cost+as.numeric(paresult[1])
        if (paresult[2] !=""){
          df=data.frame()
          record = add_row(record,
                           speaker=s,
                           type=sd$type,
                           oldPosition=as.numeric(sd$oldPosition)+i-length(t1),
                           newPosition=as.numeric(sd$newPosition)+i-length(t1),
                           oldChar=sd$oldChar,
                           newChar=sd$newChar)
        }
        t1=""
        t2=""
      }
    }
  }
  return(c(cost,record))
}  # input 2 genBd




