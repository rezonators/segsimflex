library(data.table)
library(readr)
library(reshape)
library(tidyverse)
library(dplyr)
## only need to change file name in line 406 407


# Replace -- to +
# can only deal with single notes
reDS <- function(d){
  result=mutate(d, Utterance = (str_replace_all(d$Utterance, "--", '+')))
  return(result)
} # input raw dataframe
# Replace NA speakers with the speaker above
reNA <- function(d){
  for (i in seq(1,length(d$Utterance))){  # for each row
    if (is.na(d$Speaker[i])){
      d$Speaker[i]=d$Speaker[i-1]
    }
  }
  return(d)
}  # input reDS
# Separate speakers into a dataframe
sepSpeaker <- function(d){
  num_speaker=length(levels(as.factor(d$Speaker)))  # numbder of speakers
  # Create empty datadrame
  sep=data.frame(array(dim = c(max(summary(as.factor(d$Speaker))),1,num_speaker))) # seperation result
  colnames(sep)=levels(as.factor(d$Speaker))
  # Separate
  for (s in seq(1,num_speaker)){  # for each speaker
    for (i in seq(1,length(d$Utterance))){  # for each row
      if (d$Speaker[i]==levels(as.factor(d$Speaker))[s]){
        sep[i,s]=d$Utterance[i]
      }
    }
  }
  return(sep)
}  # input reNA


# Generate boundary lists
# return one list of boundary
# + for --
# ; for IU boundary without a punctuation
genBd <- function(d,sep,boundaries,noboundary){

  punct_regex = paste0(" (", paste0(boundaries, collapse = "|"), ")")
  num_speaker=length(levels(as.factor(d$Speaker))) # number of speakers
  # remove empty space before punc
  for (s in seq(1,num_speaker)){  # for each speaker
    for (i in seq(1,length(sep[,s]))){  # for each row
      if (!(is.na(substr(sep[i,s],nchar(sep[i,s])-1,nchar(sep[i,s])-1)))){
        if(substr(sep[i,s],nchar(sep[i,s]),nchar(sep[i,s])) %in% boundaries){
          if (substr(sep[i,s],nchar(sep[i,s])-1,nchar(sep[i,s])-1) == " "){
            sep[i,s] = paste0(substr(sep[i,s],1,nchar(sep[i,s])-2),
                              substr(sep[i,s],nchar(sep[i,s]),nchar(sep[i,s])))
          }
        }
      }
    }
  }


  bd=data.frame(array(c('',''),dim = c(1,1,num_speaker)))  # result boundary list
  colnames(bd)=levels(as.factor(d$Speaker))
  # fill in boundary lists
  for (s in seq(1,num_speaker)){  # for each speaker
    for (i in seq(1,length(sep[,s]))){  # for each row
      if (!(is.na(sep[i,s]))){
        bd[1,s] = paste0(bd[1,s], str_extract_all(sep[i, s], " ")[[1]] %>% paste0(collapse = ""))
        lastChar = substring(sep[i,s],
                             nchar(sep[i,s]),
                             nchar(sep[i,s]))
        if(lastChar %in% boundaries){
          bd[1,s]=paste0(bd[1,s],lastChar)
        }else{
          bd[1,s]=paste0(bd[1,s],noboundary)
        }
      }
    }
  }
  return(bd)
}  # input reNA, sepSpeaker
# With record
# Calculate cost of two boundary lists
# sub=1 cost, trans=0.5 cost/dist
calCost <- function(l1,l2,m=matrix(data =c(1,0,0,0,0,0,0,
                                           0,1,0,0,0,0,0,
                                           0,0,1,0,0,0,0,
                                           0,0,0,1,0,0,0,
                                           0,0,0,0,1,0,0,
                                           0,0,0,0,0,1,0,
                                           0,0,0,0,0,0,1), nrow=7),order){
  record=data.frame()  # result dataframe of record process
  subCost=1  # pre-set substitution cost
  transCost=0.5  # pre-set transition cost of one space
  m=1-m
  if (length(l1)!=length(l2)){
    stop ("different speakers try again")
  }  # check speaker num
  for (s in seq(1,length(l1))){
    if (nchar(l1[s])!=nchar(l2[s])){
      stop ("different length of elements")
    }
  }  # check element length

  cost=0  # initialize the total cost
  t1=""
  t2=""  # temp for cost between two fixed boundries
  paresult = c("", "")
  for (s in seq(1,length(l1))){  # for each speacker
    for (i in seq(1,nchar(l1[s]))){  # for index of element in each list
      e1 =substring(l1[s],i,i)  # ith element in t1
      e2 =substring(l2[s],i,i)  # ith element in t2
      if (e1==e2 & e1!=" " & e2!=" "){
        if (t1!="" & t2!="" ){
          t11=data.frame(n=seq(nchar(t1)),e=c(sepChar(t1)[[1]]))
          t22=data.frame(n=seq(nchar(t2)),e=c(sepChar(t2)[[1]]))  # formated dataframe of t1 t2 and length list
          paresult=parSim(t11,t22,m,order)  # result of cost and record between two fixed boundries
          sd=sTOd(paresult[2])  # formated process of change
          cost=cost+as.numeric(paresult[1])
          if (paresult[2] !=""){
            df=data.frame(speaker=s,
                          type=sd$type,
                          oldPosition=as.numeric(sd$oldPosition)+i-length(t1),
                          newPosition=as.numeric(sd$newPosition)+i-length(t1),
                          oldChar=sd$oldChar,
                          newChar=sd$newChar)
            record = rbind(record,df)
          }
          t1=""
          t2=""}
      }else{
        if (e1!=e2 & e1!=" " & e2!=" " ){
          cost=cost+m[which (order == e1),which (order == e2)]
          if (paresult[2] !=""){
            df=data.frame(speaker=s,
                          type="s",
                          oldPosition=i,
                          newPosition=i,
                          oldChar=e1,
                          newChar=e2)
            record=rbind(record,df)
          }
          if (t1!="" & t2!= ""){
            t11=data.frame(n=seq(nchar(t1)),e=c(sepChar(t1)[[1]]))
            t22=data.frame(n=seq(nchar(t2)),e=c(sepChar(t2)[[1]]))
            paresult=parSim(t11,t22,m,order)
            sd=sTOd(paresult[2])
            cost=cost+as.numeric(paresult[1])
            if (paresult[2] !=""){
              df=data.frame(speaker=s,
                            type=sd$type,
                            oldPosition=as.numeric(sd$oldPosition)+i-length(t1),
                            newPosition=as.numeric(sd$newPosition)+i-length(t1),
                            oldChar=sd$oldChar,
                            newChar=sd$newChar)
              record = rbind(record,df)
            }
            t1=""
            t2=""
          }
        }else{
          t1=paste0(t1,e1)
          t2=paste0(t2,e2)
        }
      }
    }
    if (t1!="" & t2!= ""){
      t11=data.frame(n=seq(nchar(t1)),e=c(sepChar(t1)[[1]]))
      t22=data.frame(n=seq(nchar(t2)),e=c(sepChar(t2)[[1]]))
      paresult=parSim(t11,t22,m,order)
      sd=sTOd(paresult[2])
      cost=cost+as.numeric(paresult[1])
      if (paresult[2] !=""){
        df=data.frame(speaker=s,
                      type=sd$type,
                      oldPosition=as.numeric(sd$oldPosition)+i-length(t1),
                      newPosition=as.numeric(sd$newPosition)+i-length(t1),
                      oldChar=sd$oldChar,
                      newChar=sd$newChar)
        record = rbind(record,df)
      }
      t1=""
      t2=""
    }
  }
  return(c(cost,record))
}  # input 2 genBd
# With record
# Calculate cost between each sub string Without record
parSim <- function(t1,t2,m=matrix(data =c(1,0,0,0,0,0,0,
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


# input two sub boundary list
# Without record
# Calculate cost of two boundary lists, and confusion matrix m
# ","  "." "?" "-" "+" ";" " "
# + for --
# ; for IU boundary without a punctuation
# in m is simscore subcost=(1-cost)/1
# order= c(",", ".", "?", "-", "+", ";", " ")
# m=matrix(data =c(1,0,0,0,0,0,0,
#                  0,1,0,0,0,0,0,
#                  0,0,1,0,0,0,0,
#                  0,0,0,1,0,0,0,
#                  0,0,0,0,1,0,0,
#                  0,0,0,0,0,1,0,
#                  0,0,0,0,0,0,1), nrow=7)

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
calCost1<-function(l1,l2, m, order){
  record=data.frame()
  transCost=0.5
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
  t1=""
  t2=""  # temp for cost between two fixed boundries
  for (s in seq(1,length(l1))){  # for each speacker
    for (i in seq(1,nchar(l1[s]))){  # for index of element in each list
      e1 =substring(l1[s],i,i)
      e2 =substring(l2[s],i,i)
      if (e1==e2 & e1!=" " & e2!=" "){
        cost=cost+parSim1(t1,t2,m, order)
        #cost=cost+parSim1V2(t1,t2,m, order)
        # record
        t1=""
        t2=""
      }else{
        if (e1!=e2 & e1!=" " & e2!=" " ){
          cost=cost+m[which (order == e1),which (order == e2)]
          record=rbind(record,data.frame(old= e1, new = e2))
          cost=cost+parSim1(t1,t2,m, order)
          #cost=cost+parSim1V2(t1,t2,m, order)
          # record
          t1=""
          t2=""
        }else{
          t1=paste0(t1,e1)
          t2=paste0(t2,e2)
        }
      }
    }
    cost=cost+parSim1(t1,t2,m, order)
    #cost=cost+parSim1V2(t1,t2,m, order)
    # record
    t1=""
    t2=""
  }
  return(cost)
}  # input 2 genBd
# Without record
# Calulate cost between each sub string Without record
parSim1<- function(t1,t2, m, order){
  transCost=0.5
  e1 =substring(t1,1,1)
  e2 =substring(t2,1,1)
  s1=substring(t1,2,nchar(t1))
  s2=substring(t2,2,nchar(t2))
  t=paste0(substring(t2,2,2),substring(t2,1,1),substring(t2,3,nchar(t2)))
  if (nchar(t1)<=1){
    if (e1==e2){
      return (0)
    }else{
      return (m[which (order == e1),which (order == e2)])
    }
  }else{
    if (e1==e2){
      return(parSim1(s1,s2,m, order))
    }else{
      if(e1!=" " & e2!=" "){
        return(m[which (order == e1),which (order == e2)]+parSim1(s1,s2,m, order))
      }else{
        if ((substring(t2,2,2)!=" " & substring(t2,1,1)==" ")
            | (substring(t2,2,2)==" " & substring(t2,1,1)!=" ")){
          return(min(m[which (order == e1),which (order == e2)]+parSim1(s1,s2,m, order),transCost+parSim1(t1,t,m, order)))
        }else{
          return(m[which (order == e1),which (order == e2)]+parSim1(s1,s2,m, order))
        }
      }
    }
  }
}

transpose = function(string, pos1){
  paste0(substring(string, 1, pos1-1), substring(string, pos1 + 1, pos1 + 1), substring(string, pos1, pos1), substring(string, pos1 + 2, nchar(string)))
}


parSim1V2 <- function(t1,t2, m, order, max = Inf, costSoFar = 0, indent = ""){
  if(costSoFar < max){
  indent = paste0(">",indent)
  t1_trailing_sp = str_extract_last(t1, " +") %>% nchar %>% replace_na(0)
  t2_trailing_sp = str_extract_last(t2, " +") %>% nchar %>% replace_na(0)
  trailing_sp = min(t1_trailing_sp, t2_trailing_sp)

  t1_leading_sp = str_extract_first(t1, " +") %>% nchar %>% replace_na(0)
  t2_leading_sp = str_extract_first(t2, " +") %>% nchar %>% replace_na(0)
  leading_sp = min(t1_leading_sp, t2_leading_sp)

  t1 = substring(t1, 1 + leading_sp, nchar(t1) - trailing_sp)
  t2 = substring(t2, 1 + leading_sp, nchar(t2) - trailing_sp)

  #print(paste0(indent, "t1:", t1, "; t2:", t2, "; Max:", max, "; costSoFar:", costSoFar))
  transCost=0.5
  e1 =substring(t1,1,1)
  e2 =substring(t2,1,1)
  f1 =substring(t1,nchar(t1),nchar(t1))
  f2 =substring(t2,nchar(t2),nchar(t2))
  s1=substring(t1,2,nchar(t1))
  s2=substring(t2,2,nchar(t2))

  t1_list = strsplit(t1, "")[[1]]
  t2_list = strsplit(t2, "")[[1]]
  matches = which(t1_list != " " & t2_list != " ")

  t=paste0(substring(t2,2,2),substring(t2,1,1),substring(t2,3,nchar(t2)))
  t2_first_nonsp = str_locate(t2, "[^ ]")[1]
  if(!is.na(t2_first_nonsp) & t2_first_nonsp > 1){
    tfor=transpose(t2, str_locate(t2, "[^ ]")[1] - 1)
  }
  if (nchar(t1)<=1){
    if (e1==e2){
      result = (0)
    }else{
      result = (m[which (order == e1),which (order == e2)])
    }
  }else{
    if (e1==e2){
      result =(parSim1V2(sfin1,sfin2,m, order, max, costSoFar, indent))
    }else{
      if(e1!=" " & e2!=" "){
        opCost = m[which (order == e1),which (order == e2)]
        result =(opCost+parSim1V2(s1,s2,m, order, max, costSoFar + opCost, indent))
      } else if(length(matches) > 1){
        opCost = m[which (order == t1[matches[1]]),which (order == t2[matches[1]])]
        t1_p1 = substring(t1, 1, matches[1] - 1)
        t1_p2 = substring(t1, matches[1] + 1, nchar(t1))
        t2_p1 = substring(t2, 1, matches[1] - 1)
        t2_p2 = substring(t2, matches[1] + 1, nchar(t2))
        result =(opCost+
                   parSim1V2(t1_p1,t1_p2,m, order, max, costSoFar + opCost, indent)+
                   parSim1V2(t2_p1,t2_p2,m, order, max, costSoFar + opCost, indent))
      } else{
        if ((substring(t2,2,2)!=" " & substring(t2,1,1)==" ") |
            (substring(t2,2,2)==" " & substring(t2,1,1)!=" ")){
          option1 = m[which (order == e1),which (order == e2)]+parSim1V2(s1,s2,m,order, max, costSoFar + m[which (order == e1),which (order == e2)], indent)
          option2 = transCost+parSim1V2(t1,t,m, order, max = min(max, option1, na.rm = T), costSoFar + transCost, indent)
          result = suppressWarnings(min(option1, option2, na.rm = T))
         } else if (!is.na(t2_first_nonsp) & t2_first_nonsp > 1){
           option1 = m[which (order == e1),which (order == e2)] +
             parSim1V2(s1,s2,m,order, max, costSoFar + m[which (order == e1),which (order == e2)], indent)
           option2 = transCost +
             parSim1V2(t1,tfor,m,order, max = min(max, option1, na.rm = T), costSoFar + transCost, indent)
           result =suppressWarnings(min(option1, option2, na.rm = T))
        }else{
          opCost = m[which (order == e1),which (order == e2)]
          result =(opCost+parSim1V2(s1,s2,m, order, max, costSoFar + opCost, indent))
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
# no record
calCostNoTrans1 <- function(l1,l2, m, order){
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
      }
    }
  }
  return(cost)
}

# Transform the parSim String to dataframe.
sTOd <- function(s){
  slist=strsplit(s,"&")  # split the process string by & into steps
  result= data.frame()  # result dataframe of process
  for (i in slist[[1]]){
    slist2=strsplit(i,"#")  # split the process string by # into 5 elements: type positions charactors
    a=slist2[[1]][1]  # type of the step :s or t
    if (a=="s"){
      result=rbind(result, data.frame(type="s",
                              oldPosition=slist2[[1]][2],
                              newPosition=slist2[[1]][4],
                              oldChar=slist2[[1]][3],
                              newChar=slist2[[1]][5]))
    }else{
      if (substring(i,3,3)==" "){
        result=rbind(result, data.frame(type="t",
                                 oldPosition=slist2[[1]][4],
                                 newPosition=slist2[[1]][2],
                                 oldChar=slist2[[1]][5],
                                 newChar=slist2[[1]][5]))
      }else{
        result=rbind(result, data.frame(type="t",
                                        oldPosition=slist2[[1]][2],
                                        newPosition=slist2[[1]][4],
                                        oldChar=slist2[[1]][3],
                                        newChar=slist2[[1]][3]))
      }

    }
  }
  return(result)
}  # input boundary cost result
# Separate strings
sepChar <- function(t1){
  return (strsplit(t1,''))
}
# Calculate bound number
bdNum <- function(bd){  # input boundary list
  n=0  # number of boundries in a list
  l=0  # temperate variable for counting
  for (s in length(bd)){  # for each speacker
    for (i in seq(1,nchar(bd[s]))){  # for index of element in each list
      e =substring(bd[1,s],i,i)
      l=l+1
    }
    n=n+l
    l=0
  }
  return(n)
}

bdNumV2 = function(bd){
  bdlist1 %>% nchar %>% sum
}

# input genBd
# calculate sim score
simScore <- function(n, cost){
  return((n-cost)/(n))
}  # input bdNum, calCost
# check difference between two bdlists
checkDiff <- function(l1,l2){
  d1=''
  d2=''
  for (s in seq(1,length(l1))){  # for each speacker
    for (i in seq(1,nchar(l1[s]))){  # for index of element in each list
      e1 =substring(l1[s],i,i)
      e2 =substring(l2[s],i,i)
      if (e1!=e2){
        d1=paste0(d1,'|',e1,i)
        d2=paste0(d2,'|',e2,i)
      }
    }
  }
  d1=paste0(d1,'|')
  d2=paste0(d2,'|')
  return(data.frame(d1,d2))
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

#calCost without report - alternative by Ryan
calCost1V2 <- function(l1,l2,m=matrix(data =c(1,0,0,0,0,0,0,
                                             0,1,0,0,0,0,0,
                                             0,0,1,0,0,0,0,
                                             0,0,0,1,0,0,0,
                                             0,0,0,0,1,0,0,
                                             0,0,0,0,0,1,0,
                                             0,0,0,0,0,0,1), nrow=7),order){
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
    cost = cost + sapply(which(substPos), function(x) m[which(currBlistList1[x] == order), which(currBlistList2[x] == order)]) %>% sum

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

        t1_list = strsplit(t1, "")[[1]]
        t2_list = strsplit(t2, "")[[1]]
        if(all(t1_list == "")){ #No need to consider transposing\
          for(i in which(t2_list != " ")){
            cost = cost + m[which(order == " "), which(order == t2_list[i])]
          }
        } else if(all(t2_list == "")){
          for(i in which(t1_list != " ")){
            cost = cost + m[which(order == t1_list[i]), which(order == " ")]
          }
        } else {
          cost = cost + parSim1V2(t1,t2,m,order)  # result of cost and record between two fixed boundaries
        }
      }
    }
  }
  return(cost)
}  # input 2 genBd




#' Similarity score calculation
#'
#' @param d1 annotation_1 from read_csv, there are three columns in the csv files (Turn Speaker Utterance), each line is an Intonation Unit, space is used for tokenization, 'punctuation' are IU boundaries and should go to the end of each IU
#' @param d2 annotation_2 from read_csv, there are three columns in the csv files (Turn Speaker Utterance), each line is an Intonation Unit, space is used for tokenization, 'punctuation' are IU boundaries and should go to the end of each IU
#' @param record whether you want to get the step of transformation (slow process)
#' @param m similarity matrix to customize substitution cost
#' @param boundaries a list of boundary symbols that will exist in the data
#' @param noboundary assign a symbol for no boundary
#' @param trans choose to enable transposition action or not
#'
#' @return similarity score
#' @export
#'
#' @examples
#' sim_Score(nccu_t049_1, nccu_t049_2, record = T)
sim_Score<-function(d1,d2, record = FALSE, m=matrix(data =c(1,0,0,0,0,0,0,
                                                            0,1,0,0,0,0,0,
                                                            0,0,1,0,0,0,0,
                                                            0,0,0,1,0,0,0,
                                                            0,0,0,0,1,0,0,
                                                            0,0,0,0,0,1,0,
                                                            0,0,0,0,0,0,1), nrow=7),
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

  if (trans == TRUE){
    if (record == TRUE){
      cost=calCost(bdlist1,bdlist2,m,order)
      #cost=calCostV2(bdlist1,bdlist2,m,order)
      bdNumber=bdNumV2(bdlist1)
      sim=simScore(bdNumber,as.numeric(cost[1]))
      return(c(cost,sim))
    }else{
      cost=calCost1V2(bdlist1,bdlist2,m,order)
      bdNumber=bdNumV2(bdlist1)
      sim=simScore(bdNumber,cost)
      return(sim)
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

#For testing
#cbind(anno2p %>% mutate(ntok = Utterance %>% strsplit(" ") %>% sapply(length)), d %>% mutate(ntok2 = Utterance %>% strsplit(" ") %>% sapply(length))  %>% select(-Turn, -Speaker) %>% dplyr::rename(Utt2 = Utterance)) %>% mutate(ntok - ntok2) %>% filter(ntok != ntok2) %>% View

genBdV2 = function(d, boundaries, noboundary){
  boundary_regex = paste0(" (", sapply(boundaries, function(b) paste0("\\", strsplit(b, ""), collapse = "")) %>% paste0(collapse = "|"), ")")
  d = d %>% mutate(hasPunct = str_ends(Utterance, boundary_regex))
  d = d %>% mutate(Utterance = case_when(
    !hasPunct ~ paste0(Utterance, " ", noboundary),
    T ~ Utterance
  ))

  boundary_regex_all = paste0(" (", sapply(c(boundaries, noboundary), function(b) paste0("\\", strsplit(b, ""), collapse = "")) %>% paste0(collapse = "|"), ")")

  d = d %>%
    mutate(Utterance = paste0(substring(Utterance, 1, nchar(Utterance) - 2), substring(Utterance, nchar(Utterance), nchar(Utterance))))

  boundaries_perline = d %>%
    mutate(spaces = str_extract_all(Utterance, " ") %>% sapply(function(x) paste0(x, collapse = "")),
           punct = substring(Utterance, nchar(Utterance), nchar(Utterance))) %>%
    mutate(bounds = paste0(spaces, punct)) %>%
    mutate(nobounds = nchar(bounds)) %>%
    mutate(nowords = strsplit(Utterance, " ") %>% sapply(length))

  # boundaries_perline %>% group_by(Speaker) %>%
  #   summarise(space_num = nchar(spaces) %>% sum(),
  #             punct_num = n(),
  #             toks_num = strsplit(Utterance, " ") %>% sapply(length) %>% sum) %>%
  #   mutate(total_num = space_num + punct_num)

  bdlists = boundaries_perline %>% group_by(Speaker) %>% summarise(paste0(bounds, collapse = ""))
  bdlists = bdlists %>% as.matrix %>% t
  result = data.frame(bdlists)
  colnames(result) = bdlists[1,]
  result = result[-1,]
  result
}

#' Two separate intonation unit segmentations from text 049 of the
#' NCCU corpus.
"nccu_t049"

