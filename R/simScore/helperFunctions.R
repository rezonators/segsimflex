library(data.table)
library(readr)
library(reshape)
library(tidyverse)
library(dplyr)


#Extract something at the end
str_extract_last = function(strings, regex){
  contains = str_ends(strings, regex)
  locs = str_locate_all(strings, regex)
  sapply(1:length(strings), function(i){
    curr_locs = locs[[i]]
    if(nrow(curr_locs) > 0 & contains[i]) substring(strings[i], curr_locs[nrow(curr_locs), 1], curr_locs[nrow(curr_locs), 2])
    else NA
  })
}



str_extract_first = function(strings, regex){
  contains = str_starts(strings, regex)
  locs = str_locate(strings, regex)
  sapply(1:length(strings), function(i){
    curr_locs = locs[i,]
    if(contains[i]) substring(strings[i], curr_locs[1], curr_locs[2]) else NA
  })
}


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
  bd %>% nchar %>% sum
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


# deal with transcost
expandTrans <- function (t,o){
  if (length(t)==1){
    return(rep(t,length(o)))

  }else{
    return (t)
  }
}
