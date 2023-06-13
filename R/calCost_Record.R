# input two sub boundary list
# without trans
calCostNoTrans <- function(l1,l2, m_sim , order){
  actions = 0
  record=data.frame()
  m_cost =1-m_sim
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
        cost=cost+(m_cost [which (order == e1),which (order == e2)])
        actions=actions+1
        new_row = c("Substitution", e1, e2, i)
        record = rbind(record,new_row)
      }
    }
  }

  colnames(record) = c("type", "e1", "e2", "position")
  return(c(cost, actions,record))
}


parDistV3 <- function(t1,t2, m_cost , order, transCost, max = Inf, costSoFar = 0, cumulActions = 0,p1,p2){
  r = data.frame()
  print(costSoFar)
  if(costSoFar < max){
    t1_trailing_sp = str_extract_last(t1, " +") %>% nchar %>% replace_na(0)
    t2_trailing_sp = str_extract_last(t2, " +") %>% nchar %>% replace_na(0)
    trailing_sp = min(t1_trailing_sp, t2_trailing_sp)

    t1_leading_sp = str_extract_first(t1, " +") %>% nchar %>% replace_na(0)
    t2_leading_sp = str_extract_first(t2, " +") %>% nchar %>% replace_na(0)
    leading_sp = min(t1_leading_sp, t2_leading_sp)

    t1 = substring(t1, 1 + leading_sp, nchar(t1) - trailing_sp)
    t2 = substring(t2, 1 + leading_sp, nchar(t2) - trailing_sp)

    p1 = p1[(1 + leading_sp): (length(p1) - trailing_sp)]
    p2 = p2[(1 + leading_sp): (length(p2) - trailing_sp)]

    indent = rep(">", cumulActions) %>% paste0(collapse="")
    print(paste0(indent, "t1:'", t1, "'; t2:'", t2, "'; Max:", max, "; costSoFar:", costSoFar, "; cumulActions:", cumulActions))

    e1 =substring(t1,1,1)
    e2 =substring(t2,1,1)
    f1 =substring(t1,nchar(t1),nchar(t1))
    f2 =substring(t2,nchar(t2),nchar(t2))
    s1=substring(t1,2,nchar(t1))
    s2=substring(t2,2,nchar(t2))
    e1p=p1[1]
    e2p=p2[1]
    s1p=p1[2:length(p1)]
    s2p=p2[2:length(p2)]

    t1_list = strsplit(t1, "")[[1]]
    t2_list = strsplit(t2, "")[[1]]
    matches = which(t1_list != " " & t2_list != " ")

    t1_first_nonsp = str_locate(t1, "[^ ]")[1]
    if(!is.na(t1_first_nonsp) & t1_first_nonsp > 1){
      tback=transpose(t2, 1, t1_first_nonsp)
      pback=transposePos(p2,1, t1_first_nonsp)
    }
    t2_first_nonsp = str_locate(t2, "[^ ]")[1]
    if(!is.na(t2_first_nonsp) & t2_first_nonsp > 1){
      tfor=transpose(t2, 1, t2_first_nonsp)
      pfor=transposePos(p2, 1, t2_first_nonsp)
    }

    if (nchar(t1)<=1){
      if (e1==e2){
        result = c(0, cumulActions,r)
      }else{
        cumulActions = cumulActions + 1
        new_row = c("Substitution", e1, e2, 2, e1p, e1p)
        r = rbind(r,new_row)
        result = c(m_cost [which (order == e1),which (order == e2)], cumulActions,r)
      }
    }else{
      if (e1==e2){
        result = parDistV3(s1,s2,m_cost , order, transCost, max, costSoFar, cumulActions,s1p,s2p)
      }else{
        if(e1!=" " & e2!=" "){
          #Reason why line below is commented out:
          #At this stage if there's still such substitution,
          #There must have been a transposition already.
          #cumulActions = cumulActions + 1

          new_row = c("Substitution", e1, e2, 2, e1p, e1p)
          r = rbind(r,new_row)

          opCost = m_cost [which (order == e1),which (order == e2)]
          partialResult = parDistV3(s1,s2,m_cost , order, transCost, max, costSoFar + opCost, cumulActions,s1p,s2p)
          if(!any(is.na(partialResult))){
            result =c(opCost+partialResult[[1]], partialResult[[2]], r)
          } else result = NA

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

          p1_p1 = p1[1: (matches[1] - 1)]
          p1_p2 = p1[matches[1] + 1: nchar(t1)]
          p2_p1 = p2[1: (matches[1] - 1)]
          p2_p2 = p2[matches[1] + 1: nchar(t2)]

          part1=parDistV3(t1_p1,t2_p1,m_cost , order, transCost, max, costSoFar + opCost, cumulActions,p1_p1,p2_p1)
          part2=parDistV3(t1_p2,t2_p2,m_cost , order, transCost, max, costSoFar + opCost, cumulActions,p1_p2,p2_p2)

          r= rbind(r, part1[3], part2[3])

          print(t1_list)
          print(t2_list)
          print(part1)

          if(!any(is.na(part1)) &
             !any(is.na(part2))){
            result =c(opCost+part1[1]+part2[1],
                    part1[2]+part2[2]-cumulActions,
                    r) #Because we're adding tgt two operations, there will be an extra copy of cumulActions, which we delete here
          } else result = NA
        } else {
          if (!is.na(t1_first_nonsp) & t1_first_nonsp > 1){ #Moving the first char of t2 back or substituting
            #Old condition:  (substring(t2,1,1)!=" " & substring(t2,1,1)!=" ")
            cumulActions = cumulActions + 1

            new_row_1 = c("Substitution", e1, e2, 2, e1p, e1p)
            r= rbind(r,new_row_1)
            partialresult_1=parDistV3(s1,s2,m_cost ,order, transCost, max,
                                     costSoFar + m_cost [which (order == e1),which (order == e2)], cumulActions,s1p,s2p)
            if(!any(is.na(partialresult_1))){
              option1 = c(m_cost [which (order == e1),which (order == e2)]+partialresult_1[[1]],partialresult_1[[2]],r)
            } else option1 = NA

            if (e1==" ") {temp=e2} else {temp=e1}
            new_row_2 = c("Transposition", e1, e2, 2, temp, temp)
            r= rbind(r,new_row_2)
            partialresult_2= parDistV3(t1, tback,m_cost , order, transCost,
                                      max = min(max, costSoFar + option1[[1]], na.rm = T),
                                      costSoFar + transCost[which (order == t1_list[t1_first_nonsp])] * (t1_first_nonsp - 1),
                                      cumulActions,p1, pback)
            if(!any(is.na(partialresult_2))){
              option2 = c(transCost[which (order == t1_list[t1_first_nonsp])] * (t1_first_nonsp - 1) + partialresult_2[[1]],
                        partialresult_2[[2]],r)
            } else option2 = NA


            if(any(is.na(option1)) & any(is.na(option2))){
              result = c(Inf, cumulActions,r)
            } else if (any(is.na(option1)) | ((option2[[1]] < option1[[1]]) %>% replace_na(F))){
              result = option2
            } else {
              result = option1
            }
          } else if(!is.na(t2_first_nonsp) & t2_first_nonsp > 1) { #Moving the first nonspace char of t2 front or substituting
            cumulActions = cumulActions + 1

            new_row_1 = c("Substitution", e1, e2, 2, e1p, e1p)
            r= rbind(r,new_row_1)
            partialresult_1 =parDistV3(s1,s2,m_cost ,order, transCost,max,
                                      costSoFar + m_cost [which (order == e1),which (order == e2)], cumulActions,s1p,s2p)
            if(!any(is.na(partialresult_1))){
              option1 = c(m_cost [which (order == e1),which (order == e2)]+partialresult_1[[1]], partialresult_1[[2]],r)
            } else option1 = NA

            if (e1==" ") {temp=e2} else {temp=e1}
            new_row_2 = c("Transposition", e1, e2,2,temp,temp)
            r= rbind(r,new_row_2)
            partialresult_2 = parDistV3(t1,tfor,m_cost ,order, transCost,
                                      max = min(max, costSoFar + option1[[1]], na.rm = T),
                                      costSoFar + transCost[which(order == t2_list[t2_first_nonsp])] * (t2_first_nonsp - 1),
                                      cumulActions,p1,pfor)
            if(!any(is.na(partialresult_2))){
              option2 = c(transCost[which (order == t2_list[t2_first_nonsp])] * (t2_first_nonsp - 1)+partialresult_2[[1]],
                        partialresult_2[[2]],r)
            } else option2 = NA

            if(any(is.na(option1)) & any(is.na(option2))){
              result = c(Inf, cumulActions,r)
            } else if (any(is.na(option1)) | ((option2[[1]] < option1[[1]]) %>% replace_na(F))){
              result = option2
            } else {
              result = option1
            }
          } else {
            cumulActions = cumulActions + 1
            new_row_1 = c("Substitution", e1, e2,e1p,e1p)
            r= rbind(r,new_row_1)
            opCost = m_cost [which (order == e1),which (order == e2)]
            partialResult= parDistV3(s1,s2,m_cost , order, transCost, max, costSoFar + opCost,
                     cumulActions,p1,p2)
            if(!any(is.na(partialResult))){
              result =(c(opCost+partialResult[[1]], partialResult[[2]],r))
            } else result = NA
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
calCostV2 <- function(l1,l2,m_sim=matrix(data =c(1,0,0,0,0,0,0,
                                              0,1,0,0,0,0,0,
                                              0,0,1,0,0,0,0,
                                              0,0,0,1,0,0,0,
                                              0,0,0,0,1,0,0,
                                              0,0,0,0,0,1,0,
                                              0,0,0,0,0,0,1), nrow=7),order,transCost){


  record = data.frame(type = character(0),
                      e1 = character(0),
                      e2 = character(0),
                      pass = character(0),
                      pos1 = character(0),
                      pos2 = character(0))
  subCost=1  # pre-set substitution cost
  # transCost=0.5  # pre-set transition cost of one space
  m_cost =1-m_sim #Similarities to differences

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
  fullMatches = character(0)
  for (s in seq(1,length(l1))){  # for each speaker
    pos1=seq(nchar(l1[[s]]))
    pos2=seq(nchar(l2[[s]]))

    currBlist1 = l1[[s]]
    currBlist2 = l2[[s]]

    currBlistList1 = sapply(1:nchar(currBlist1), function(x) substring(currBlist1, x, x))
    currBlistList2 = sapply(1:nchar(currBlist2), function(x) substring(currBlist2, x, x))
    #First do all the straightforward substitutions
    posMatch = sapply(1:nchar(currBlist1), function(x) substring(currBlist1, x, x) != " " & substring(currBlist2, x, x) != " ")
    substPos = posMatch & sapply(1:nchar(currBlist1), function(x) substring(currBlist1, x, x) != substring(currBlist2, x, x))
    fullMatches = c(currBlistList1[currBlistList1 == currBlistList2 & currBlistList1 != " "], fullMatches)

    if(length(which(substPos)) > 0){
      cost = cost + sapply(which(substPos), function(x) m_cost [which(currBlistList1[x] == order), which(currBlistList2[x] == order)]) %>% sum

      for (pos in which(substPos)){
        new_row = c("Substitution", substring(currBlist1, pos, pos), substring(currBlist2, pos, pos), 1, pos, pos)
        record = rbind(record,new_row)
      }

    }
    actions = actions + sum(posMatch)

    #Then get the areas between the places where both annotators put a boundary
    #transAreas = transitional areas between two places where both annotators
    #put a boundary
    transAreas1 = list()
    transAreas2 = list()

    transAreasPos1 = list()
    transAreasPos2 = list()
    if(posMatch[1]) currItem = 0 else {
      currItem = 1
      transAreas1[[currItem]] = character(0)
      transAreas2[[currItem]] = character(0)

      transAreasPos1[[currItem]] = character(0)
      transAreasPos2[[currItem]] = character(0)
    }
    for(i in 1:length(posMatch)){
      if(posMatch[i]){
        currItem = currItem + 1
        transAreas1[[currItem]] = character(0)
        transAreas2[[currItem]] = character(0)

        transAreasPos1[[currItem]] = character(0)
        transAreasPos2[[currItem]] = character(0)
      } else {
        transAreas1[[currItem]] = c(transAreas1[[currItem]], substring(currBlist1, i, i))
        transAreas2[[currItem]] = c(transAreas2[[currItem]], substring(currBlist2, i, i))

        transAreasPos1[[currItem]] = c(transAreasPos1[[currItem]], pos1[i])
        transAreasPos2[[currItem]] = c(transAreasPos2[[currItem]], pos2[i])
      }
    }

    spaceOnly = sapply(1:length(transAreas1), function(x) all(c(transAreas1[[x]], transAreas2[[x]]) == " "))
    transAreas1 = transAreas1[!spaceOnly]
    transAreas2 = transAreas2[!spaceOnly]

    transAreasPos1 = transAreasPos1[!spaceOnly]
    transAreasPos2 = transAreasPos2[!spaceOnly]

    colnames(record) = c("type", "e1", "e2", "pass", "pos1","pos2")

    #Now go through each of those non-matching transitional areas
    #And call parDist
    if(length(transAreas1) > 0){
      t1s = sapply(transAreas1, paste0, collapse = "")
      t2s = sapply(transAreas2, paste0, collapse = "")

      p1s = transAreasPos1
      p2s = transAreasPos2


      for(x in 1:length(t1s)){
        t1 = t1s[x]
        t2 = t2s[x]

        p1 = p1s[[x]]
        p2 = p2s[[x]]
        print(paste0("Speaker ", s, "/", length(l1), ", Segment ", x, "/", length(t1s), " - t1:", t1, "|t2:", t2))

        t1_trailing_sp = str_extract_last(t1, " +") %>% nchar %>% replace_na(0)
        t2_trailing_sp = str_extract_last(t2, " +") %>% nchar %>% replace_na(0)
        trailing_sp = min(t1_trailing_sp, t2_trailing_sp)

        t1_leading_sp = str_extract_first(t1, " +") %>% nchar %>% replace_na(0)
        t2_leading_sp = str_extract_first(t2, " +") %>% nchar %>% replace_na(0)
        leading_sp = min(t1_leading_sp, t2_leading_sp)

        t1 = substring(t1, 1 + leading_sp, nchar(t1) - trailing_sp)
        t2 = substring(t2, 1 + leading_sp, nchar(t2) - trailing_sp)

        p1 = p1[(1 + leading_sp): (length(p1) - trailing_sp)]
        p2 = p2[(1 + leading_sp): (length(p2) - trailing_sp)]

        t1_list = strsplit(t1, "")[[1]]
        t2_list = strsplit(t2, "")[[1]]

        p1_list = p1
        p2_list = p2

        if(all(t1_list == " ")){ #No need to consider transposing\
          for(i in which(t2_list != " ")){
            cost = cost + m_cost [which(order == " "), which(order == t2_list[i])]
            actions = actions + 1

            new_row = c("Substitution", " ", t2_list[i], 2,p1_list[i],p2_list[i])
            record = rbind(record,new_row)
          }
        } else if(all(t2_list == " ")){
          for(i in which(t1_list != " ")){
            cost = cost + m_cost [which(order == t1_list[i]), which(order == " ")]
            actions = actions + 1

            new_row = c("Substitution",t1_list[i], " ", 2,p1_list[i],p2_list[i])
            record = rbind(record,new_row)
          }
        } else {
          parDistResult = parDistV3(t1,t2,m_cost ,order,transCost,p1 = p1,p2 = p2)
          cost = cost + parDistResult[[1]]
          actions = actions + parDistResult[[2]]  # result of cost and record between two fixed boundaries

          segRecord = cbind(data.frame(parDistResult[3:7]), 2)
          colnames(segRecord) = c("type", "e1", "e2", "pass","pos1","pos2")
          record = rbind(record,segRecord)
        }
      }
    }
  }

  colnames(record) = c("type", "e1", "e2", "pass","pos1","pos2")

  record = record %>% mutate(postTranspose = (pass == 2 & type == "Substitution" & e1 != " " & e2 != " ")) %>%
    select(-pass)

  return(list(cost = cost, actions = actions, record = record, fullMatches = fullMatches))
}  # input 2 genBd


