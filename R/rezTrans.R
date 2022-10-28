library(rezonateR)
library(rjson)
yujie_split = rez_load("yujie_split.Rdata")
temp=yujie_split$unitDF
result=data.frame(Turn = temp$TurnSeq,Speaker = temp$Speaker,Utterance =temp$Utterance)

write.csv(result,"yujie_split.csv")
