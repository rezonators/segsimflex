

library(rezonateR)
library(rjson)


#' Inter-annotator agreement
#'
#' @param inPath The string of the path of the .rez file
#' @param outPath The string of the path of the output csv file
#'
#' @return A data frame that can be used for similarity score and inter-annotated agreement
#' @export
#'
#'
rezTrans = function(inPath){
  split = importRez(inPath, concatFields = "Utterance")
  temp=split$unitDF
  data.frame(Turn = temp$TurnSeq,Speaker = temp$Speaker,Utterance =temp$Utterance)
}

