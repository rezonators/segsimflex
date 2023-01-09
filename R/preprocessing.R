library(data.table)
library(readr)
library(reshape)
library(tidyverse)
library(dplyr)

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
