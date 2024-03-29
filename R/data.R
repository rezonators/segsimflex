#' Santa Barbara Corpus prediction data
#'
#' A
#'
#' @format ## `who`
#' A list of two items:
#' \describe{
#'   \item{new}{Model predictions about IU boundaries.}
#'   \item{old}{Original IU boundaries.}
#' }
#'
#' Each item is a list containing 60 dataframes. Each dataframe has the following content:
#' A list of two items:
#' \describe{
#'   \item{new}{The document ID.}
#'   \item{old}{The participant label.}
#'   \item{unitId / newUID}{The unit ID.}
#'   \item{Utterance}{The text of that unit.}
#' }
"sbc_iubound_prediction"




#' IU segmentations of squared-numbered texts in the NCCU Taiwan Mandarin Corpus
#'
#' @format ## `nccu_squareno`
#' A list of 7 items, each of which contains two items (annotations from each transcriber).
"nccu_squareno"
