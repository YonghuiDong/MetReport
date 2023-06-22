#' @title Get meta data
#' @description Get metadata from processed data.
#' @param DF the processed data frame resulted from formatData function.
#' @return a data frame, which contains meta data information
#' @export
#' @noRd
#' @examples
#' library(magrittr)
#' library(data.table)
#' load("data/cancerCell.rda")
#' feature <- formatData(cancerCell)
#' meta <- getMeta(feature)

getMeta <- function(DF) {
  rowNameDF <- colnames(DF)
  rowNameDF <- rowNameDF[!rowNameDF %in% "ID"]
  metaData <- read.table(text = rowNameDF, sep = "_", colClasses = "character")
  colnames(metaData) <- sub("V", "Group", colnames(metaData))
  metaData <- subset(metaData, select = -ncol(metaData))
  rownames(metaData) <- rowNameDF
  return(metaData)
}
