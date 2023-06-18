#' @title Get meta data
#' @description Get metadata from processed data.
#' @param DF the processed data frame resulted from formatData function.
#' @return a data frame, which contains meta data information
#' @export
#' @noRd
#' @examples
#'DF <- cbind.data.frame(ID = c("malic acid", "citric acid", "glucose"),
#'                       WT_Male_T1_1 = c(1, 2, 3),
#'                       WT_Male_T1_2 = c(1, 2, 3),
#'                       WT_Male_T1_3 = c(1, 2, 3),
#'                       WT_Male_T2_1 = c(1, 2, 3),
#'                       WT_Male_T2_2 = c(1, 2, 3),
#'                       WT_Male_T2_3 = c(1, 2, 3),
#'                       WT_Female_T1_1 = c(1, 2, 3),
#'                       WT_Female_T1_2 = c(1, 2, 3),
#'                       WT_Female_T1_3 = c(1, 2, 3),
#'                       WT_Female_T2_1 = c(1, 2, 3),
#'                       WT_Female_T2_2 = c(1, 2, 3),
#'                       WT_Female_T2_3 = c(1, 2, 3),
#'                       Mu_Male_T1_1 = c(1, 2, 3),
#'                       Mu_Male_T1_2 = c(1, 2, 3),
#'                       Mu_Male_T1_3 = c(1, 2, 3),
#'                       Mu_Male_T2_1 = c(1, 2, 3),
#'                       Mu_Male_T2_2 = c(1, 2, 3),
#'                       Mu_Male_T2_3 = c(1, 2, 3),
#'                       Mu_Female_T1_1 = c(1, 2, 3),
#'                       Mu_Female_T1_2 = c(1, 2, 3),
#'                       Mu_Female_T1_3 = c(1, 2, 3),
#'                       Mu_Female_T2_1 = c(1, 2, 3),
#'                       Mu_Female_T2_2 = c(1, 2, 3),
#'                       Mu_Female_T2_3 = c(1, 2, 3)
#'                       )
#'getMeta1(DF)

getMeta <- function(DF) {
  rowNameDF <- colnames(DF)
  rowNameDF <- rowNameDF[!rowNameDF %in% "ID"]
  metaData <- read.table(text = rowNameDF, sep = "_", colClasses = "character")
  colnames(metaData) <- sub("V", "Group", colnames(metaData))
  metaTable <- cbind.data.frame(Sample = rowNameDF, metaData)
  metaTable <- metaTable[, -ncol(metaTable)]
  return(metaTable)
}
