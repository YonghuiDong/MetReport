#' @title Get meta data
#' @description Get metadata from input raw data.
#' @return The return value, if any, from executing the function.
#' @param DF a dataframe
#' @importFrom utils read.table
#' @importFrom dplyr select
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
#'getMeta(DF)


getMeta <- function(DF) {
  #(1) Suppress no visible binding for global variable
  ID <- NULL

  #(2) Function
  rowNameDF <- DF %>%
    dplyr::select(-ID) %>%
    t() %>%
    rownames()
  metaData <- read.table(text = rowNameDF, sep = "_", colClasses = "character")
  colnames(metaData) <- sub("V", "Group", colnames(metaData))
  metaTable <- cbind.data.frame(Sample = rowNameDF, metaData) %>%
    dplyr::select(-dplyr::last_col())
  return(metaTable)
}
