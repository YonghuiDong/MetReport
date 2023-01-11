#' @title Change raw data format
#' @description Change raw data format for subsequent data analysis according to user selected format types. Only Unique ID and Peak Area information are kept.
#' @param DF raw data
#' @param metaGroup meta group information
#' @param format data format, Compound Discoverer and other.
#' @importFrom dplyr mutate select rename_with starts_with %>%
#' @return The return value, if any, from executing the function.
#' @noRd
#' @examples
#' df1 <- data.frame(
#' Name <- c("x", "y", "z"),
#' `Area: WT_M_1` = 1:3,
#' `Area: WT_M_2` = 1:3,
#' `Area: WT_M_3` = 1:3,
#' `Area: MU_F_4` = 1:3,
#' `Area: MU_F_5` = 1:3,
#' `Area: MU_F_6` = 1:3,
#' check.names = FALSE
#' )
#' df1_format <- formatData(df1)
#' metaGroup <- data.frame(
#' A = c("P", "P", "P", "S", "S", "S"),
#' B = c("M", "M", "M", "F", "F", "F")
#' )
#' df2_format <- formatData(df1, metaGroup)

formatData <- function(DF, metaGrpup = NULL, format = "CD") {
  #(1) Suppress no visible binding for global variable notes
  Name <- NULL
  ID <- NULL
  . <- NULL

  #(2) Function
  ## CD processed files include "Area: ", which is a key word for peak area
  processedData <- switch(format,
                          "CD" = DF %>%
                            dplyr::mutate(ID = paste0("ID", row.names(.), "_", Name)) %>%
                            dplyr::select(ID, starts_with("Area:")) %>%
                            dplyr::rename_with(~ gsub("Area: ", "", .x, fixed = TRUE)) %>%
                            dplyr::rename_with(~ gsub(".raw.*", "", .x)),
                          "Other" = DF %>%
                            dplyr::mutate(ID = paste0("ID", row.names(.), "_", Name)) %>%
                            dplyr::select(ID, starts_with("Sample_")) %>%
                            dplyr::rename_with(~ gsub("Sample_", "", .x, fixed = TRUE))
                          )
  if(!is.null(metaGrpup)) {
    dfName <- do.call(paste, c(metaGrpup, sep = '_'))
    dfName <- paste0(dfName, "_", 1:length(dfName))
    if(length(dfName) != (dim(processedData)[2] - 1)) {stop("Sample number and meta number are not equal")}
    colnames(processedData) <- c("ID", dfName)
  }
  return(processedData)
  }
