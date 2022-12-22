#' @title Change raw data format
#' @description Change raw data format for subsequent data analysis according to user selected format types. Only Unique ID and Peak Area information are kept.
#' @param DF raw data
#' @param format data format, Compound Discoverer and other.
#' @importFrom dplyr mutate select rename_with starts_with
#' @return The return value, if any, from executing the function.
#' @noRd

formatData <- function(DF, format = "CD") {
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
  return(processedData)
  }
