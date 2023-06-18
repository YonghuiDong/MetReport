#' #' @title Change raw data format
#' #' @description Change raw data format for subsequent data analysis according to user selected format types. Only Unique ID and Peak Area information are kept.
#' #' @param DF raw data
#' #' @param metaGroup meta group information
#' #' @param format data format, Compound Discoverer and other.
#' #' @importFrom dplyr mutate select rename_with starts_with %>%
#' #' @return The return value, if any, from executing the function.
#' #' @noRd
#' #' @examples
#' #' ## Method 1: peak area and metadata are in the same table
#' #' df1 <- data.frame(
#' #' Name = c("x", "y", "z"),
#' #' `Area: Sample_WT_M_1` = 1:3,
#' #' `Area: Sample_WT_M_2` = 1:3,
#' #' `Area: Sample_WT_M_3` = 1:3,
#' #' `Area: Sample_MU_F_4` = 1:3,
#' #' `Area: Sample_MU_F_5` = 1:3,
#' #' `Area: Sample_MU_F_6` = 1:3,
#' #'  check.names = FALSE
#' #' )
#' #' df1_format <- formatData(df1)
#' #'
#' #' ## Method2: feature table and metadata table are separated
#' #' df2 <- data.frame(
#' #' `Sample1` = 1:3,
#' #' `Sample2` = 1:3,
#' #' `Sample3` = 1:3,
#' #' `Sample4` = 1:3,
#' #' `Sample5` = 1:3,
#' #' `Sample6` = 1:3,
#' #' check.names = FALSE
#' #' )
#' #' metaGroup <- data.frame(
#' #' Sample = c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5", "Sample6"),
#' #' A = c("P", "P", "P", "S", "S", "S"),
#' #' B = c("M", "M", "M", "F", "F", "F")
#' #' )
#' #' df2_format <- formatData(df2, metaGroup = metaGroup, format = "Other")
#'
#' formatData1 <- function(DF, metaGroup = NULL, format = "CD"){
#'   ## CD processed files include "Area: ", which is a key word for peak area
#'   if (!("Name" %in% colnames(DF))){DF["Name"] <- "Unknown"} # add a Name column
#'   processedData <- switch(format,
#'                           "CD" = DF %>%
#'                             dplyr::mutate(ID = paste0("ID", row.names(.), "_", Name)) %>%
#'                             dplyr::select(ID, starts_with("Area:")) %>%
#'                             dplyr::rename_with(~ gsub("Area: ", "", .x, fixed = TRUE)) %>%
#'                             dplyr::rename_with(~ gsub("Sample_", "", .x, fixed = TRUE)) %>%
#'                             dplyr::rename_with(~ gsub(".raw.*", "", .x)),
#'                           "Other" = DF %>%
#'                             dplyr::mutate(ID = paste0("ID", row.names(.), "_", Name)) %>%
#'                             dplyr::select(ID, starts_with("Sample")) %>%
#'                             dplyr::rename_with(~ gsub("Sample_", "", .x, fixed = TRUE))
#'                           )
#'   if(!is.null(metaGroup)){
#'     metaGroup <- metaGroup[, -1, drop = FALSE]
#'     dfName <- do.call(paste, c(metaGroup, sep = '_'))
#'     dfName <- paste0(dfName, "_", 1:length(dfName))
#'     if(length(dfName) != (dim(processedData)[2] - 1)) {stop("Sample number and meta number are not equal")}
#'     colnames(processedData) <- c("ID", dfName)
#'   }
#'   return(processedData)
#' }


#' @title Change raw data format
#' @description Change raw data format for subsequent data analysis according to user selected format types.
#' Only Unique ID and Peak Area information are kept.
#' @param DF raw data
#' @param metaGroup meta group information
#' @param format data format, Compound Discoverer and other.
#' @import data.table
#' @import magrittr
#' @return The return value, if any, from executing the function.
#' @noRd
#' @examples
#' ## Method 1: peak area and metadata are in the same table
#' df1 <- data.frame(
#' Name = c("x", "y", "z"),
#' `Area: Sample_WT_M_1` = 1:3,
#' `Area: Sample_WT_M_2` = 1:3,
#' `Area: Sample_WT_M_3` = 1:3,
#' `Area: Sample_MU_F_4` = 1:3,
#' `Area: Sample_MU_F_5` = 1:3,
#' `Area: Sample_MU_F_6` = 1:3,
#'  check.names = FALSE
#' )
#' df1_format <- formatData(df1)
#'
#' ## Method2: feature table and metadata table are separated
#' df2 <- data.frame(
#' Sample_1 = 1:3,
#' Sample_2 = 1:3,
#' Sample_3 = 1:3,
#' Sample_4 = 1:3,
#' Sample_5 = 1:3,
#' Sample_6 = 1:3,
#' check.names = FALSE
#' )
#' metaGroup <- data.frame(
#' Sample = c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5", "Sample6"),
#' A = c("P", "P", "P", "S", "S", "S"),
#' B = c("M", "M", "M", "F", "F", "F")
#' )
#' df2_format <- formatData(df2, metaGroup = metaGroup, format = "Other")


formatData2 <- function(DF, metaGroup = NULL, format = "CD"){
  #(1) Change DF to datatable --------------------------------------------------
  DF <- data.table::as.data.table(DF)
  if (!("Name" %in% colnames(DF))) {DF[, Name := "Unknown"]}

  #(2) Format feature data -----------------------------------------------------
  ## CD processed files include "Area: "; Other processed files include "Sample", which is a key word for identifying peak area columns.
  processedData <- switch(format,
                          "CD" = DF %>%
                            .[, ID := paste0("ID", seq_len(.N), "_", Name)] %>%
                            data.table::setcolorder(., neworder = "ID") %>%
                            .[, c("ID", grep("Area:", names(.), value = TRUE)), with = FALSE] %>%
                            data.table::setnames(old = names(.), new = sub("Area: ", "", names(.), ignore.case = TRUE)) %>%
                            data.table::setnames(old = names(.), new = sub("Sample_", "", names(.), ignore.case = TRUE)) %>%
                            data.table::setnames(old = names(.), new = sub(".raw.*", "", names(.), ignore.case = TRUE)),
                          "Other" = DF %>%
                            .[, ID := paste0("ID", seq_len(.N), "_", Name)] %>%
                            data.table::setcolorder(., neworder = "ID") %>%
                            .[, c("ID", grep("Sample_", names(.), value = TRUE)), with = FALSE] %>%
                            data.table::setnames(old = names(.), new = sub("Sample_", "", names(.), ignore.case = TRUE))
                          )

  #(3) Extract meta data -------------------------------------------------------
  if(!is.null(metaGroup)){
    metaGroup <- data.table::setDT(metaGroup) %>%
      .[, 1 := NULL]
    dfName <- do.call(paste, c(metaGroup, sep = '_'))
    dfName <- paste0(dfName, "_", 1:length(dfName))
    if (length(dfName) != (ncol(processedData) - 1)) {stop("Sample number and meta number are not equal")}
    setnames(processedData, c("ID", dfName))
  }
  return(processedData)
}


