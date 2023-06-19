#' @title Delete columns
#' @description Delete columns
#' @param df a data frame
#' @param nameofthecolumn names of the columns to be deleted
#' @importFrom magrittr %>%
#' @return a data frame.
#' @noRd
#' @examples
#' df <- copy(mtcars)
#' cols <- c("cyl", "disp", "hp")
#' removecolumn(df, cols)

# data.table solution ----------------------------------------------------------
removecolumn <- function(df, nameofthecolumn){
  df <- data.table::setDT(df) %>%
  .[, -(nameofthecolumn), with = FALSE]
  return(df)
}

# dplyr solution ---------------------------------------------------------------
# removecolumn <- function(df, nameofthecolumn){
#   dplyr::select(df, -all_of(nameofthecolumn))
# }
