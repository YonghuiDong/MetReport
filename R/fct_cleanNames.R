#' @title Cleans strings
#' @description Trim leading and tailing white spaces and replace middle white spaces with underscore
#' @param str strings
#' @return The return value, if any, from executing the function.
#' @noRd
#' @examples
#' str <- c("a and b", "  c and d    ", "ID_(o)")
#' str2 <- cleanNames(str)

cleanNames <- function(str) {
  gsub_trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  gsub_rep <- function(x) gsub("[[:punct:]]+", "_", x)
  gsub_trim2 <- function(x) gsub("^\\_+|\\_+$", "", x)
  tem1 <- gsub_trim(str) # trim leading and tailing white spaces
  tem2 <- make.names(tem1) # make syntactically valid names
  #tem3 <- gsub_rep(tem2) # replace "dots" with "underscore"
  tem4 <- gsub_trim2(tem2) # remove mostly tailing "underscores"
  return(tem4)
}
