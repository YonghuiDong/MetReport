#' @title k-Nearest Neighbour Imputation
#' @description k-Nearest Neighbour imputation using VIM package
#' @param x dataframe, row is feature, column is sample
#' @importFrom VIM kNN
#' @return a dataframe will filled missing value
#' @export
#' @noRd
#' @examples
#' df <- cbind.data.frame(S1 = c(14, 3), S2 = c(15, NA), S3 = c(NA, 5))
#' rownames(df) <- c("M1", "M2")
#' df_fill <- kNNMissing(df)

kNNMissing <- function(x) {
  df <- VIM::kNN(t(x), imp_var = F)
  rownames(df) <- colnames(x)
  df2 <- t(df)
  df2 <- as.data.frame(df2)
  rownames(df2) <- NULL
  return(df2)
}
