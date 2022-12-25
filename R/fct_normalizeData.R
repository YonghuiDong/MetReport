#' @title Sample normalization
#' @description Sample normalization of a data frame.
#' @param DF a data frame, row is feature, and column is sample
#' @param Method dataframe normalization method. "None" is without normalization; "SUM" is normalized by sum; "Median" is normalized by median
#' @return a row normalized dataframe
#' @export
#' @noRd
#' @examples
#' dat <- data.frame(S1 = c(1, 3, 4), S2 = c(1, 10, 3), S3 = c(2, 10, 20))
#' ret <- normalizeData(dat, Method = "Sum")

normalizeData <- function(DF, Method = "None") {
  tem <- switch(Method,
                "None" = DF,
                "Sum" = apply(DF, 2, function(x) (100 * x/sum(x))),
                "Median" = apply(DF, 2, function(x) (100 * x/median(x)))
                )
  return(tem)
  }
