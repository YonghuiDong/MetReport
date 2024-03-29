#' @title Sample normalization
#' @title perform normalization
#' @description perform normalization
#' @param DF a data frame, row is feature, and column is sample
#' @param Method normalization method: (1) LBME: linear baseline normalization based on mean values;
#' (2) LBMD: linear baseline normalization based on median values; (3) PQN: probabilistic quotient normalization;
#' (4) QT: quantile normalization; (5) None: without normalization; (6) IS: internal standards based calibration.
#' @param Inx row index of internal standards, only applys to IS normalization method.
#' @return normalized dataframe
#' @importFrom stats median
#' @importFrom dplyr slice summarise across everything
#' @export
#' @noRd
#' @examples
#' dat <- data.frame(S1 = c(1, 3, 4), S2 = c(1, 10, 3), S3 = c(2, 10, 20))
#' ret <- doNormalization(dat, Method = "PQN")

doNormalization <-function(x, Method = NULL, Inx = NULL){
  #(1) check input
  if(is.null(Method)) {stop("Please select a normalization Method")}
  if (!(toupper(Method) %in% c("NONE", "LBME", "LBMD", "PQN", "QT", "IS")))
  {stop("Invalid normalization Method")}

  #(2)perform normalization
  ##(2.1) linear baseline normalization based on mean values
  if(toupper(Method) == "LBME"){
    linear.baseline <- apply(x, 1, function(x) median(x, na.rm = T)) #compute baseline
    baseline.mean <- mean(linear.baseline)
    sample.means <- apply(x, 2, mean)
    linear.scaling <- baseline.mean/sample.means
    norm.metabo.data <- t(t(x)*linear.scaling)
  }

  ##(2.2) linear baseline normalization based on median values
  if (toupper(Method) == "LBMD"){
    linear.baseline <- apply(x, 1, function(x) median(x, na.rm = T)) #compute baseline
    baseline.median<-median(linear.baseline)
    sample.medians<-apply(x, 2, function(x) median(x, na.rm = T))
    linear.scaling<-baseline.median/sample.medians
    norm.metabo.data <- t(t(x)*linear.scaling)
  }

  #(2.3) PQN
  if(toupper(Method) == "PQN"){
    reference <- apply(x, 1, function(x) median(x, na.rm = T))
    quotient <- x/reference
    quotient.median <-apply(quotient, 2, function(x) median(x, na.rm = T))
    norm.metabo.data <-t(t(x)/quotient.median)
  }

  #(2.4) QT
  if(toupper(Method) == "QT"){
    df_rank <- apply(x, 2, rank, ties.method = "min")
    df_sorted <- data.frame(apply(x, 2, sort))
    df_mean <- apply(df_sorted, 1, mean)
    index_to_mean <- function(my_index, my_mean){
      return(my_mean[my_index])
    }
    norm.metabo.data <- apply(df_rank, 2, index_to_mean, my_mean = df_mean)
    rownames(norm.metabo.data) <- rownames(x)
  }

  #(2.5) No normalization
  if(toupper(Method) == "NONE"){
    norm.metabo.data = x
    rownames(norm.metabo.data) <- rownames(x)
  }

  #(2.6) IS-based normalization
  if(toupper(Method) == "IS"){
    # sum IS
    SumIS <- colMeans(x[Inx, ])

    # if contain 0, data frame will not get normalized
    if(isTRUE(any(SumIS == 0))){norm.metabo.data <- x}

    norm.metabo.data <- mapply('/', x, SumIS)
    # add random noises to the single IS, otherwise the complete row of the IS is 1, which will affect multivariate analysis
    if(length(Inx) == 1){norm.metabo.data[Inx, ] = norm.metabo.data[Inx, ] + 1e-3 * runif(ncol(norm.metabo.data))}
    rownames(norm.metabo.data) <- rownames(x)
  }

  return(norm.metabo.data)
}
