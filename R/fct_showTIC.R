#' @title Plot TIC
#' @description Plot peak area as box plot.
#' @param df dataframe, row is feature, and column is sample
#' @param Group sample group information
#' @param method which method is used to make boxplot, default is "fast".
#' @importFrom ggplot2 aes element_blank
#' @importFrom tidyr pivot_longer
#' @return boxplot
#' @export
#' @noRd
#' @examples
#' df <- data.frame(
#' ID = c("A", "B", "C", "D", "E"),
#' p1 = c(99, 90, 86, 88, 95),
#' p2 = c(33, 28, 31, 39, 34),
#' p3 = c(30, 28, 24, 24, 28)
#' )
#' Group <- c("X", "Y", "Z")
#' showTIC(df, Group)

showTIC <- function(df, Group = NULL, method = "fast"){
  if(method == "fast") return(fastShowTIC(df, Group))
  showTIC(df, Group)
}

# (1) Normal TIC with outlier ==================================================
normalshowTIC <- function(df, Group = NULL){
  #(1) format data
  tem <- df[, !(names(df) %in% c("ID"))]
  tem <- t(as.matrix(tem))
  if(is.null(Group)){Group = rownames(tem)}
  tem <- cbind.data.frame(Group = Group, Sample = rownames(tem), tem)
  colnames(tem)[1] <- "Group"
  tem <- tidyr::pivot_longer(tem, cols = !c(Group, Sample), names_to = "name", values_to = "Area")

  #(2) plot
  ggplot2::ggplot(tem, aes(x = Sample, y = Area, fill = Group)) +
    ggplot2::geom_boxplot(alpha = 0.7) +
    ggplot2::ylab("Peak Area") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_blank())
}


## (2) fast TIC but without outlier ============================================
fastShowTIC <- function(df, Group = NULL){
  #(1) format data
  tem <- df[, !(names(df) %in% c("ID"))]
  tem <- t(as.matrix(tem))
  quantiles <- t(apply(tem, 1, quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)))
  if(is.null(Group)){Group = rownames(quantiles)}
  tem <- cbind.data.frame(Group = Group, Sample = rownames(quantiles), quantiles)
  colnames(tem)[1] <- "Group"

  #(2) plot
  ggplot2::ggplot(tem, aes(x = Sample, ymin = `5%`, lower =`25%`, middle = `50%`, upper = `75%`, ymax = `95%`, fill = Group)) +
    ggplot2::geom_boxplot(stat = "identity", alpha = 0.7) +
    ggplot2::ylab("Peak Area") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_blank())
}
