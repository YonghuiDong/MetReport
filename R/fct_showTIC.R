#' @title Plot TIC
#' @description Plot TIC as bar plot.
#' @param df dataframe, row is feature, and column is sample
#' @param Group sample group information
#' @importFrom ggplot2 aes element_text
#' @return The return value, if any, from executing the function.
#' @export
#' @noRd

showTIC <- function(df, Group = NULL){
  #(1) format data
  tem <- df[, !(names(df) %in% c("ID"))]
  tem <- as.data.frame(colSums(tem), col.name = "TIC")
  tem$Sample <- rownames(tem)
  tem$Groups <- Group
  colnames(tem)[1] <- "TIC"
  colnames(tem)[3] <- "Groups"
  #(2) ploy
  ggplot2::ggplot(tem, aes(x = Sample, y = TIC, fill = Groups)) +
    ggplot2::geom_bar(stat = "identity",
                      color="black",
                      size=0.1,
                      alpha = 0.7) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
