#' @title Plot TIC
#' @description Plot peak area as box plot.
#' @param df dataframe, row is feature, and column is sample
#' @param Group sample group information
#' @importFrom ggplot2 aes element_blank
#' @importFrom tidyr pivot_longer
#' @return boxplot
#' @export
#' @noRd

showTIC <- function(df, Group = NULL){
  #(1) format data
  tem <- t(df[, !(names(df) %in% c("ID"))])
  tem <- cbind.data.frame(Group = Group, Sample = rownames(tem), tem)
  colnames(tem)[1] <- "Group"
  tem <- tidyr::pivot_longer(tem, cols = !c(Group, Sample), names_to = "name", values_to = "Area")

  #(2) ploy
  ggplot2::ggplot(tem, aes(x = Sample, y = Area, fill = Group)) +
    ggplot2::geom_boxplot(alpha = 0.7) +
    ggplot2::ylab("Peak Area") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_blank())
}
