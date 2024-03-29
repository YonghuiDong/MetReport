#' @title Plot box plot.
#' @description Plot box plot.
#' @param DF the same data frame used for heatmap analysis. Row is sample, and column is features.
#' @param Group sample group information
#' @param Metabolite which metabolite to display?
#' @param colorPalette color palette.
#' @return The return value, if any, from executing the function.
#' @noRd
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#' heatmapDF <- read.csv("rawData/heatmapDF.csv", header = TRUE)
#' Group <- c(rep("Lung", 18), rep("Blood", 6), rep("QC", 6), rep("Blood", 6))
#' showBoxplot(DF = heatmapDF, Transform = "log2", Group = Group, Metabolite = "ID3_Creatine")


showBoxplot <- function(DF, Transform = "none", Group, Metabolite, colorPalette = "Default", BPPlotType = "Box plot"){
  #(1) Prepare plot ------------------------------------------------------------
  DF <- switch(Transform,
               "none" = DF,
               "log2" = log2(DF),
               "log10" = log10(DF)
               )
  yLegend <- switch(Transform,
                    "none" = "Peak Area",
                    "log2" = "Log2 Transformed Peak Area",
                    "log10" = "Log10 Transformed Peak Area"
                    )
  p <- DF %>%
    dplyr::mutate(Group = Group) %>%
    dplyr::select(Group, Metabolite = all_of(Metabolite)) %>%
    dplyr::filter(Group != "QC") %>%
    dplyr::group_by(Group) %>%
    dplyr::mutate(MEAN = mean(Metabolite), SD = sd(Metabolite)) %>%
    ggplot2::ggplot(aes(x = Group, fill = Group)) +
    ggplot2::ylab(yLegend) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(paste0("Metabolite: ", Metabolite)) +
    ggplot2::theme(text = element_text(size = 16), legend.position="none")

  #(2) Choose color palette ----------------------------------------------------
  if(colorPalette == "Default") {
    p <- p
  } else {
    p <- p + scale_fill_brewer(palette = colorPalette)
  }

  #(3) Choose plot type --------------------------------------------------------
  p2 <- switch(BPPlotType,
               "Box plot" = p +
                 ggplot2::geom_boxplot(aes(y = Metabolite), outlier.shape = 24, outlier.fill = "red", outlier.size = 3, alpha = 0.8) +
                 ggplot2::geom_jitter(aes(y = Metabolite), shape = 16, position = position_jitter(0.2), color = "black"),
               "Violin plot" = p +
                 ggplot2::geom_violin(aes(y = Metabolite), alpha = 0.8) +
                 ggplot2::geom_jitter(aes(y = Metabolite), shape = 16, position = position_jitter(0.2), color = "black"),
               "Bar plot" = p +
                 ggplot2::geom_bar(aes(y = MEAN), alpha = 0.8, stat = "identity", width = 0.5, color = "black", position = position_dodge()) +
                 ggplot2::geom_errorbar(aes(ymin = MEAN, ymax = MEAN + SD), width = 0.2, position = position_dodge(0.9)) +
                 ggplot2::geom_jitter(aes(y = Metabolite), shape = 16, position = position_jitter(0.2), color = "black")
               )
  return(p2)
}
