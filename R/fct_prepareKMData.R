#' @title Prepare data for k-means
#' @description repare data for k-means
#' @param DF the same data frame used for heatmap analysis. Row is sample, and column is features.
#' @param Group sample group information.
#' @return a data frame for k-means.
#' @noRd
#' @examples
#' library(dplyr)
#' library(tidyr)
#' heatmapDF <- read.csv("rawData/heatmapDF.csv", header = TRUE)
#' Group <- c(rep("Lung", 18), rep("Blood", 6), rep("QC", 6), rep("Blood", 6))
#' KMData <- prepareKMData(heatmapDF, Group)

prepareKMData <- function(DF, Group){
  #(1) Format data -------------------------------------------------------------
  tem <- DF %>%
    dplyr::mutate(Group = Group) %>%
    dplyr::filter(Group != "QC") %>%
    tidyr::pivot_longer(cols = !Group, names_to = "Metabolite", values_to = "Area") %>%
    dplyr::group_by(Group, Metabolite) %>%
    dplyr::summarize(meanArea = mean(Area), .groups = 'drop') %>%
    tidyr::pivot_wider(names_from = Group, values_from = meanArea)

  #(2) Standardize data --------------------------------------------------------
  RS <- rowSums(dplyr::select(tem, -Metabolite))
  tem <- tem %>%
    dplyr::mutate_if(is.numeric, function(x)(x/RS))
  return(tem)
}


