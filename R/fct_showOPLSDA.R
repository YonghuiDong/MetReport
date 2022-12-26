#' @title Show PLSDA score plot
#' @description perform PLSDA and show the score plot
#' @author Yonghui Dong
#' @param oplsda oplsda result from r package ropls
#' @param Group sample group information
#' @param size point size
#' @param interactive should the plot be interactive? default is FALSE
#' @importFrom ggplot2 aes
#' @noRd
#' @export
#' @examples
#' library(ropls)
#' data(sacurine)
#' genderFc <- sacurine$sampleMetadata[, "gender"]
#' oplsda <- opls(sacurine$dataMatrix, genderFc, predI = 1, orthoI = NA, fig.pdfC = "none")
#' showOPLSDA(oplsda, Group = genderFc, interactive = T)

showOPLSDA <- function(oplsda, Group, size = 3, interactive = FALSE){
  #(1) Suppress no visible binding for global variable notes
  p1 <- ot1 <- NULL

  #(2) check input
  Group <- as.factor(Group)
  if(class(oplsda)[1] != "opls") {stop("Wrong oplsda object detected")}
  if(length(levels(Group)) != 2){stop("Two sample groups are needed for OPLSDA analysis")}

  #(3) plot
  df <- data.frame(t1 = oplsda@scoreMN,  ot1 = oplsda@orthoScoreMN[, 1], Group = Group)
  p <- ggplot2::ggplot(df, aes(x = p1, y = ot1, color = Group, text = row.names(df))) +
    ggplot2::stat_ellipse(aes(fill = Group, group = Group), geom = "polygon", level = 0.95, alpha = 0.1, show.legend = F) +
    ggplot2::geom_point(size = size) +
    ggplot2::labs(caption = paste0("R2X    R2Y    Q2    pR2Y    pQ2\n",
                                   oplsda@summaryDF$`R2X(cum)`, "    ",
                                   oplsda@summaryDF$`R2Y(cum)`, "    ",
                                   oplsda@summaryDF$`Q2(cum)`, "    ",
                                   oplsda@summaryDF$pR2Y, "    ",
                                   oplsda@summaryDF$pQ2)
                  ) +
    ggplot2::xlab(paste0("T score[1] (", oplsda@modelDF$R2X[1]*100, "%)")
                  )+
    ggplot2::ylab("Orthogonal T score[1]") +
    ggplot2::ggtitle(paste("Sample groups:", levels(Group)[1], "vs", levels(Group)[2], sep = " ")
                     ) +
    ggplot2::theme_bw()
  if(interactive){
    p <- plotly::ggplotly(p) %>%
      plotly::layout(annotations =
                       list(text = paste0("R2X    R2Y    Q2    pR2Y    pQ2\n",
                                          oplsda@summaryDF$`R2X(cum)`, "    ",
                                          oplsda@summaryDF$`R2Y(cum)`, "    ",
                                          oplsda@summaryDF$`Q2(cum)`, "    ",
                                          oplsda@summaryDF$pR2Y, "    ",
                                          oplsda@summaryDF$pQ2),
                            showarrow = F,
                            xref = 'paper', x = 0,
                            yref = 'paper', y = 0,
                            bgcolor = "#e8e8e8")

                     ) # these info is lost when convert to plotly object
    }
  return(p)
}
