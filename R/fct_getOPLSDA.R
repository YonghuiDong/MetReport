#' @title Calculate OPLSDA
#' @description calcualte OPLSDA
#' @param feature feature data frame.
#' @param group sample groups.
#' @return opls result.
#' @noRd
#' @examples
#' library(ropls)
#' data(sacurine)
#' attach(sacurine)
#' sacurine.plsda <- getOPLSDA(dataMatrix, sampleMetadata[, "gender"])

getOPLSDA <- function(Feature, Group){
  resultOPLSDA <- tryCatch({
    ropls::opls(x = Feature,
                y = Group,
                log10L = FALSE,
                scaleC = "none",
                predI = 1,
                permI = 20,
                orthoI = 1, # see bug #25
                crossvalI = min(length(Group), 7),
                fig.pdfC = "none",
                info.txtC = "none"
                )
  },
  # For the following error:
  # Error: No model was built because the first predictive component was already not significant;
  # Select a number of predictive components of 1 if you want the algorithm to compute a model despite this.
  error = function(e){
    ropls::opls(x = Feature,
                y = Group,
                log10L = FALSE,
                scaleC = "none",
                predI = 1,
                permI = 20,
                orthoI = 1,
                crossvalI = min(length(Group), 7),
                fig.pdfC = "none",
                info.txtC = "none"
                )
  })
  return(resultOPLSDA)
}


