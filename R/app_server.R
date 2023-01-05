#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # global variable
  ## sdData: sample-feature data
  ##(1) data: raw data
  ##(2) group: metadata
  ##(3) filter: QC filtered data
  ##(4) filterNormTransform: filtered, normalized and transformed data
  ##(5) clean: QC filtered, normalized, transformed and scaled data
  global <- reactiveValues(
    data = NULL,
    group = NULL,
    filter = NULL,
    filterNormTransform = NULL,
    clean = NULL
    )
  # Your application server logic
  mod_01_home_server("01_home_1")
  uploadData <- mod_02_uploadData_server("02_uploadData_1", sfData = global)
  mod_03_preprocess_server("03_preprocess_1", sfData = global)
  viewResult <- mod_04_viewResult_server("04_viewResult_1", sfData = global)
  mod_05_downloadReport_server("05_downloadReport_1", sfData = global, inputData = uploadData, resultList = viewResult)
}
