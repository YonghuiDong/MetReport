#' 05_downloadReport UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_05_downloadReport_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      #1. User Guide ===========================================================
      column(width = 12,
             box(
               width = 12,
               title = strong("User Guide"),
               status = "warning",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               p("1. You can download the HTML report in this panel. Don't repeatedly click the download button. It will make the process even slower."),
               p("2. You may need to restart your PC if the report generation is stuck.")
               )
             ),

      #2. Materials & Methods ==================================================
      column(width = 5,
             box(
               width = 12,
               inputId = "input_card",
               title = strong("Report Information Panel"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = FALSE,
               collapsed = FALSE,
               closable = FALSE,
               textInput(inputId = ns("projectName"),
                         label = "Project Name",
                         value = "",
                         placeholder = "(Optional) Name of the project"
                         ),
               textInput(inputId = ns("authorName"),
                         label = "Author Name",
                         value = "Metabolic Profiling Group, Weizmann Institute of Science",
                         placeholder = "(Optional) Contact name"
                         )
               ),
             box(
               width = 12,
               inputId = "input_card",
               title = strong("Materials & Methods Panel"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = FALSE,
               collapsed = FALSE,
               closable = FALSE,
               radioButtons(inputId = ns("showIntro"),
                            label = "1. Do you want to show introduction section in the report?",
                            choices = list("Yes" = TRUE, "No" = FALSE),
                            selected = TRUE
                            ),
               radioButtons(inputId = ns("showMethods"),
                            label = "2. Do you want to show materials and methods section in the report?",
                            choices = list("Yes" = TRUE, "No" = FALSE),
                            selected = FALSE
                            ),
               selectInput(inputId = ns("samplePrep"),
                           label = "2.1 Select Sample Preparation Method",
                           choices = list("None",
                                          "Method 1",
                                          "Method 2",
                                          "Method 3"),
                           selected = "None",
                           multiple = FALSE
                           ),
               textInput(inputId = ns("samplePrepManual"),
                         label = "(or) Use Another Sample Preparation Method",
                         value = "",
                         placeholder = "You can also paste your sample preparation method here"
                         ),
               selectInput(inputId = ns("instrument"),
                           label = "2.2 Select Sample Analysis Method",
                           choices = list("None",
                                          "Method 1",
                                          "Method 2",
                                          "Method 3"),
                           selected = "None",
                           multiple = FALSE
                           ),
               textInput(inputId = ns("instrumentManual"),
                         label = "(or) Use Another Sample Analysis Method",
                         value = "",
                         placeholder = "You can also paste your sample analysis method here"
                         ),
               selectInput(inputId = ns("dataProcess"),
                           label = "2.3 Select Data Processing Method",
                           choices = list("None",
                                          "Method 1",
                                          "Method 2",
                                          "Method 3"),
                           selected = "None",
                           multiple = FALSE
                           ),
               textInput(inputId = ns("dataProcessManual"),
                         label = "(or) Use Another Data Processing Method",
                         value = "",
                         placeholder = "You can also paste your data processing method here"
                         ),
               actionButton(inputId = ns("getReport"),
                            label = "Generate Report",
                            icon = icon("paper-plane"),
                            style = "color: #fff; background-color: #4daf4a; border-color: #4daf4a"
                            )
               )
             ),

      #3. Download Report Panel ================================================
      column(width = 7,
             box(
               width = 12,
               inputId = "report_card",
               title = strong("Report"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               p(style = "color:#b2182b;", shiny::icon("bell"), strong("Note: ")),
               p(style = "color:#b2182b;", "It may take few minutes to download the reuslt.
                 Don't repeatedly click the download button."),
               uiOutput(ns("report_button"))
               )
             )
       )
  )
}

#===============================================================================
# =============================== Server =======================================
#===============================================================================

#' 05_downloadReport Server Functions
#'
#' @noRd
mod_05_downloadReport_server <- function(id, sfData = global, inputData, resultList){
  ns <- NS(id)
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #1. Prepare Input File -----------------------------------------------------
    projectName <- reactive({
      input$projectName
       })
    authorName <- reactive({
      input$authorName
       })
    slectedSamplePrep <- reactive({
      inFile <- input$samplePrep
      return(getSamplePrep(inFile))
      })

    ## Prepare Report ----------------------------------------------------------
    output$report_button <- ({NULL})
    observeEvent(input$getReport, {
      output$report_button <- renderUI({
        downloadButton(outputId = ns("report"),
                       label = "Download Report",
                       style= "color: #fff; background-color: #7570b3; border-color: #7570b3"
                       )
        })
      })

    ## Generate and Download Report --------------------------------------------
    output$report <- downloadHandler(
      filename <- paste0(Sys.Date(), "_Report.html"),
      content <- function(file){
        shiny::withProgress(
          message = "Generating report, it will take few minutes",
          detail = "Be patient...",
          value = 0.4,
          {
            tempReport <- file.path(tempdir(), "Report.Rmd")
            tempCSS <- file.path(tempdir(), "style.css")
            tempLogo <- file.path(tempdir(), "logo.png")
            file.copy(app_sys("app/www/Report.Rmd"), tempReport, overwrite = TRUE)
            file.copy(app_sys("app/www/reportCSS/style.css"), tempCSS)
            file.copy(app_sys("app/www/img/logo.png"), tempLogo)
            params <- list(
              myRawData = inputData(), # raw data
              sampleGroup = sfData$group, # complete sample meta group
              dataGlobal3PCA = resultList$dataGlobal3PCA(), # data for PLSDA
              OPLSDAGroup = resultList$OPLSDAGroup(), # group information for PLSDA plot
              statTable = resultList$statTable(), # data matrix for volcano plot
              VCGroup = resultList$VCGroup(), # group information for volcano plot
              myStat = resultList$combinedTable(),
              myPCAPlot = resultList$PCAPlot(),
              myHMPlot = resultList$HMPlot(),
              dataGlobal3Transform= resultList$dataGlobal3Transform(), # data for boxplot
              BPGroup = resultList$BPGroup(), # group information for boxplot
              BPTransform = resultList$BPTransform(), # data transformation for boxplot
              KMDG = resultList$KMDG(),
              KMTrendPlot = resultList$KMTrendPlot(),
              KMTable = resultList$KMTable(),
              projectName = projectName(),
              authorName = authorName(),
              showIntro = input$showIntro,
              showMethods = input$showMethods,
              mySamplePrep = slectedSamplePrep()
              )
            rmarkdown::render(input = tempReport,
                              output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
                              )
            })
        }
    )
  })
}

## To be copied in the UI
# mod_05_downloadReport_ui("05_downloadReport_1")

## To be copied in the server
# mod_05_downloadReport_server("05_downloadReport_1")
