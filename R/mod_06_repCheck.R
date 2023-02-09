#' 06_repCheck UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))


mod_06_repCheck_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      #(1) User Guide ==========================================================
      column(width = 12,
             box(
               width = 12,
               title = strong("User Guide"),
               status = "warning",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               p("1. This widget is designed to check whether duplicate rows exist."),
               p("2. After uploading the data table, you can select the columns to define duplicate rows. For instance, if column A and B are selected,
                 then rows with same information in column A and B will be treated as duplicated rows"),
               p("3. Then click", strong("Submit"), "button to perform duplicate analysis."),
               p("4. A label", strong("true"), "will be added to duplicate rows. For rows without any duplicates, a label", strong("false"), "will be added.")
               )
             ),

      #(2) Data Input Panel ====================================================
      column(width = 4,
             ##(1) Data upload Panel -------------------------------------------
             box(
               width = 12,
               inputId = "input_card",
               title = strong("Data Input Panel"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = FALSE,
               collapsed = FALSE,
               closable = FALSE,
               fileInput(inputId = ns("rawFile"),
                         label = "1. Upload Data Table:",
                         multiple = FALSE,
                         placeholder = "accepts csv, xls or xlsx format",
                         accept = c(".csv", ".xls", ".xlsx")
                         ),
               uiOutput(outputId = ns("selectColumn"),
                        label = "Please select the columns for replicate analysis"
                        ),
               actionButton(inputId = ns("submit"),
                            label = "Submit",
                            icon = icon("paper-plane"),
                            style = "color: #fff; background-color: #4daf4a; border-color: #4daf4a"
                            )
               )
             ),

      #(3) Result Panel ========================================================
      column(width = 8,
             box(
               width = 12,
               inputId = "repCheck_card",
               title = strong("Replicate Data Panel"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               downloadButton(outputId = ns("downloadrepCheckTable")),
               br(),
               br(),
               DT::dataTableOutput(outputId = ns("repView"))
               )
             )
      )
    )
  }

#' 06_repCheck Server Functions
#'
#' @noRd
mod_06_repCheck_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #1.Load Data ===============================================================
    inputData <- reactive({
      inFile <- input$rawFile
      if(is.null(inFile)){return(NULL)}
      extension <- tools::file_ext(inFile$name)
      filepath <- inFile$datapath
      df <- switch(extension,
                   csv = read.csv(filepath, header = TRUE, check.names = FALSE),
                   xls = readxl::read_xls(filepath),
                   xlsx = readxl::read_xlsx(filepath)
                   )
      return(df)
    })

    #2. Show Result ============================================================

    ##(1) Select Columns for Replicate Analysis --------------------------------
    output$selectColumn <- renderUI({
      shiny::req(inputData())
      selectInput(inputId = ns("selectColumn"),
                  label = "Select columns to remove",
                  multiple = TRUE,
                  choices = names(inputData())
                  )
      })

    observeEvent(input$submit, {
      ##(2) Replicates Analysis ------------------------------------------------
      repData <- inputData() %>%
        dplyr::group_by(across(input$selectColumn)) %>%
        dplyr::mutate(isDuplicate = n()>1) %>%
        dplyr::relocate(isDuplicate)

      ##(3) Show Replicate Table -----------------------------------------------
      output$repView <- DT::renderDataTable({
        shiny::validate(need(!is.null(repData), message = "No input data"))
        DT::datatable(repData,
                      options = list(scrollX = TRUE,
                                     deferRender = TRUE,
                                     scroller = TRUE,
                                     fixedColumns = FALSE
                                     )
                      )
        })

      ##(4) Download Replicate Table -------------------------------------------
      output$downloadrepCheckTable <- downloadHandler(
        filename = "replicatesCheck.csv",
        content = function(file){
          write.csv(repData, file, row.names = FALSE)
          }
        )

    })
  })
}

## To be copied in the UI
# mod_06_repCheck_ui("06_repCheck_1")

## To be copied in the server
# mod_06_repCheck_server("06_repCheck_1")
