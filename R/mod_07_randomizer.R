#' 07_randomizer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

sampleMatrix <- matrix(data = NA, nrow = 1, ncol = 2, dimnames = list(NULL, c("Group", "Replicates")))
mod_07_randomizer_ui <- function(id){
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
               p("1. This widget was designed to prepare LCMS injection sequence using a block randomization approach."),
               p("2. Set the following parameters and click the", strong("Randomize"), "button, your injection sequence will be ready to download."),
               p("3. If you are unsatistied with the generated sequence, you can click the", strong("Randomize"),
                 "button to re-generate a new sequence file.")
               )
             ),

      #2. Data Input Panel =====================================================
      column(width = 4,
             box(
               width = 12,
               inputId = "input_card",
               title = strong("Enter sample metadata and replicates"),
               status = "primary",
               solidHeader = FALSE,
               collapsible = FALSE,
               collapsed = FALSE,
               closable = FALSE,

               shinyMatrix::matrixInput(
                 inputId = ns("sampleList"),
                 value = sampleMatrix,
                 rows = list(extend = TRUE, names = FALSE),
                 cols = list(names = TRUE),
                 class = (c("character"))
                 ),

               p(style = "color:#b2182b;", shiny::icon("bell"), strong("Note: ")),
               p(style = "color:#b2182b;", "1.Blanks will be added at the beginning of the injection sequence."),
               p(style = "color:#b2182b;", "2.QC will be inserted every nth of the sample."),
               sliderInput(inputId = ns("nBlank"),
                           label = "How many Blanks do you want to use?",
                           min = 0,
                           max = 20,
                           value = 0,
                           step = 1
                           ),
               sliderInput(inputId = ns("nQC"),
                           label = "What frequency (every Nth sample) do you want to insert QC?",
                           min = 0,
                           max = 20,
                           value = 0,
                           step = 1,
                           ),
               p(strong("Illustration of a LC sample plate")),
               column(width = 12, img(src='www/img/lcPlate.png', align = "left", width = "100%")),
               numericInput(inputId = ns("plateRow"),
                            label = "Please enter the row number of the sample plate",
                            value = 6,
                            min = 1,
                            step = 1
                            ),
               numericInput(inputId = ns("plateCol"),
                            label = "Please enter the row number of the sample plate",
                            value = 8,
                            min = 1,
                            step = 1
                            ),
               selectInput(inputId = ns("plateIDType"),
                           label = "Please select the sample plate ID type",
                           choices = list("plates are labeled by Letter, such as A, B, C" = "Letter",
                                          "plates are lebeled by numbers, such as 1, 2, 3" = "Number"
                                          ),
                           selected = "Letter"
                           ),
               selectInput(inputId = ns("outputType"),
                           label = "Please select the injection sequence type",
                           choices = list("Orbitrap", "Waters"),
                           selected = "Orbitrap"
                           ),
               actionButton(inputId = ns("randomize"),
                            label = "Randomize",
                            icon = icon("paper-plane"),
                            style = "color: #fff; background-color: #4daf4a; border-color: #4daf4a"
                            )
               )
             ),

      #(3) Result Panel ========================================================
      column(width = 8,
             box(
               width = 12,
               inputId = "report_card",
               title = strong("Result"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               p(style = "color:#b2182b;", shiny::icon("bell"), strong("Note: ")),
               p(style = "color:#b2182b;", "For Obitrap-type sequence, you need to insert", strong("Bracket Type=4"), "in the first row of the download sequence file."),
               uiOutput(outputId = ns("downloadSeq")),
               hr(),
               DT::dataTableOutput(ns("randomizedList"))
               )
             )
      )
    )
  }

#' 07_randomizer Server Functions
#'
#' @noRd
mod_07_randomizer_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #(1) Parameters ============================================================
    nBlank <- reactive({input$nBlank})
    nQC <- reactive({input$nQC})
    plateRow <- reactive({input$plateRow})
    plateCol <- reactive({input$plateCol})
    plateIDType <- reactive({input$plateIDType})
    outputType <- reactive({input$outputType})

    #(2) Show Result ===========================================================
    observeEvent(input$randomize, {

      ##(1) Load Data ---------------------------------------------------------
      inputData <- reactive({
        inFile <- input$sampleList
        if(any(inFile == "")) {return(NULL)}
        inFile <- as.data.frame(inFile)
        suppressWarnings(inFile$Replicates <- as.numeric(inFile$Replicates))
        if(is.na(sum(inFile$Replicates))) {return(NULL)} ## check non-numeric
        if(any(inFile$Replicates <= 0)) {return(NULL)} ## check value < 0

        DF <- data.frame(Group = rep(inFile$Group, times = inFile$Replicates)) %>%
          dplyr::mutate(Group = as.factor(Group)) %>%
          dplyr::group_by(Group) %>%
          dplyr::mutate(Rep = 1:length(Group)) %>%
          as.data.frame() ## important, otherwise it is not random
        return(DF)
        })

      ##(2) Randomize sample ---------------------------------------------------
      randomizedDF <- reactive({
        shiny::validate(need(!is.null(inputData()), message = "No input Data"))
        randomizeSeq(df = inputData(),
                     nBlank = nBlank(),
                     nQC = nQC(),
                     plateRow = plateRow(),
                     plateCol = plateCol(),
                     plateIDType = plateIDType(),
                     outputType = outputType()
                     )
        })

      #(3) Prepare output file -------------------------------------------------
      sequenceFile <- reactive({
        getRows <- nrow(randomizedDF())
        if(!is.null(randomizedDF()) & outputType() == "Orbitrap"){
          header <- "Bracket Type=4"
          thermoFile <- data.frame(
            "Sample Type" = character(getRows),
            "File Name" = character(getRows),
            "Sample ID" = character(getRows),
            "Path" = character(getRows),
            "Instrument Method" = character(getRows),
            "Process Method" = character(getRows),
            "Calibration File" = character(getRows),
            "Position" = character(getRows),
            "Inj Vol" = character(getRows),
            "Level" = character(getRows),
            "Sample Wt" = character(getRows),
            "Sample Vol" = character(getRows),
            "ISTD Amt" = character(getRows),
            "Dil Factor" = character(getRows),
            "L1 Study" = character(getRows),
            "L2 Client" = character(getRows),
            "L3 Laboratory" = character(getRows),
            "L4 Company" = character(getRows),
            "L5 Phone" = character(getRows),
            "Comment" = character(getRows),
            "Sample Name" = character(getRows),
            check.names = FALSE
            )
          thermoFile$`File Name` <- randomizedDF()$Sample
          thermoFile$`Position` <- randomizedDF()$Position
          thermoFile$`Sample Type` <- rep("Unknown", times = nrow(thermoFile))
          return(thermoFile)
        }
        if(!is.null(randomizedDF()) & outputType() == "Waters"){
          watersFile <- data.frame(
            "File Name" = character(getRows),
            "File Text" = character(getRows),
            "MS File" = character(getRows),
            "MS Tune File" = character(getRows),
            "Inlet File" = character(getRows),
            "Bottle" = character(getRows),
            "Injection Volume" = character(getRows),
            check.names = FALSE
            )
          watersFile$`File Name` <- randomizedDF()$Sample
          watersFile$`Bottle` <- randomizedDF()$Position
          return(watersFile)
        }
      })

      ## (3) show randomized sample list ---------------------------------------
      output$randomizedList <- DT::renderDataTable({
        shiny::validate(need(!is.null(inputData()), message = "No input or Replicates should be positive numbers"))
        DT::datatable(sequenceFile(),
                      extensions = "Buttons",
                      options = list(scrollX = TRUE,
                                     deferRender = TRUE,
                                     scroller = TRUE,
                                     fixedColumns = FALSE,
                                     editable = TRUE
                                     )
                      )
        })

      #(4) download result ----------------------------------------------------
      output$downloadSeq <- renderUI({
        if(!is.null(sequenceFile())) {
          downloadButton(outputId = ns('seqFile'),
                         label = paste('Download', outputType(), "Injection Sequence", sep = "_"),
                         style = "color: #fff; background-color: #037e8a; border-color: #037e8a"
                         )
          }
        })

      output$seqFile <- downloadHandler(
        filename <- paste0(outputType(), "_Sequence_", Sys.Date(), ".csv"),
        content <- function(file) {
          write.csv(sequenceFile(), file, row.names = FALSE)
          }
        )
      })
  })
}

## To be copied in the UI
# mod_07_randomizer_ui("07_randomizer_1")

## To be copied in the server
# mod_07_randomizer_server("07_randomizer_1")
