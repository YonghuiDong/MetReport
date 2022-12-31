#' 03_preprocess UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput

mod_03_preprocess_ui <- function(id){
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
               closable = FALSE
               )
             ),

      #2. Data Cleaning Panel ==================================================
      column(width = 4,
             ##(1) Missing Data Computation Panel ------------------------------
             box(
               width = 12,
               inputId = "input_card",
               title = strong("Missing Value Imputation"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               p(style = "color:#C70039;", shiny::icon("bell-o"), strong("Note:")),
               p(style = "color:#C70039;", "This step will be auto-skipped if no missing values are detected"),
               p(strong("Select (a) or (b):")),
               p(style = "color:#C70039;", "(a) Missing Not At Random (MNAR)"),
               p(style = "color:#C70039;", "(b) Missing Completely At Random (MCAR)"),

               radioButtons(inputId = ns("missingValue"),
                            label = "1. Compute/Replace missing value with:",
                            choices = c("1 (a)" = "1",
                                        "1/5 minimum of the mass feature, LOD (a)" = "HM",
                                        "k-Nearest Neighbours (b)" = "KNN",
                                        "Feature mean (b)" = "Mean",
                                        "Feature median (b)" = "Median"
                                        ),
                            selected = "1"
                            )
               ),

             ##(2) Data Filtering Panel ----------------------------------------
             box(
               width = 12,
               inputId = "input_card",
               title = strong("QC-based Data Filtering"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               radioButtons(inputId = ns("QCFiltering"),
                            label = "2. Feature filter using QC samples?",
                            choices = c("Yes" = "1", "No" = "0"),
                            selected = "1"
                            ),

               p(style = "color:#C70039;", shiny::icon("bell-o"), strong("Note: ")),
               p(style = "color:#C70039;", "Features with CV value > the threshold will be filtered out"),

               sliderInput(inputId = ns("QCCV"),
                           label = "If [Yes]: input CV% filtering threshold in QC",
                           value = 30,
                           min = 0,
                           max = 100
                           )
               ),

             ##(3) Data Transformation Panel -----------------------------------
             box(
               width = 12,
               inputId = "input_card",
               title = strong("Data Treatment"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,

               radioButtons(inputId = ns("normalizeMethod"),
                            label = "Sample Normalization",
                            choices = c("None" = "None",
                                        "Linear baseline normalization based on mean values" = "LBME",
                                        "Linear baseline normalization based on median values" = "LBMD",
                                        "Probabilistic quotient normalization" = "PQN",
                                        "Quantile normalization" = "QT"
                                        ),
                            selected = "None"
                            ),

               radioButtons(inputId = ns("transformMethod"),
                            label = "Feature Transformation",
                            choices = c("None" = "None",
                                        "Log10" = "Log10"
                                        ),
                            selected = "Log10"
                            ),

               radioButtons(inputId = ns("scaleMethod"),
                            label = "Feature Scaling",
                            choices = c("None" = "None",
                                        "Mean Centering" = "meanCenter",
                                        "Auto Scaling" = "autoScale",
                                        "Pareto Scaling" = "paretoScale",
                                        "Range Scaling" = "rangeScale",
                                        "Vast Scaling" = "vastScale",
                                        "Level Scaling" = "levelScale"
                                        ),
                            selected = "autoScale"
                            )
               )
             ),

      #3.Result Panel ==========================================================
      column(width = 8,
             ##(1) Boxplot Panel -----------------------------------------------
             box(
               width = 12,
               inputId = ns("Boxplot_card"),
               title = strong("Feature Boxplot Panel"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               p(style = "color:#C70039;", shiny::icon("bell-o"), strong("Note: ")),
               p(style = "color:#C70039;", "1. The Boxplot shows you how the feature distributions change upon
                 using different sample normalization methods."),
               p(style = "color:#C70039;", "2. The Boxplot can be used to select sample normalization method."),
               p(style = "color:#C70039;", "3. Scaling is not applied here, therefore change of scaling method will not
                 affect Boxplot."),
               varSelectInput(inputId = ns("prePCAGroup"),
                              label = "1. Select MetaData (Sample Groups) for Sample Coloring",
                              data = ""
                              ),
               shiny::plotOutput(ns("TICPlot"))
               ),

             ##(2) PCA Panel ---------------------------------------------------
             box(
               width = 12,
               inputId = "RawData_card",
               title = strong("PCA Panel"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               fluidRow(width = 12,
                        column(width = 6,
                               selectInput(inputId = ns("prePCAX"),
                                           label = "Which PC in X-axis?",
                                           choices = list(1, 2, 3, 4),
                                           selected = 1,
                                           multiple = FALSE
                                           )
                               ),

                        column(width = 6,
                               selectInput(inputId = ns("prePCAY"),
                                           label = "Which PC in Y-axis?",
                                           choices = list(1, 2, 3, 4),
                                           selected = 2,
                                           multiple = FALSE
                                           )
                               ),

                        column(width = 6,
                               radioButtons(inputId = ns("prePCAFrame"),
                                            label = "Add frame for group?",
                                            choices = list("none", "polygon", "norm"),
                                            selected = "none"
                                            )
                               )
                        ),
               plotly::plotlyOutput(ns("prePCAPlot"))
             ),

             ##(3) Feature Panel -----------------------------------------------
             box(
               width = 12,
               inputId = "Feature_card",
               title = strong("Feature Panel"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               p(style = "color:#C70039;", shiny::icon("bell-o"), strong("Note: ")),
               p(style = "color:#C70039;", "1. You can click this button to view the results for 30 randomly selected features"),
               p(style = "color:#C70039;", "2. You can click this button many times to view different features"),

               actionButton(inputId = ns("featureSampling"),
                            label = "Sampling",
                            icon = icon("paper-plane"),
                            style = "color: #fff; background-color: #7570b3; border-color: #7570b3"
                            ),
               shiny::plotOutput(ns("featureDensityPlot")),
               shiny::plotOutput(ns("featureBoxPlot"))
               )
             )
      )
    )
}


#' 03_preprocess Server Functions
#'
#' @noRd
#' @importFrom ggplot2 aes element_text position_jitter

mod_03_preprocess_server <- function(id, sfData){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    #1. Pre-processing Parameters ==============================================
    missingValue <- reactive({
      as.character(input$missingValue)
      })

    QCFiltering <- reactive({
      as.logical(as.numeric(input$QCFiltering))
      })

    QCCV <- reactive({
      input$QCCV
      })

    normalizeMethod <- reactive({
      as.character(input$normalizeMethod)
      })

    transformMethod <- reactive({
      as.character(input$transformMethod)
      })

    scaleMethod <- reactive({
      as.character(input$scaleMethod)
      })

    prePCAGroup <- reactive({
      as.character(input$prePCAGroup)
      })

    observeEvent(sfData$group, {
      updateVarSelectInput(
        inputId = "prePCAGroup",
        data = sfData$group,
        selected = "Group1"
        )
      })

    prePCAX <- reactive({
      as.numeric(input$prePCAX)
      })

    prePCAY <- reactive({
      as.numeric(input$prePCAY)
    })

    prePCAFrame <- reactive({
      as.character(input$prePCAFrame)
    })

    #2. Data Pre-processing ====================================================
    ##(1) Fill missing values---------------------------------------------------
    fillMissing <- reactive({
      shiny::req(sfData$data)
      df1 <- dplyr::select(sfData$data, -ID)
      if(anyNA(df1)) {
        df2 <- switch(missingValue(),
                      "1" = replace(df1, is.na(df1), 1),
                      "HM" = as.data.frame(t(apply(df1, 1, function(x) replace(x, is.na(x), 0.2 * min(x, na.rm = TRUE))))),
                      "KNN" = kNNMissing(df1),
                      "Mean" = as.data.frame(t(apply(df1, 1, function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))),
                      "Median" = as.data.frame(t(apply(df1, 1, function(x) replace(x, is.na(x), median(x, na.rm = TRUE)))))
                      )
        } else {
          df2 <- df1
          }
      ## replace 0 with 1, for log transformation
      df2[df2 == 0] <- 1
      ## add ID back
      df2$ID <- cleanNames(sfData$data$ID)
      df2 <- df2 %>% dplyr::relocate(ID)
      return(df2)
      })

    ##(2) QC-based filtering----------------------------------------------------
    tFillMissing <- reactive({
      shiny::req(fillMissing())
      tem <- fillMissing() %>%
        dplyr::select(-ID) %>%
        t()
      colnames(tem) <- fillMissing()$ID
      return(tem)
      })

    dfCV <- reactive({
      shiny::req(tFillMissing())
      getCV(x = tFillMissing(), Group = sfData$group[, prePCAGroup()])
    })

    QCFilteredData <- reactive({
      shiny::req(fillMissing())
      shiny::req(dfCV())
      tem1 <- cbind.data.frame(dfCV(), fillMissing())
      if(isTRUE(QCFiltering()) & ("CV_QC" %in% names(tem1))){
        tem2 <- tem1 %>%
          dplyr::filter(CV_QC <= QCCV()) %>%
          dplyr::select(-starts_with("CV"))
        } else{
        tem2 <- fillMissing()
        }
      rownames(tem2) <- NULL
      sfData$filter <- tem2
      return(tem2)
      })

    ##(3) Data treatment--------------------------------------------------------
    treatedData <- reactive({
      shiny::req(QCFilteredData())
      normalizedData <- doNormalization(QCFilteredData() %>% dplyr::select(-ID),
                                      Method = normalizeMethod())

      transformedData <- switch(transformMethod(),
                                "None" = normalizedData,
                                "Log10" = log10(normalizedData)
                                )

      sfData$filterNormTransform <- transformedData %>%
        as.data.frame() %>%
        dplyr::mutate(ID = QCFilteredData()$ID) %>%
        dplyr::relocate(ID)

      scaledData <- scaleData(transformedData, Method = scaleMethod()) %>%
        as.data.frame() %>%
        dplyr::mutate(ID = QCFilteredData()$ID) %>%
        dplyr::relocate(ID)
      sfData$clean <- scaledData
      return(scaledData)
      })

    #3.Output Plot and Table ---------------------------------------------------
    ##(1) Feature Box Plot------------------------------------------------------
    output$TICPlot <- shiny::renderPlot({
      shiny::validate(need(!is.null(sfData$data), message = "Input data not found"))
      showTIC(df = sfData$filterNormTransform, Group = sfData$group[, prePCAGroup()])
    })

    ##(2) prePCA ---------------------------------------------------------------
    prePCAPlot <- reactive({
      shiny::req(treatedData())
      shiny::req(sfData$group)
      prePCAData <- treatedData() %>%
        select(-ID) %>%
        t()
      p <- showPCA(prePCAData,
                   Group = sfData$group[, prePCAGroup()],
                   inx = prePCAX(),
                   iny = prePCAY(),
                   showFrame = prePCAFrame(),
                   interactive = TRUE
                   )
      return(p)
      })

    output$prePCAPlot <- plotly::renderPlotly({
      shiny::validate(need(!is.null(sfData$data), message = "Input data not found"))
      shiny::validate(need(prePCAX() != prePCAY(), message = "Please use different PCs"))
      prePCAPlot()
      })

    ##(3) Plot Features --------------------------------------------------------
    ## assign initial values for nFeature
    nFeature <- reactiveValues()
    nFeature$n <- 1:30
    observeEvent(input$featureSampling, {
      nFeature$n <- sample(1:dim(QCFilteredData())[1], min(30, dim(QCFilteredData())[1]))
      })

    combinedData <- reactive({
      preData <- QCFilteredData()[nFeature$n, ] %>%
        dplyr::mutate(ID = sub("_.*", "", ID)) %>%
        tidyr::pivot_longer(cols = !ID, names_to = "Sample", values_to = "Area") %>%
        dplyr::mutate(Treatment = "Before Data Treatment")

      afterData <- treatedData()[nFeature$n, ] %>%
        dplyr::mutate(ID = sub("_.*", "", ID)) %>%
        tidyr::pivot_longer(cols = !ID, names_to = "Sample", values_to = "Area") %>%
        dplyr::mutate(Treatment = "After Data Treatment")

      combinedData <- rbind.data.frame(preData, afterData) %>%
        dplyr::mutate(Treatment = factor(Treatment,
                                         levels = c("Before Data Treatment", "After Data Treatment")))
      return(combinedData)
      })

    featureDensityPlot <- reactive({
      p <- ggplot2::ggplot(combinedData(), aes(x = Area, y=..scaled.., fill = Treatment)) +
        ggplot2::geom_density(alpha = 0.8) +
        ggplot2::scale_fill_manual(values = c("#e9a3c9", "#a1d76a"), guide = 'none') +
        ggplot2::facet_wrap(~ Treatment, scales = "free") +
        ggplot2::xlab("") +
        ggplot2::ylab("Density") +
        ggplot2::ggtitle(paste0("Density plots of ", length(nFeature$n), " randomly selected features")) +
        ggplot2::theme_bw() +
        ggplot2::theme(text = element_text(size = 16))
      return(p)
      })

    featureBoxPlot <- reactive({
      p <- ggplot2::ggplot(combinedData(), aes(x = ID, y = Area)) +
        ggplot2::geom_boxplot(aes(fill = Treatment), alpha = 0.8) +
        ggplot2::scale_fill_manual(values = c("#e9a3c9", "#a1d76a"), guide = 'none') +
        ggplot2::geom_jitter(position = position_jitter(0.2), alpha = 0.3) +
        ggplot2::facet_wrap(~ Treatment, scales = "free") +
        ggplot2::xlab("Metabolite ID") +
        ggplot2::ylab("Peak Area") +
        ggplot2::ggtitle(paste0("Box plots of ", length(nFeature$n), " randomly selected features")) +
        ggplot2::theme_bw() +
        ggplot2::theme(text = element_text(size = 16)) +
        ggplot2::coord_flip()
      return(p)
      })

    output$featureDensityPlot <- shiny::renderPlot({
      shiny::validate(need(!is.null(sfData$data), message = "Input data not found"))
      featureDensityPlot()
      })

    output$featureBoxPlot <- shiny::renderPlot({
      shiny::validate(need(!is.null(sfData$data), message = "Input data not found"))
      featureBoxPlot()
      })

  })
}

## To be copied in the UI
# mod_03_preprocess_ui("03_preprocess_1")

## To be copied in the server
# mod_03_preprocess_server("03_preprocess_1")
