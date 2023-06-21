#' 04_viewResult UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom dplyr n
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#'
mod_04_viewResult_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      #(1) User Guide **********************************************************
      column(width = 12,
             box(
               width = 12,
               title = strong("User Guide"),
               status = "warning",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               p("1. Statistical analyses are perfromed in this panel. Tables and most figures are interactive so that you can better inspect your data and results."),
               p("2. You can customize the figures by changing the color, aspect ratio before downloading them with preferred format.")
               )
             ),

      #(2) View Statistics Panel ***********************************************
      column(width = 4,
             box(
               width = 12,
               inputId = ns("input_card"),
               title = strong("Perform Statistics"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = FALSE,
               collapsed = FALSE,
               closable = FALSE,
               selectInput(inputId = ns("statMethod"),
                           label = "1. What do you want to do?",
                           choices = c("Compare two unpaired groups" = "tTest",
                                       "Compare two paired groups" = "ptTest",
                                       "Compare more than two unmatched groups" = "anovaHSD",
                                       "Compare more than two matched groups" = "anovaRM"
                                       ),
                           selected = "anovaHSD",
                           multiple = FALSE
                           ),
               selectInput(inputId = ns("pAdjMethod"),
                           label = "2. Adjust p-value by:",
                           choices = c("Don't adjust" = "none",
                                       "False Discovery Rate" = "fdr",
                                       "Bonferroni" = "bonferroni"
                                       ),
                           selected = "fdr",
                           multiple = FALSE
                           ),
               radioButtons(inputId = ns("SigFilter"),
                            label = "3. Only consider statistically significant mass features?",
                            choices = c("Yes" = 1, "No" = 0),
                            selected = 0
                            ),
               p(style = "color:#C70039;", shiny::icon("bell"), strong("Notes:")),
               p(style = "color:#C70039;", "1. Features with p-values < the threshold in all the sample groups will be removed"),
               p(style = "color:#C70039;", "2. This step is not applied to PCA, OPLS-DA or Volcano plot"),
               sliderInput(inputId = ns("SigP"),
                           label = "If [Yes]: enter p-value threshold",
                           value = 0.05,
                           min = 0,
                           max = 0.1
                           ),
               actionButton(inputId = ns("viewStat"),
                            label = "Start",
                            icon = icon("eye"),
                            style = "color: #fff; background-color: #4daf4a; border-color: #4daf4a"
                            )
               )
             ),

      #(3) Result Panel ********************************************************
      column(width = 8,
             ##(1) Statistics Panel --------------------------------------------
             box(width = 12,
                 inputId = "Stat_card",
                 title = strong("Statistics Panel"),
                 status = "success",
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 collapsed = FALSE,
                 closable = FALSE,
                 p(style = "color:#C70039;", shiny::icon("bell"), strong("Note: ")),
                 p(style = "color:#C70039;", "1. Fold change (FC) analysis is performed on missing value-filled and QC-filtered data."),
                 p(style = "color:#C70039;", "2. Univariate analysis is performed on filled, filtered, normalized and transformed data."),
                 p(style = "color:#C70039;", "3. VIP values are calculated based on filled, filtered, normalized, transformed and scaled data."),
                 fluidRow(width = 12,
                          column(width = 12,
                                 varSelectInput(inputId = ns("StatGroup"),
                                                label = "Select MetaData (Sample Groups) for statistics",
                                                data = ""
                                                )
                                 ),
                          column(width = 12, downloadButton(outputId = ns("downloadStatTable")))
                          ),
                 br(),
                 shinycssloaders::withSpinner(DT::dataTableOutput(ns("StatResult")), type = 5)
                ),

             ##(2) PCA Panel ---------------------------------------------------
             box(
               width = 12,
               inputId = "viewPCA",
               title = strong("PCA Plot"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               varSelectInput(inputId = ns("PCAGroup"),
                              label = "1. Select MetaData (Sample Groups) for Sample Coloring",
                              data = ""
                              ),
               fluidRow(width = 12,
                        column(width = 6,
                               selectInput(inputId = ns("PCAX"),
                                           label = "Which PC in X-axis?",
                                           choices = list(1, 2, 3, 4),
                                           selected = 1,
                                           multiple = FALSE
                                           )
                               ),
                        column(width = 6,
                               selectInput(inputId = ns("PCAY"),
                                           label = "Which PC in Y-axis?",
                                           choices = list(1, 2, 3, 4),
                                           selected = 2,
                                           multiple = FALSE
                                           )
                               ),
                        column(width = 12,
                               radioButtons(inputId = ns("PCAFrame"),
                                            label = "Add frame for group?",
                                            choices = list("none", "polygon", "norm"),
                                            selected = "none"
                                            )
                               ),
                        column(width = 6,
                               selectInput(inputId = ns("PCAColor"),
                                           label = "Color palette?",
                                           choices = list("Default", "Accent", "Dark2", "Paired", "Pastel2", "Pastel1", "Set1", "Set2", "Set3"),
                                           selected = "Default"
                                           )
                               ),
                        column(width = 6,
                               radioButtons(inputId = ns("PCAType"),
                                            label = "Select file type to download",
                                            choices = list("pdf", "tiff", "png"),
                                            selected = "pdf"
                                            )
                               ),
                        column(width = 6,
                               numericInput(inputId = ns("PCARatio"),
                                            label = "Aspect ratio of the plot (width:height)",
                                            value = 1,
                                            min = NA,
                                            max = NA
                                            )
                               ),
                        column(width = 6,
                               br(),
                               downloadButton(outputId = ns("downloadPCA"))
                               )
                        ),
               shinycssloaders::withSpinner(plotly::plotlyOutput(ns("PCAPlot")), type = 5)
               ),

             ##(3) OPLSDA Plot Panel -------------------------------------------
             box(
               width = 12,
               inputId = "viewOPLSDA",
               title = strong("OPLS-DA Plot"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               varSelectInput(inputId = ns("OPLSDAGroup"),
                              label = "1. Select MetaData (Sample Groups)",
                              data = ""
                              ),
               fluidRow(width = 12,
                        column(width = 6,
                               selectInput(inputId = ns("OPLSDALevel1"),
                                           label = "2. Select the first sample group level",
                                           choices = ""
                                           )
                               ),
                        column(width = 6,
                               selectInput(inputId = ns("OPLSDALevel2"),
                                           label = "2. Select the second sample group level",
                                           choices = ""
                                           )
                               ),
                        column(width = 6,
                               selectInput(inputId = ns("OPLSDAColor"),
                                           label = "Color palette?",
                                           choices = list("Default", "Accent", "Dark2", "Paired", "Pastel2", "Pastel1", "Set1", "Set2", "Set3"),
                                           selected = "Default"
                                           )
                               ),
                        column(width = 6,
                               radioButtons(inputId = ns("OPLSDAType"),
                                            label = "Select file type to download",
                                            choices = list("pdf", "tiff", "png"),
                                            selected = "pdf"
                                            )
                               ),
                        column(width = 6,
                               numericInput(inputId = ns("OPLSDARatio"),
                                            label = "Aspect ratio of the plot (width:height)",
                                            value = 1,
                                            min = NA,
                                            max = NA
                                            )
                               ),
                        column(width = 6,
                               br(),
                               downloadButton(outputId = ns("downloadOPLSDA"))
                               )
                        ),
               shinycssloaders::withSpinner(shiny::plotOutput(ns("OPLSDAPlot")), type = 5),
               plotly::plotlyOutput(ns("SPlot"))
               ),

             ##(4) Heatmap Panel -----------------------------------------------
             box(
               width = 12,
               inputId = "viewHeatmap",
               title = strong("Heatmap"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               varSelectInput(inputId = ns("HMGroup"),
                              label = "1. Select MetaData (Sample Groups) for Sample Coloring",
                              data = ""
                              ),
               fluidRow(width = 12,
                        column(width = 6,
                               radioButtons(inputId = ns("HMColCluster"),
                                            label = "Cluster columns (mass features) in heatmap?",
                                            choices = c("Yes" = 1, "No" = 0),
                                            selected = 1
                                            )
                               ),
                        column(width = 6,
                               radioButtons(inputId = ns("HMRowCluster"),
                                            label = "Cluster rows (samples) in heatmap?",
                                            choices = c("Yes" = 1, "No" = 0),
                                            selected = 1
                                            )
                               ),
                        column(width = 6,
                               radioButtons(inputId = ns("HMColName"),
                                            label = "Show column names (mass feature names) in heatmap?",
                                            choices = c("Yes" = 1, "No" = 0),
                                            selected = 0
                                            )
                               ),
                        column(width = 6,
                               radioButtons(inputId = ns("HMRowName"),
                                            label = "Show row names (sample names) in heatmap?",
                                            choices = c("Yes" = 1, "No" = 0),
                                            selected = 1
                                            )
                               ),
                        column(width = 6,
                               numericInput(inputId = ns("HMSplitCol"),
                                            label = "Number of column groups (features) you want to split",
                                            value = 1,
                                            min = 1,
                                            step = 1,
                                            )
                               ),
                        column(width = 6,
                               numericInput(inputId = ns("HMSplitRow"),
                                            label = "Number of row groups (samples) you want to split",
                                            value = 1,
                                            min = 1,
                                            step = 1,
                                            )
                               ),
                        column(width = 6,
                               radioButtons(inputId = ns("HMQCFilter"),
                                            label = "Do you want to include QC samples (if exist)?",
                                            choices = c("Yes" = 1, "No" = 0),
                                            selected = 1
                                            )
                               ),
                        column(width = 6,
                               radioButtons(inputId = ns("HMType"),
                                            label = "Select file type to download",
                                            choices = list("pdf", "tiff", "png"),
                                            selected = "pdf", inline = T
                                            )
                               ),
                        column(width = 12,
                               numericInput(inputId = ns("HMRatio"),
                                            label = "Aspect ratio of the plot (width:height)",
                                            value = 1,
                                            min = NA,
                                            max = NA
                                            )
                               ),
                        column(width = 6,
                               br(),
                               downloadButton(outputId = ns("downloadHM"))
                               )
                        ),
               shiny::plotOutput(outputId = ns("HMPlot"))
               ),

             ##(4) Volcano Plot Panel ------------------------------------------
             box(
               width = 12,
               inputId = "viewVolcano",
               title = strong("Volcano Plot"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               fluidRow(width = 12,
                        column(width = 6,
                               selectInput(inputId = ns("VCLevel1"),
                                           label = "1. Select the first sample group level",
                                           choices = ""
                                           )
                               ),
                        column(width = 6,
                               selectInput(inputId = ns("VCLevel2"),
                                           label = "1. Select the second sample group level",
                                           choices = ""
                                           )
                               ),
                        column(width = 6,
                               numericInput(inputId = ns("VCFC"),
                                            label = "2. Input fold change threshold",
                                            value = 2,
                                            min = NA,
                                            max = NA
                                            )
                               ),
                        column(width = 6,
                               numericInput(inputId = ns("VCPvalue"),
                                            label = "3. Input p-value threshold",
                                            value = 0.05,
                                            min = 0,
                                            max = 1
                                            )
                               ),
                        column(width = 6,
                               radioButtons(inputId = ns("VCType"),
                                            label = "Select file type to download",
                                            choices = list("pdf", "tiff", "png"),
                                            selected = "pdf",
                                            inline = T
                                            )
                               ),
                        column(width = 6,
                               numericInput(inputId = ns("VCRatio"),
                                            label = "Aspect ratio of the plot (width:height)",
                                            value = 1,
                                            min = NA,
                                            max = NA
                                            )
                               ),
                        column(width = 4,
                               downloadButton(outputId = ns("downloadVCPlot"))
                               )
                        ),
               plotly::plotlyOutput(ns("VCPlot"))
               ),

             ##(5) K-Means Panel -----------------------------------------------
             box(
               width = 12,
               inputId = "viewKMeans",
               title = strong("K-means Cluster Analysis"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               fluidRow(width = 12,
                        column(width = 6,
                               varSelectInput(inputId = ns("KMGroup"),
                                              label = "1. Select MetaData (Sample Groups)",
                                              data = ""
                                              )
                               ),
                        column(width = 6,
                               numericInput(inputId = ns("KMCluster"),
                                            label = "2. Input number of clusters",
                                            value = 1,
                                            min = 1,
                                            max = NA)
                               )
                        ),
               fluidRow(width = 12,
                        column(width = 6,
                               radioButtons(inputId = ns("KMType"),
                                            label = "Select plot type to download",
                                            choices = list("pdf", "tiff", "png"),
                                            selected = "pdf"
                                            )
                               ),
                        column(width = 6,
                               numericInput(inputId = ns("KMRatio"),
                                            label = "Aspect ratio of the plot (width:height)",
                                            value = 1,
                                            min = NA,
                                            max = NA
                                            )
                               )
                        ),
               fluidRow(width = 12,
                        column(width = 6,
                               downloadButton(outputId = ns("downloadKMTrend"),
                                              label = "Download Line Plot"
                                              )
                               ),
                        column(width = 6,
                               downloadButton(outputId = ns("downloadKMTable"),
                                              label = "Download Table"
                                              )
                               )
                        ),
               br(),
               shiny::plotOutput(ns("KMTrendPlot")),
               DT::dataTableOutput(ns("KMTable"))
               ),

             ##(6) Box Plot Panel ----------------------------------------------
             box(
               width = 12,
               inputId = "viewBoxPlot",
               title = strong("Bar/Box/Violin Plot"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               fluidRow(width = 12,
                        column(width = 12,
                               varSelectInput(inputId = ns("BPGroup"),
                                              label = "1. Select MetaData (Sample Groups)",
                                              data = ""
                                              )
                               ),
                        column(width = 12,
                               varSelectizeInput(inputId = ns("BPMetabolite"),
                                                 label = "2. Select Metabolites",
                                                 data = ""
                                                 )
                               ),
                        column(width = 6,
                               selectInput(inputId = ns("BPTransform"),
                                           label = "3. How to transform the data?",
                                           choices = list("none", "log2", "log10"),
                                           selected = "log10"
                                           )
                               ),
                        column(width = 6,
                               selectInput(inputId = ns("BPPlotType"),
                                           label = "4. Box plot or Violin plot?",
                                           choices = list("Bar plot", "Box plot", "Violin plot"),
                                           selected = "Bar plot"
                                           )
                               ),
                        column(width = 6,
                               selectInput(inputId = ns("BPPlotColor"),
                                           label = "Color palette?",
                                           choices = list("Default", "Accent", "Dark2", "Paired", "Pastel2", "Pastel1", "Set1", "Set2", "Set3"),
                                           selected = "Default"
                                           )
                               ),
                        column(width = 6,
                               radioButtons(inputId = ns("BPType"),
                                            label = "Select plot type to download",
                                            choices = list("pdf", "tiff", "png"),
                                            selected = "pdf"
                                            )
                               ),
                        column(width = 6,
                               numericInput(inputId = ns("BPRatio"),
                                            label = "Aspect ratio of the plot (width:height)",
                                            value = 1,
                                            min = NA,
                                            max = NA
                                            )
                               ),
                        column(width = 6,
                               br(),
                               downloadButton(outputId = ns("downloadBP"),
                                              label = "Download Plot"
                                              )
                               )
                        ),
               shiny::plotOutput(ns("BPPlot"))
               ),

             ##(7) Correlation Analysis Panel ----------------------------------
             # box(
             #   width = 12,
             #   inputId = "viewCN",
             #   title = strong("Correlation Analysis"),
             #   status = "success",
             #   solidHeader = FALSE,
             #   collapsible = TRUE,
             #   collapsed = TRUE,
             #   closable = FALSE,
             #   fluidRow(width = 12,
             #            column(width = 12,
             #                   p(style = "color:#C70039;", strong("Attention:")),
             #                   p("(1) Intensive computation is requied in this section.
             #                 Don't run this section if you have over 150 features.
             #                 Don't worry, you'll not miss any important information."),
             #                 p("(2) ", code("Correlation Network "), "Panel offers you similar result."),
             #                 p("(3) You can check the the number of features in above ", code("Statistics "), "Panel"),
             #                 ),
             #            column(width = 6,
             #                   numericInput(inputId = ns("CAThreshold"),
             #                                label = "Select correlation threshold (absolute value)",
             #                                value = 0,
             #                                min = 0,
             #                                max = 1
             #                                )
             #                   ),
             #            column(width = 6,
             #                   numericInput(inputId = ns("CASize"),
             #                                label = "Set Metabolite ID size",
             #                                value = 3,
             #                                min = 1,
             #                                max = NA
             #                                )
             #                   ),
             #            column(width = 6,
             #                   radioButtons(inputId = ns("CAType"),
             #                                label = "Select plot type to download",
             #                                choices = list("pdf", "tiff", "png"),
             #                                selected = "pdf"
             #                                )
             #                   ),
             #            column(width = 6,
             #                   numericInput(inputId = ns("CARatio"),
             #                                label = "Aspect ratio of the plot (width:height)",
             #                                value = 1,
             #                                min = NA,
             #                                max = NA
             #                                )
             #                   ),
             #            column(width = 12,
             #                   actionButton(inputId = ns("RunCA"),
             #                                label = "Run Analysis",
             #                                icon = icon("paper-plane"),
             #                                style = "color: #fff; background-color: #7570b3; border-color: #7570b3"
             #                                )
             #                   ),
             #            column(width = 6,
             #                   br(),
             #                   downloadButton(outputId = ns("downloadCA"),
             #                                  label = "Download"
             #                                  )
             #                   )
             #            ),
             #   shiny::plotOutput(ns("CAPlot"))
             #   ),

             ##(8) Correlation analysis Panel ----------------------------------
             box(
               width = 12,
               inputId = "viewCN",
               title = strong("Correlation Network"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               fluidRow(width = 12,
                        column(width = 12,
                               varSelectizeInput(inputId = ns("CNMetabolite"),
                                                 label = "1. Select Metabolites",
                                                 data = ""
                                                 )
                               ),
                        column(width = 6,
                               numericInput(inputId = ns("CNThreshold"),
                                            label = "Select correlation threshold (absolute value)",
                                            value = 0.8,
                                            min = 0,
                                            max = 1
                                            )
                               ),
                        column(width = 6,
                               radioButtons(inputId = ns("CNName"),
                                            label = "How to display metabolite name?",
                                            choices = c("By Name" = "Name",
                                                        "By ID" = "IDNO"
                                                        ),
                                            selected = "Name"
                                            )
                               ),
                        column(width = 6,
                               radioButtons(inputId = ns("CNType"),
                                            label = "Select plot type to download",
                                            choices = list("pdf", "tiff", "png"),
                                            selected = "pdf"
                                            )
                               ),
                        column(width = 6,
                               numericInput(inputId = ns("CNRatio"),
                                            label = "Aspect ratio of the plot (width:height)",
                                            value = 1,
                                            min = NA,
                                            max = NA
                                            )
                               ),
                        column(width = 6,
                               br(),
                               downloadButton(outputId = ns("downloadCN"),
                                              label = "Download Plot"
                                              )
                               )
                        ),
               shiny::plotOutput(ns("CNPlot"))
               )
             )
      )
  )
}


#' 04_viewResult Server Functions
#'
#' @noRd
#' @importFrom ggplot2 aes scale_color_brewer scale_fill_brewer position_dodge
mod_04_viewResult_server <- function(id, sfData){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #(1) Statistics Panel=======================================================
    ##(1) Stat parameters-------------------------------------------------------
    StatGroup <- reactive({
      as.character(input$StatGroup)
    })
    observeEvent(sfData$group, {
      updateVarSelectInput(
        inputId = "StatGroup",
        data = sfData$group,
        selected = "Group1"
      )
    })

    ##(2) Perform statistics----------------------------------------------------
    # tDataGlobal <- reactive({
    #   shiny::req(sfData$clean)
    #   transformDF(sfData$clean)
    # })

    statTable <- reactive({
      shiny::req(sfData$filter)
      shiny::req(sfData$filterNormTransform)
      shiny::req(sfData$clean)
      shiny::req(sfData$group)
      ## Transform the data for statistics, QCs are excluded.
      tFilter <- transformDF(sfData$filter, Group = sfData$group[, StatGroup()]) # filtered DF
      tFilterNormTransform <- transformDF(sfData$filterNormTransform, Group = sfData$group[, StatGroup()]) # filtered, normalized and transformed
      tClean <- transformDF(sfData$clean, Group = sfData$group[, StatGroup()]) # filtered, normalized, transformed and scaled data
      Group <- sfData$group[, StatGroup()]
      Group <- Group[Group != "QC"]
      checkPair <- as.data.frame(table(Group)) # to valid paired test
      shiny::withProgress(
        message = "Performing statistics; Be patient",
        detail = "This may take a while...",
        value = 0.4,
        {
          if(input$statMethod == "tTest" & length(levels(as.factor(Group))) != 2) {return(NULL)}
          if(input$statMethod == "ptTest" & length(levels(as.factor(Group))) != 2) {return(NULL)}
          if(input$statMethod == "ptTest" & var(checkPair$Freq) != 0) {return(NULL)}
          if(input$statMethod == "anovaRM" & var(checkPair$Freq) != 0) {return(NULL)}
          statFC <- getFC(tFilter, Group = Group)
          statP <- getP(tFilterNormTransform, Group = Group, Method = input$statMethod)
          statP <- statP %>% dplyr::mutate_all(~ p.adjust(., method = input$pAdjMethod, n = length(.)))
          statVIP <- getVIP(tClean, Group = Group)
          statTable <- cbind.data.frame(statFC, statP, statVIP)
          rownames(statTable) <- sfData$clean$ID
          return(statTable)
          })
    }) %>%
      shiny::bindCache(sfData$filter, sfData$filterNormTransform, sfData$clean, input$statMethod, StatGroup(), input$pAdjMethod) %>%
      shiny::bindEvent(input$viewStat)

    ### On click, both only QC-filtered (raw area) and QC-filtered & transformed peak areas (processed area) are included
    combinedTable <- eventReactive(input$viewStat, {
      shiny::req(sfData$filter)
      shiny::req(sfData$clean)
      shiny::req(statTable())
      tem1 <- subset(sfData$filter, select = -ID)
      names(tem1) <- paste0("rawArea_", names(tem1))
      tem2 <- subset(sfData$clean, select = -ID)
      names(tem2) <- paste0("processedAREA_", names(tem2))
      tem3 <- cbind.data.frame(ID = sfData$clean$ID, statTable(), tem1, tem2)

      ## filter rows based on defined p-value
      if(isTRUE(as.logical(as.numeric(input$SigFilter)))){
        tem3 <- tem3 %>%
          dplyr::filter(if_any(contains("Pvalue"), ~ . < input$SigP))
        }
      rownames(tem3) <- NULL
      return(tem3)
    })

    ##(2) Show statistical result ==============================================
    output$StatResult <- DT::renderDataTable({
      shiny::validate(
        need(!is.null(sfData$clean), message = "Input data not found"),
        need(!is.null(sfData$group), message = "Meta data not found"),
        need(!is.null(statTable()), message = "More than 2 groups detected or unequal samples found for paired test; Please choose another statistical method."),
        need(!is.null(combinedTable()), message = "No statistical result")
      )
      DT::datatable(combinedTable(),
                    extensions = c("KeyTable"),
                    options = list(scrollX = TRUE,
                                   deferRender = TRUE,
                                   scroller = TRUE,
                                   keys = TRUE
                                   )
                    )
    })

    output$downloadStatTable <- downloadHandler(
      filename = "Stat_table.csv",
      content = function(file){
        write.csv(combinedTable(), file, row.names = FALSE)
      }
    )

    #(2) PCA====================================================================
    ##(1) PCA parameters--------------------------------------------------------
    PCAGroup <- reactive({
      as.character(input$PCAGroup)
    })
    observeEvent(sfData$group, {
      updateVarSelectInput(
        inputId = "PCAGroup",
        data = sfData$group,
        selected = "Group1"
      )
    })
    PCARatio <- reactive({
      if(input$PCARatio <=0){return(1)}
      input$PCARatio
    })

    ##(2) Prepare plot----------------------------------------------------------
    ### sfData$clean (QC filtered data) is used for PCA
    ## Note: combinedTablePCA could be removed.
    combinedTablePCA <- eventReactive(input$viewStat, {
      shiny::req(sfData$clean)
      shiny::req(statTable())
      tem1 <- subset(sfData$clean, select = -ID)
      names(tem1) <- paste0("rawArea_", names(tem1))
      tem2 <- cbind.data.frame(ID = sfData$clean$ID, statTable(), tem1)
      rownames(tem2) <- NULL
      return(tem2)
    })

    dataGlobal3PCA <- reactive({
      shiny::req(sfData$clean)
      tem <- transformDF(sfData$clean, Group = NULL, rowName = TRUE)
      return(tem)
    })

    PCAPlot <- reactive({
      shiny::req(dataGlobal3PCA())
      shiny::req(sfData$group)
      p <- showPCA(dataGlobal3PCA(),
                   Group = sfData$group[, PCAGroup()],
                   inx = as.numeric(input$PCAX),
                   iny = as.numeric(input$PCAY),
                   showFrame = input$PCAFrame,
                   interactive = FALSE
                   ) +
        ggplot2::theme(text = element_text(size = 16))

      if(input$PCAColor == "Default") {
        p <- p
        } else {
          p <- p +
            scale_color_brewer(palette = input$PCAColor) +
            scale_fill_brewer(palette = input$PCAColor)
        }
      return(p)
    })

    ##(3) Show PCA Result ------------------------------------------------------
    output$PCAPlot <- plotly::renderPlotly({
      shiny::validate(need(!is.null(sfData$clean), message = "Input data not found"))
      shiny::validate(need(!is.null(sfData$group), message = "Meta data not found"))
      plotly::ggplotly(PCAPlot(), tooltip = "text")
    })

    output$downloadPCA <- downloadHandler(
      filename = function(){paste("PCA", input$PCAType, sep = ".")},
      content = function(file){
        ggplot2::ggsave(file, plot = PCAPlot(), dpi = 600, width = 20, height = 20 / PCARatio(),
                        units = "cm", device = input$PCAType
                        )
      }
    )

    #3. OPLSDA==================================================================
    ##(1) OPLSDA parameters-----------------------------------------------------
    OPLSDAGroup <- reactive({
      as.character(input$OPLSDAGroup)
    })
    observeEvent(sfData$group, {
      updateVarSelectInput(
        inputId = "OPLSDAGroup",
        data = sfData$group,
        selected = "Group1"
      )
    })
    observeEvent(OPLSDAGroup(),{
      OPLSDALevels <- sfData$group[, OPLSDAGroup()]
      updateSelectInput(inputId = "OPLSDALevel1",
                        choices = levels(as.factor(OPLSDALevels[!(OPLSDALevels %in% "QC")])),
                        selected = NULL
                        )
      updateSelectInput(inputId = "OPLSDALevel2",
                        choices = rev(levels(as.factor(OPLSDALevels[!(OPLSDALevels %in% "QC")]))),
                        selected = NULL
                        )
    })
    OPLSDARatio <- reactive({
      if(input$PCARatio <= 0){return(1)}
      input$OPLSDARatio
    })

    ##(2) Prepare plot----------------------------------------------------------
    OPLSDA <- reactiveValues(feature = NULL, group = NULL)
    observe({
      shiny::req(dataGlobal3PCA())
      shiny::req(sfData$group)
      shiny::validate(need(input$OPLSDALevel1 != input$OPLSDALevel2, message = "Please select two different groups"))
      dfOPLSDA <- cbind.data.frame(dataGlobal3PCA(), Group = sfData$group[, OPLSDAGroup()]) %>%
        dplyr::filter(Group %in% c(input$OPLSDALevel1, input$OPLSDALevel2))
      OPLSDA$feature <- subset(dfOPLSDA, select = -Group)
      OPLSDA$group <- as.factor(dfOPLSDA$Group)
    })

    resultOPLSDA <- reactive({
      shiny::req(OPLSDA$feature)
      shiny::req(OPLSDA$group)
      resultOPLSDA <- tryCatch({
        ropls::opls(OPLSDA$feature,
                    OPLSDA$group,
                    log10L = FALSE,
                    scaleC = "none",
                    predI = 1,
                    permI = 20,
                    orthoI = 1, # see bug #25
                    crossvalI = min(length(OPLSDA$group), 7),
                    fig.pdfC = "none",
                    info.txtC = "none"
                    )
      },
      # For the following error:
      # Error: No model was built because the first predictive component was already not significant;
      # Select a number of predictive components of 1 if you want the algorithm to compute a model despite this.
      error = function(e){
        ropls::opls(OPLSDA$feature,
                    OPLSDA$group,
                    log10L = FALSE,
                    scaleC = "none",
                    predI = 1,
                    permI = 20,
                    orthoI = 1,
                    crossvalI = min(length(OPLSDA$group), 7),
                    fig.pdfC = "none",
                    info.txtC = "none"
                    )
      })
      return(resultOPLSDA)
    })

    OPLSDAPlot <- reactive({
      shiny::req(resultOPLSDA())
      shiny::req(OPLSDA$group)
      p <- showOPLSDA(resultOPLSDA(), Group = OPLSDA$group) +
        ggplot2::theme(text = element_text(size = 16))

      if(input$OPLSDAColor == "Default") {
        p <- p
        } else {
        p <- p +
          scale_color_brewer(palette = input$OPLSDAColor) +
          scale_fill_brewer(palette = input$OPLSDAColor)
        }
      return(p)
    })

    SPlot <- reactive({
      shiny::req(resultOPLSDA())
      shiny::req(OPLSDA$feature)
      showSplot(OPLSDA$feature, resultOPLSDA()) +
        ggplot2::theme(text = element_text(size = 16))
    })

    ##(3) Show OPLSDA Result ---------------------------------------------------
    output$OPLSDAPlot <- shiny::renderPlot({
      shiny::validate(need(!is.null(sfData$clean), message = "Input data not found"))
      shiny::validate(need(!is.null(sfData$group), message = "Meta data not found"))
      shiny::validate(need(input$OPLSDALevel1 != input$OPLSDALevel2, message = "Please select two different groups"))
      shiny::req(OPLSDAPlot())
      OPLSDAPlot()
    })

    output$SPlot <- plotly::renderPlotly({
      shiny::validate(need(!is.null(sfData$clean), message = "Input data not found"))
      shiny::validate(need(!is.null(sfData$group), message = "Meta data not found"))
      shiny::validate(need(input$OPLSDALevel1 != input$OPLSDALevel2, message = "Please select two different groups"))
      shiny::req(SPlot())
      SPlot2 <- SPlot() +
        ggplot2::geom_point(size = 1) +
        ggplot2::theme(text = element_text(size = 12))
      plotly::ggplotly(SPlot2, tooltip = "text")
    })

    output$downloadOPLSDA <- downloadHandler(
      filename = function(){paste("OPLSDA_ScorePlot", input$OPLSDAType, sep = ".")},
      content = function(file){
        ggplot2::ggsave(file, plot = OPLSDAPlot(), dpi = 600, width = 20, height = 20 / OPLSDARatio(), units = "cm", device = input$OPLSDAType)
      }
    )

    #3. Heat Map================================================================
    ##(1) Heatmap parameters----------------------------------------------------
    HMGroup <- reactive({
      as.character(input$HMGroup)
    })
    observeEvent(sfData$group, {
      updateVarSelectInput(
        inputId = "HMGroup",
        data = sfData$group,
        selected = "Group1"
      )
    })
    HMColCluster <- reactive({
      as.logical(as.numeric(input$HMColCluster))
    })
    HMRowCluster <- reactive({
      as.logical(as.numeric(input$HMRowCluster))
    })
    HMColName <- reactive({
      as.logical(as.numeric(input$HMColName))
    })
    HMRowName <- reactive({
      as.logical(as.numeric(input$HMRowName))
    })
    HMSplitCol <- reactive({
      input$HMSplitCol
    })
    HMSplitRow <- reactive({
      input$HMSplitRow
    })
    HMQCFilter <- reactive({
      as.logical(as.numeric(input$HMQCFilter))
    })
    HMType <- reactive({
      as.character(input$HMType)
    })
    HMRatio <- reactive({
      if(input$HMRatio <=0){return(1)}
      input$HMRatio
    })

    ##(2) Prepare plot----------------------------------------------------------
    ### Data for Figures that need non-transformed data: from sfData$filter
    dataGlobal3 <- reactive({
      shiny::req(combinedTable())
      tem1 <- combinedTable() %>%
        dplyr::select(ID, starts_with("rawArea_")) %>%
        dplyr::rename_with(~ gsub("rawArea_", "", .x, fixed = TRUE))
      tem2 <- tem1 %>%
        dplyr::select(-ID) %>%
        t()
      colnames(tem2) <- tem1$ID
      tem3 <- tem2 %>%
        as.data.frame() %>%
        dplyr::rename_with(make.names)
      return(tem3)
    })

    HMPlot <- reactive({
      shiny::req(dataGlobal3())
      shiny::req(sfData$group)
      HMData <- dataGlobal3() %>%
        dplyr::mutate(Group = sfData$group[, HMGroup()])
      ## Should QC be filtered?
      if(!HMQCFilter()){
        HMData <- HMData %>%
          dplyr::filter(Group != "QC")
      }
      annoRow <- dplyr::select(HMData, Group)
      rownames(annoRow) <- rownames(HMData)
      set.seed(1998)
      ComplexHeatmap::pheatmap(scale(dplyr::select(HMData, -Group), center = T, scale = T),
                               name = "ColorBar",
                               cluster_cols = HMColCluster(),
                               cluster_rows = HMRowCluster(),
                               annotation_row = annoRow,
                               show_rownames = HMRowName(),
                               show_colnames = HMColName(),
                               column_km = HMSplitCol(),
                               row_km = HMSplitRow()
                               )
    })

    ##(3) Show and download plot -----------------------------------------------
    output$HMPlot <- shiny::renderPlot({
      shiny::validate(
        need(!is.null(sfData$clean), message = "Input data not found"),
        need(!is.null(sfData$group), message = "Meta data not found")
      )
      HMPlot()
    })

    output$downloadHM <- downloadHandler(
      filename = function(){paste("Heatmap", HMType(), sep = ".")},
      content = function(file){
        switch(HMType(),
               "png" = png(file, width = 20, height = 20 / HMRatio(), units = "cm", res = 600),
               "pdf" = pdf(file, width = 20, height = 20 / HMRatio()),
               "tiff" = tiff(file, width = 20, height = 20 / HMRatio(), units = "cm", res = 600)
               )
        ComplexHeatmap::draw(HMPlot())
        dev.off()
      })

    #3. Volcano Plot------------------------------------------------------------
    VCLevel1 <- reactive({
      as.character(input$VCLevel1)
    })
    VCLevel2 <- reactive({
      as.character(input$VCLevel2)
    })
    observeEvent(StatGroup(), {
      VCLevels <- sfData$group[, StatGroup()]
      updateSelectInput(inputId = "VCLevel1",
                        choices = levels(as.factor(VCLevels[!(VCLevels %in% "QC")])),
                        selected = NULL
                        )
      updateSelectInput(inputId = "VCLevel2",
                        choices = rev(levels(as.factor(VCLevels[!(VCLevels %in% "QC")]))),
                        selected = NULL
                        )
    })
    VCFC <- reactive({
      input$VCFC
    })
    VCPvalue <- reactive({
      input$VCPvalue
    })
    VCType <- reactive({
      as.character(input$VCType)
    })
    VCRatio <- reactive({
      if(input$VCRatio <=0){return(1)}
      input$VCRatio
    })

    ##(2) Prepare plot----------------------------------------------------------
    VCPlot <- reactive({
      shiny::req(statTable())
      if(VCLevel1() == VCLevel2()) {return(NULL)}
      if(VCPvalue() > 1 | VCPvalue() < 0) {return(NULL)}
      if(VCFC() <=0) {return(NULL)}
      p <- showVolcano(result = statTable(),
                       FC = VCFC(),
                       pValue = VCPvalue(),
                       compare_group = c(VCLevel1(), VCLevel2()),
                       interactive = FALSE
                       )
      return(p)
    })

    ##(3) Show and download plot------------------------------------------------
    output$VCPlot <- plotly::renderPlotly({
      shiny::validate(
        need(!is.null(sfData$clean), message = "Input data not found"),
        need(!is.null(sfData$group), message = "Meta data not found"),
        need(VCLevel1() != VCLevel2(), message = "Please select two different groups"),
        need(VCFC() > 0, message = "Fold change value should be higher than 0"),
      )
      plotly::ggplotly(VCPlot(), tooltip = c("text"))
    })

    output$downloadVCPlot <- downloadHandler(
      filename = function(){paste("VolcanoPlot", VCType(), sep = ".")},
      content = function(file){
        ggplot2::ggsave(file, plot = VCPlot(), dpi = 600, width = 20, height = 20 / VCRatio(), units = "cm", device = VCType())
      }
    )

    #4. K-Means=================================================================
    ## (1) K-Means plot parameters----------------------------------------------
    KMGroup <- reactive({
      as.character(input$KMGroup)
    })
    observeEvent(sfData$group, {
      updateVarSelectInput(
        inputId = "KMGroup",
        data = sfData$group,
        selected = "Group1"
      )
    })
    KMCluster <- reactive({
      input$KMCluster
    })
    KMType <- reactive({
      as.character(input$KMType)
    })
    KMRatio <- reactive({
      if(input$KMRatio <=0) {return(1)}
      input$KMRatio
    })

    ##(2) Prepare plot----------------------------------------------------------
    ###(2.1)prepare k-means data
    KMdata <- reactive({
      shiny::req(dataGlobal3())
      tem <- dataGlobal3() %>%
        dplyr::mutate(Group = sfData$group[, KMGroup()]) %>%
        dplyr::filter(Group != "QC") %>%
        tidyr::pivot_longer(cols = !Group, names_to = "Metabolite", values_to = "Area") %>%
        dplyr::group_by(Group, Metabolite) %>%
        dplyr::summarize(meanArea = mean(Area), .groups = 'drop') %>%
        tidyr::pivot_wider(names_from = Group, values_from = meanArea)

      ## standardize data
      RS <- rowSums(dplyr::select(tem, -Metabolite))
      tem2 <- tem %>%
        dplyr::mutate_if(is.numeric, function(x)(x/RS))
      return(tem2)
    })

    ###(2.2) calculate k-means
    KMResult <- reactive({
      shiny::req(KMdata())
      if(KMCluster() <= 0) {return(NULL)}
      if(KMCluster() >= dim(KMdata())[1]) {return(NULL)}
      kmeans(dplyr::select(KMdata(), -Metabolite), KMCluster())
    })

    ###(2.3) calculate k-means clusters
    KMResultCluster <- reactive({
      shiny::req(KMdata())
      if(KMCluster() <= 0) {return(NULL)}
      if(KMCluster() >= dim(KMdata())[1]) {return(NULL)}
      set.seed(666)
      kmeans(dplyr::select(KMdata(), -Metabolite), centers = KMCluster())$cluster
    })

    ###(2.4) KM Table
    KMTable <- reactive({
      shiny::req(KMResultCluster())
      data_with_cust_info <- KMdata() %>%
        dplyr::mutate(clust = paste0("cluster", KMResultCluster()))
    })

    ###(2.5) K-means trend plot
    KMTrendPlot <- reactive({
      KMTable() %>%
        tidyr::pivot_longer(cols = !c(Metabolite, clust), names_to = "Group", values_to = "normArea") %>%
        dplyr::group_by(Group) %>%
        dplyr::mutate(row_num =  1:n()) %>%
        ggplot2::ggplot(aes(x =  Group , y = normArea , group = row_num)) +
        ggplot2::geom_point(alpha = 0.1) +
        ggplot2::geom_line(alpha = 0.5 , aes(col = as.character(clust))) +
        ggplot2::scale_colour_manual(values= rep(RColorBrewer::brewer.pal(9, "Paired") , 30)) +
        ggplot2::theme_bw() +
        ggplot2::theme(text = element_text(size = 14),
                       legend.position = "none",
                       axis.text.x = element_text(angle = 90 , vjust = 0.4)) +
        ggplot2::ylab("Standardized Peak Area") +
        ggplot2::facet_wrap(~clust)
    })

    ###(2.6) KM dendrogram
    # KMDG <- reactive({
    #   d1 <- data.frame(from = "origin", to = levels(as.factor(KMTable()$clust)))
    #   d2 <- data.frame(from = KMTable()$clust, to = KMTable()$Metabolite)
    #   edges <- rbind(d1, d2)
    #   vertices <- data.frame(
    #     name = unique(c(as.character(edges$from), as.character(edges$to))) ,
    #     value = 1
    #   )
    #   vertices$group <- edges$from[match(vertices$name, edges$to)]
    #   vertices$id <- NA
    #   myleaves <- which(is.na( match(vertices$name, edges$from) ))
    #   nleaves <- length(myleaves)
    #   vertices$id[myleaves] <- seq(1:nleaves)
    #   vertices$angle <- 90 - 360 * vertices$id / nleaves
    #   vertices$hjust <- ifelse(vertices$angle < -90, 1, 0)
    #   vertices$angle <- ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)
    #   mygraph <- igraph::graph_from_data_frame(edges, vertices = vertices )
    #   p <- ggraph::ggraph(mygraph, layout = 'dendrogram', circular = TRUE) +
    #     ggraph::geom_edge_diagonal(colour="grey") +
    #     ##ggraph::scale_edge_colour_distiller(palette = "RdPu") +
    #     ggraph::geom_node_point(aes(filter = leaf, x = x * 1.07, y = y*1.07, colour = group), size = 4) +
    #     ggplot2::scale_colour_manual(name = "Cluster", values= rep(RColorBrewer::brewer.pal(9, "Paired") , 30)) +
    #     ggplot2::scale_size_continuous(range = c(0.1,10) ) +
    #     ggplot2::theme_void() +
    #     ggplot2::theme(text = element_text(size = 14)) +
    #     ggplot2::expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))
    #   return(p)
    # })

    ##(3) Show and download plot----------------------------------------------------
    # output$KMDG <- shiny::renderPlot({
    #   shiny::validate(need(!is.null(sfData$data), message = "Input data not found"))
    #   shiny::validate(need(!is.null(sfData$group), message = "Meta data not found"))
    #   KMDG() + ggplot2::ggtitle("Circular Dendrogram")
    # })

    output$KMTrendPlot <- shiny::renderPlot({
      shiny::validate(need(!is.null(sfData$data), message = "Input data not found"))
      shiny::validate(need(!is.null(sfData$group), message = "Meta data not found"))
      shiny::validate(need(KMCluster() > 0, message = "Number of clusters should be higher than 0"))
      shiny::validate(need(KMCluster() < dim(KMdata())[1], message = "Input a smaller cluster group"))
      KMTrendPlot() + ggplot2::ggtitle("Metabolic Profile Clustering")
    })

    output$KMTable <- DT::renderDataTable({
      shiny::validate(need(!is.null(sfData$data), message = "Input data not found"))
      shiny::validate(need(!is.null(sfData$group), message = "Meta data not found"))
      shiny::validate(need(!is.null(KMTable()), message = "K-means cluster table not found"))
      DT::datatable(KMTable(),
                    caption = "K-means cluster table:",
                    options = list(scrollX = TRUE,
                                   deferRender = TRUE,
                                   scroller = TRUE,
                                   fixedColumns = FALSE
                                   )
                    )
    })

    # output$downloadKMDG <- downloadHandler(
    #   filename = function(){paste("KM_Dendrogram", KMType(), sep = ".")},
    #   content = function(file){
    #     ggplot2::ggsave(file, plot = KMDG(), dpi = 600, width = 20, height = 20 / KMRatio(), units = "cm", device = KMType())
    #   }
    # )

    output$downloadKMTrend <- downloadHandler(
      filename = function(){paste("KM_Lineplot", KMType(), sep = ".")},
      content = function(file){
        ggplot2::ggsave(file, plot = KMTrendPlot(), dpi = 600, width = 20, height = 20 / KMRatio(), units = "cm", device = KMType())
      }
    )

    output$downloadKMTable <- downloadHandler(
      filename = "K-means_table.csv",
      content = function(file){
        write.csv(KMTable(), file, row.names = FALSE)
      }
    )

    #5. Box Plot ===============================================================
    ##(1) Parameters -----------------------------------------------------------

    BPGroup <- reactive({
      as.character(input$BPGroup)
    })
    observeEvent(sfData$group, {
      updateVarSelectInput(
        inputId = "BPGroup",
        data = sfData$group,
        selected = "Group1"
      )
    })
    BPMetabolite <- reactive({
      as.character(input$BPMetabolite)
    })
    observeEvent(input$viewStat,{
      updateVarSelectizeInput(
        server = TRUE,
        inputId = "BPMetabolite",
        data = dataGlobal3()
      )
    })
    BPTransform <- reactive({
      as.character(input$BPTransform)
    })
    BPPlotType <- reactive({
      as.character(input$BPPlotType)
    })
    BPPlotColor <- reactive({
      as.character(input$BPPlotColor)
    })
    BPType <- reactive({
      as.character(input$BPType)
    })
    BPRatio <- reactive({
      if(input$BPRatio <=0){return(1)}
      input$BPRatio
    })
    dataGlobal3Transform <- reactive({
      switch(BPTransform(),
             none = dataGlobal3(),
             log2 = log2(dataGlobal3()),
             log10 = log10(dataGlobal3())
             )
    })
    yLegend <- reactive({
      switch(BPTransform(),
             none = "Peak Area",
             log2 = "Log2 Transformed Peak Area",
             log10 = "Log10 Transformed Peak Area"
             )
    })

    ##(2) Prepare plot----------------------------------------------------------

    BPPlot <- reactive({
      p <- dataGlobal3Transform() %>%
        dplyr::mutate(Group = sfData$group[, BPGroup()]) %>%
        dplyr::select(Group, Metabolite = BPMetabolite()) %>%
        dplyr::filter(Group != "QC") %>%
        dplyr::group_by(Group) %>%
        dplyr::mutate(MEAN = mean(Metabolite), SD = sd(Metabolite)) %>%
        ggplot2::ggplot(aes(x = Group, fill = Group)) +
        ggplot2::ylab(yLegend()) +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(paste0("Metabolite: ", BPMetabolite())) +
        ggplot2::theme(text = element_text(size = 16),
                       legend.position="none"
                       )
      ## color palette
      if(BPPlotColor() == "Default") {
        p <- p
        } else {
          p <- p + scale_fill_brewer(palette = BPPlotColor())
        }

      ## plot type
      p2 <- switch(BPPlotType(),
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
    })

    ##(3) Show and download plot----------------------------------------------------
    output$BPPlot <- shiny::renderPlot({
      shiny::validate(need(!is.null(sfData$data), message = "Input data not found"))
      shiny::validate(need(!is.null(sfData$group), message = "Meta data not found"))
      BPPlot()
    })

    output$downloadBP <- downloadHandler(
      filename = function(){paste("Box_plot", BPType(), sep = ".")},
      content = function(file){
        ggplot2::ggsave(file, plot = BPPlot(), dpi = 600, width = 20, height = 20 / BPRatio(), units = "cm", device = BPType())
      }
    )

    # #6. Correlation Network ----------------------------------------------------
    ##(1) parameters------------------------------------------------------------
    CNMetabolite <- reactive({
      as.character(input$CNMetabolite)
    })
    observeEvent(input$viewStat,{
      updateVarSelectizeInput(
        server = TRUE,
        inputId = "CNMetabolite",
        data = dataGlobal3()
      )
    })
    CNName <- reactive({
      as.character(input$CNName)
    })
    CNThreshold <- reactive({
      if(input$CNThreshold <= 0 | input$CNThreshold >1) {return(1)}
      input$CNThreshold
    })
    CNType <- reactive({
      as.character(input$CNType)
    })
    CNRatio <- reactive({
      if(input$CNRatio <=0) {return(1)}
      input$CNRatio
    })

    ##(2) Prepare plot----------------------------------------------------------
    CNCor <- reactive({
      cor(dataGlobal3())
    })

    CNPlot <- reactive({
      if(CNThreshold() <= 0) {return(NULL)}
      tem <- CNCor() %>%
        as.data.frame() %>%
        dplyr::select(Metabolite = CNMetabolite()) %>%
        dplyr::filter(Metabolite >= CNThreshold() | Metabolite <= -CNThreshold()) %>%
        as.matrix()

      selectedName <- CNMetabolite()
      if(CNName() == "IDNO"){
        colnames(tem) <- sub("_.*", "", colnames(tem))
        rownames(tem) <- sub("_.*", "", rownames(tem))
        selectedName <- sub("_.*", "", selectedName)
      }

      links <- cbind.data.frame(from = rep(selectedName, dim(tem)[1]),
                                to = rownames(tem),
                                weight = tem[, 1] ,
                                mycolor = ifelse(tem[, 1] > 0, "Pos", "Neg")
                                )

      p <- ggraph::ggraph(links, layout = "star") +
        ggraph::geom_edge_link(aes(width = abs(links$weight)), alpha = 0.5) +
        ggraph::scale_edge_width_continuous(name = "Absolute Correlation Coefficient") +
        ggraph::geom_node_point(aes(fill = links$mycolor), shape = 21, size = 15, stroke = 1) +
        ggplot2::scale_fill_manual(name = "Positive/Negative Correlation", values = c("#e78ac3", "#8da0cb")) +
        ggraph::geom_node_text(aes(label = name), size = 5, repel = FALSE) +
        ggplot2::theme_void()

      return(p)
    })

    output$CNPlot <- shiny::renderPlot({
      shiny::validate(need(!is.null(sfData$data), message = "Input data not found"))
      shiny::validate(need(!is.null(sfData$group), message = "Meta data not found"))
      CNPlot()
    })

    ##(3) Download plot-------------------------------------------------------------
    output$downloadCN <- downloadHandler(
      filename = function(){paste("Correlation_Network", CNType(), sep = ".")},
      content = function(file){
        ggplot2::ggsave(file, plot = CNPlot(), dpi = 600, width = 20, height = 20 / CNRatio(), units = "cm", device = CNType())
      })

    #7. Correlation Analysis ===================================================
    ##(1) parameters------------------------------------------------------------
    CASize <- reactive({
      input$CASize
    })
    CAThreshold <- reactive({
      input$CAThreshold
    })
    CAType <- reactive({
      as.character(input$CAType)
    })
    CARatio <- reactive({
      if(input$CNRatio <=0) {return(1)}
      input$CNRatio
    })

    ##(2) Prepare plot----------------------------------------------------------
    CAPlot <- eventReactive(input$RunCA, {
      CACor <- CNCor()
      colnames(CACor) <- sub("_.*", "", colnames(CACor))
      rownames(CACor) <- sub("_.*", "", rownames(CACor))
      p <- qgraph::qgraph(-CACor,
                          posCol = "#d01c8b",
                          negCol = "#4dac26",
                          layout = "spring",
                          vsize = CASize(),
                          minimum = CAThreshold(),
                          labels = colnames(CACor)
                          )
      return(p)
    })

    observeEvent(input$RunCA, {
      output$CAPlot <- shiny::renderPlot({
        shiny::validate(need(!is.null(sfData$data), message = "Input data not found"),
                        need(!is.null(sfData$group), message = "Meta data not found"),
                        need(CAThreshold() >=0 & CAThreshold() < 1, message = "Absolute correlation threshold value should between 0 and 1.")
                        )
        CAPlot()
      })
    })

    ##(3) Download plot---------------------------------------------------------
    output$downloadCA <- downloadHandler(
      filename = function(){paste("Correlation_Analysis", CAType(), sep = ".")},
      content = function(file){
        if(CAType() == "pdf"){
          pdf(file, width = 20, height = 20 / CNRatio())
          plot(CAPlot())
          dev.off()
        } else if(CAType() == "png"){
          png(file, width = 20, height = 20 / CNRatio(), units = "cm", res = 600)
          plot(CAPlot())
          dev.off()
        } else{
          tiff(file, width = 20, height = 20 / CNRatio(), units = "cm", res = 600)
          plot(CAPlot())
          dev.off()
        }
    })
    resultList <- list(
      dataGlobal3PCA = dataGlobal3PCA, # data for PLSDA
      OPLSDAGroup = OPLSDAGroup, # group information for PLSDA plot
      statTable = statTable, # data matrix for volcano plot
      VCGroup = StatGroup, # group information for volcano plot
      combinedTable = combinedTable,
      PCAPlot = PCAPlot,
      HMPlot = HMPlot,
      dataGlobal3Transform = dataGlobal3Transform, # data for boxplot
      BPGroup = BPGroup, # group information for boxplot
      BPTransform = BPTransform, # data transformation for boxplot
      KMTrendPlot = KMTrendPlot,
      KMTable = KMTable
      )
    return(resultList)
  })
}

## To be copied in the UI
# mod_04_viewResult_ui("04_viewResult_1")

## To be copied in the server
# mod_04_viewResult_server("04_viewResult_1")
