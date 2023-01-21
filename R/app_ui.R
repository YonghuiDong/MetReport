#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import markdown
#' @importFrom  shinydashboard sidebarMenu menuItem menuSubItem dashboardBody tabItems tabItem
#' @import shinydashboardPlus
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    dashboardPage(

      ## Header ----------------------------------------------------------------
      header = dashboardHeader(
        title = strong("MetReport")
      ),

      ## Sidebar ---------------------------------------------------------------
      sidebar = dashboardSidebar(

        sidebarMenu(
          style = "position: fixed; overflow: visible;",
          id = "sidebarmenu",
          menuItem(text = strong("Home"), tabName = "home", icon = icon("home")),
          hr(),
          menuItem(text = strong("Upload Data"), tabName = "uploadData", icon = icon("upload")),
          hr(),
          menuItem(text = strong("Preprocess Data"), tabName = "preprocessData", icon = icon("adjust")),
          hr(),
          menuItem(text = strong("Statistics"), tabName = "viewResult", icon = icon("chart-bar")),
          hr(),
          menuItem(text = strong("Download Report"), tabName = "downloadReport", icon = icon("download")),
          hr(),
          menuItem(text = strong("Widgets"), tabName = "widgets", icon = icon("tools"),
                   menuSubItem(text = strong("Randomizer"), tabName = "randomizer", icon = icon("bolt")),
                   menuSubItem(text = strong("Solvent"), tabName = "solvent", icon = icon("bolt")),
                   menuSubItem(text = strong("LC Tool"), tabName = "lcTool", icon = icon("bolt")),
                   menuSubItem(text = strong("MS Tool"), tabName = "massTool", icon = icon("bolt"))
          ),
          hr(),
          menuItem(text = strong("Contact"), tabName = "contact", icon = icon("envelope"))
        )
      ),

      ## Body ------------------------------------------------------------------
      body = dashboardBody(
        tabItems(
          tabItem("home", mod_01_home_ui("01_home_1")),
          tabItem("uploadData", mod_02_uploadData_ui("02_uploadData_1")),
          tabItem(tabName = "preprocessData", mod_03_preprocess_ui("03_preprocess_1")),
          tabItem(tabName = "viewResult", mod_04_viewResult_ui("04_viewResult_1")),
          tabItem(tabName = "downloadReport",  mod_05_downloadReport_ui("05_downloadReport_1")),
          # tabItem(tabName = "randomizer",  source("ui-tab-randomizer.R", local = TRUE)$value),
          # tabItem(tabName = "solvent",  source("ui-tab-solvent.R", local = TRUE)$value),
          # tabItem(tabName = "lcTool",  source("ui-tab-lcTool.R", local = TRUE)$value),
          # tabItem(tabName = "massTool",  source("ui-tab-massTool.R", local = TRUE)$value),
          tabItem(tabName = "contact",  mod_10_contact_ui("10_contact_1"))
        )
      ),

      ## Skin ------------------------------------------------------------------
      controlbar = dashboardControlbar(collapsed = TRUE, skinSelector()),

      ## Footer --------------------------------------------------------------------
      footer = dashboardFooter(
        left = "Weizmann Institute of Science",
        right = "Copyright (C) 2022"
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'MetReport'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
  # add css file
  tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
}

