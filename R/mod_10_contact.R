#' 10_contact UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @import markdown
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_10_contact_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 10, includeMarkdown(app_sys("app/www/contact.md")))
      #column(width = 10, includeHTML(app_sys("app/www/contact.html")))
      )
  )
}

#' 10_contact Server Functions
#'
#' @noRd
mod_10_contact_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_10_contact_ui("10_contact_1")

## To be copied in the server
# mod_10_contact_server("10_contact_1")
