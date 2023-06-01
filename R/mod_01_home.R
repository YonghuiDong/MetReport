#' 01_home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_01_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 10, includeMarkdown(app_sys("app/www/landing.md")))

))}

#' 01_home Server Functions
#'
#' @noRd
mod_01_home_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

})}

## To be copied in the UI
# mod_01_home_ui("01_home_1")

## To be copied in the server
# mod_01_home_server("01_home_1")
