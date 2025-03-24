#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_welcome_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_fillable(
    bslib::card(
        leaflet::leafletOutput(ns("map"), height = "100%")
    )
  )
}
    
#' welcome Server Functions
#'
#' @noRd 
mod_welcome_server <- function(id, conn){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$map <- leaflet::renderLeaflet({
    
    })
 
  })
}
    
## To be copied in the UI
# mod_welcome_ui("welcome_1")
    
## To be copied in the server
# mod_welcome_server("welcome_1")
