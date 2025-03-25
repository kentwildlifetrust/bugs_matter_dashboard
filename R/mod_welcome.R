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
    shiny::div(
      class = "welcome-page",
      shiny::div(
        class = "welcome-background-image"
      ),
      shiny::div(
        class = "welcome-page-content",
        shiny::div(
          class = "welcome-header",
          shiny::div(
            class = "welcome-message",
            "Welcome to the"
          ),
          shiny::h1(
            "Bugs Matter Dashboard"
          ),
          shiny::div(
            class = "welcome-lead",
            "In recent years, scientists, conservation organisations and the media have drawn attention to global declines in insect abundance, the consequences of which are potentially catastrophic. Invertebrates are critical to ecosystem functions and services, and without them, life on Earth would collapse. This dashboard "
          )
        )
      )
    )
  )
}

#' welcome Server Functions
#'
#' @noRd
mod_welcome_server <- function(id, conn) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pal <- leaflet::colorNumeric("Spectral", -100:100)

    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::setView(lng = -3.244293, lat = 54.350497, zoom = 6) %>%
        leaflet::addPolygons(
          data = bugsMatterDashboard::region_trends,
          color = "gray",
          weight = 1,
          opacity = 1,
          fillOpacity = 0.5,
          fillColor = ~ pal(estimate),
          popup = ~ paste0(estimate, ", ", low, " to ", high)
        ) %>%
        leaflet::addLegend(
          pal = pal,
          values = bugsMatterDashboard::region_trends$estimate,
          opacity = 1
        )
    })
  })
}

## To be copied in the UI
# mod_welcome_ui("welcome_1")

## To be copied in the server
# mod_welcome_server("welcome_1")
