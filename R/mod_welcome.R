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
          shiny::br(),
          shiny::p(
            class = "welcome-lead",
            "Here, you can keep up-to-date with the findings of Bugs Matter, the global citizen science
            survey of ‘bug splats’ on vehicle number plates to monitor flying insect abundance. You can explore
            where and how many journeys have been recorded, and see information about the lengths of journeys,
            and the types of vehicles used. You can explore trends in the number of bugs splatted by location and over different time periods.
            You can also find information on the numbers of users in different parts of the world and how many journeys have been recorded by our top recorders!"
          )
        ),
        shiny::div(
          class = "welcome-body",
          shiny::div(
            class = "welcome-body-text",
            shiny::p(
              "The Bugs Matter citizen science survey uses an innovative
method for large-scale indiscriminate monitoring of flying
insect populations. Citizen scientists record the number
of insect splats on their vehicle number plates following
a journey, having first removed any residual insects from
previous journeys. It has the potential to provide an efficient,
standardised and scalable approach to monitor trends in
insect abundance across local, regional and global scales. The sampling technique is based on the ‘windscreen
phenomenon’, a term given to the anecdotal observation that fewer insect splats appear on
the windscreens of cars now compared to a decade or
several decades ago. These observations, which have also
been reported from empirical data (Møller, 2019), have been
interpreted as an indicator of major global declines in insect
abundance."
            )
          ),
          shiny::div(
            class = "welcome-figures",
            shiny::div(
              class = "row welcome-value-row",
              shiny::div(
                class = "col-sm",
                bslib::value_box(
                  class = "welcome-value-box",
                  title = "Kilometres travelled",
                  value = format(123456, big.mark = ","),
                  showcase = shiny::icon("fa fa-route fa-solid"),
                  theme = bslib::value_box_theme(
                    bg = "#3C91E6",
                    fg = "#FFF"
                  ),
                  showcase_layout = "top right"
                )
              ),
              shiny::div(
                class = "col-sm",
                bslib::value_box(
                  class = "welcome-value-box",
                  title = "Years sampled",
                  value = "2021 - 2024",
                  showcase = shiny::icon("fa fa-calendar-days fa-solid"),
                  theme = bslib::value_box_theme(
                    bg = "#58A732",
                    fg = "#FFF"
                  ),
                  showcase_layout = "top right"
                )
              ),
              shiny::div(
                class = "col-sm",
                bslib::value_box(
                  class = "welcome-value-box",
                  title = "Change in splat rate over 3 years",
                  value = "- 55%",
                  showcase = shiny::icon("fa fa-chart-line-down fa-solid"),
                  theme = bslib::value_box_theme(
                    bg = "#F46036",
                    fg = "#FFF"
                  ),
                  showcase_layout = "top right"
                )
              )
            ),
            br(),
            bslib::card(
              min_height = 500,
              bslib::card_header(
                "Change in Count Rates, 2021 to 2024"
              ),
              bslib::card_body(
                class = "p-0",
                leaflet::leafletOutput(ns("map"), height = "100%")
              )
            )
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
