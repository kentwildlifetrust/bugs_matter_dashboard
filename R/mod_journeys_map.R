#' journeys_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
library(magrittr)
. <- ""

mod_journeys_map_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_sidebar(
    sidebar = bslib::sidebar(
      shinyjs::disabled(shiny::selectInput(
        ns("year"),
        "Year",
        choices = bugsMatterDashboard::years,
        selected = "2024"
      ))
    ),
    shiny::div(
      class = "data-header",
      shiny::h2("Data Collection"),
      shiny::div(
        class = "data-lead",
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
      ),
    ),
    shiny::hr(class = "data-header-hr"),
    bslib::layout_column_wrap(
      bslib::card(
        bslib::card_header(
          "Map"
        ),
      ),
      shiny::div(
        bslib::card(
          bslib::card_header(
            "Cumulative journeys"
          ),
          shiny::actionButton(
            ns("open_animation"),
            "Animate"
          )
        ),
        bslib::navset_card_pill(
          title = "Journey characteristics",
          bslib::nav_panel(
            "Length"
          ),
          bslib::nav_panel(
            "Vehicle type"
          )
        )
      )
    )
  )
}

#' journeys_map Server Functions
#'
#' @noRd
mod_journeys_map_server <- function(id, conn) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # output$map <- leaflet::renderLeaflet({
    #   map <- leaflet::leaflet() %>%
    #     leaflet::addProviderTiles("CartoDB.Positron") %>%
    #     leaflet::setView(lng = -3.244293, lat = 54.350497, zoom = 6)
    # })

    # shiny::observeEvent(input$year, {
    #   map <- leaflet::leafletProxy(ns("map")) %>%
    #     leaflet::clearGroup(input$map_groups)
    #   i <- which(bugsMatterDashboard::years %in% as.numeric(input$year))
    #   for (j in seq_len(length(bugsMatterDashboard::journeys[i][[1]]$data))) {
    #     map <- map %>%
    #       leaflet::hideGroup(as.character(bugsMatterDashboard::journeys[i][[1]]$dates))
    #     map <- map %>%
    #       leaflet::addPolylines(
    #         data = bugsMatterDashboard::journeys[i][[1]]$data[[j]]$lines,
    #         color = "#147331",
    #         group = bugsMatterDashboard::journeys[i][[1]]$data[[j]]$group,
    #         weight = 3,
    #         opacity = 0.2
    #       )
    #   }
    #   shinyjs::enable("year")
    #   shinyjs::enable("date")
    # })

    # values <- shiny::reactiveValues()

    # shiny::observe({
    #   values$p <- plotly::plot_ly(
    #     type = "scatter",
    #     mode = "lines"
    #   ) %>%
    #     plotly::add_trace(
    #       y = c(0, 0),
    #       x = as.Date(
    #         c(
    #           sprintf("%s-04-29", input$year),
    #           sprintf("%s-04-30", input$year)
    #         )
    #       ),
    #       line = list(
    #         color = "#147331",
    #         width = 3
    #       )
    #     ) %>%
    #     plotly::layout(
    #       yaxis = list(range = c(0, max(bugsMatterDashboard::cumulative_count$cumulative_count)))
    #     )
    # })

    # output$plot <- plotly::renderPlotly({
    #   values$p
    # })

    # dates_in_year <- shiny::reactive({
    #   year_index <- which(bugsMatterDashboard::years %in% as.numeric(input$year))
    #   bugsMatterDashboard::journeys[year_index][[1]]$dates
    # })

    # prev_date <- shiny::reactiveVal(as.Date("2025-04-30"))

    # shiny::observeEvent(input$year,
    #   {
    #     prev_date(sprintf("%s-04-30", input$year))
    #     shiny::updateSliderInput(
    #       session,
    #       "date",
    #       min = as.Date(sprintf("%s-05-01", input$year)),
    #       max = as.Date(sprintf("%s-10-01", input$year)),
    #       value = as.Date(sprintf("%s-05-01", input$year))
    #     )
    #   },
    #   ignoreInit = TRUE
    # )

    # shiny::observeEvent(c(input$date, dates_in_year()),
    #   {
    #     if (format(input$date, "%Y") != input$year) return()
    #     showing_dates <- dates_in_year()[which(dates_in_year() <= input$date)]
    #     map <- leaflet::leafletProxy(ns("map"))

    #     dates_to_show <- setdiff(
    #       as.character(showing_dates),
    #       input$map_groups
    #     )
    #     dates_to_hide <- input$map_groups[!input$map_groups %in% showing_dates]

    #     map <- map %>%
    #       leaflet::hideGroup(dates_to_hide) %>%
    #       leaflet::showGroup(dates_to_show)

    #     if (prev_date() <= input$date) {
    #       points_to_add <- bugsMatterDashboard::cumulative_count %>%
    #         dplyr::filter(date <= input$date & date > prev_date())
    #       plotly::plotlyProxy("plot", session, deferUntilFlush = FALSE) %>%
    #         plotly::plotlyProxyInvoke("extendTraces", list(
    #           x = list(as.list(points_to_add$date)),
    #           y = list(as.list(points_to_add$cumulative_count))
    #         ), list(1))
    #     } else {
    #       points_to_add <- bugsMatterDashboard::cumulative_count %>%
    #         dplyr::filter(
    #           date <= input$date &
    #           date >= as.Date(sprintf("%s-05-01", input$year))
    #         )

    #       plotly::plotlyProxy("plot", session, deferUntilFlush = FALSE) %>%
    #         plotly::plotlyProxyInvoke("deleteTraces", list(1)) %>% # remove old line
    #         plotly::plotlyProxyInvoke("addTraces", list(
    #           list(
    #             x = points_to_add$date,
    #             y = points_to_add$cumulative_count,
    #             type = "scatter",
    #             mode = "lines"
    #           )
    #         ))
    #     }
    #     prev_date(input$date)
    #   },
    #   ignoreInit = TRUE
    # )
  })
}

## To be copied in the UI
# mod_journeys_map_ui("journeys_map_1")

## To be copied in the server
# mod_journeys_map_server("journeys_map_1")
