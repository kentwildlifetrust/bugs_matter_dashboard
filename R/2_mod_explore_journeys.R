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

mod_explore_journeys_ui <- function(id) {
  ns <- shiny::NS(id)

  data_control_row <- shiny::div(
    class = "data-control-row",
    shiny::div(
      class = "data-controls",
      shiny::selectInput(
        ns("year"),
        "Year",
        choices = c(
          "All years",
          bugsMatterDashboard::years
        ),
        selected = "2025",
        width = 150
      ),
      shiny::selectInput(
        ns("region"),
        "Region",
        choices = bugsMatterDashboard::region_choices,
        selected = "world",
        width = 250
      )
    ),
    shiny::actionButton(
      ns("next_page"),
      shiny::span(
        style = "color: black;",
        "Analyse trends",
        shiny::tags$i(
          class = "fa fa-arrow-right"
        ),
      ),
      class = "btn-primary m-2",
      style = "flex-grow: 0; height: min-content; margin-bottom: 1rem !important;"
    )
  )

  map_card <- bslib::navset_card_pill(
    title = "Routes",
    height = "100%",
    full_screen = TRUE,
    bslib::card_body(
      padding = c(0, 0, 0, 0),
      min_height = 400,
      leaflet::leafletOutput(ns("map"), height = "100%")
    )
  )

  temperature_panel <- bslib::nav_panel(
    "Temperature",
    bslib::card_body(
      padding = c(15, 0, 10, 0),
      min_height = 400,
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(
          ns("temperature_histogram"),
          height = "100%"
        )
      ),
      div(
        style = "position: absolute; top: 5px; right: 20px;",
        bslib::popover(
          shiny::actionLink(
            ns("temperature_histogram_info"),
            shiny::tags$i(class = "fa fa-info-circle"),
            style = "font-size: 1.5rem;"
          ),
          "Lorem ipsum.",
          placement = "bottom"
        )
      )
    ) %>%
      shiny::tagAppendAttributes(style = "position: relative;")
  )

  distance_panel <- bslib::nav_panel(
    "Distance",
    bslib::card_body(
      padding = c(15, 0, 10, 0),
      min_height = 400,
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(
          ns("distance_histogram"),
          height = "100%"
        )
      ),
      div(
        style = "position: absolute; top: 5px; right: 20px;",
        bslib::popover(
          shiny::actionLink(
            ns("distance_histogram_info"),
            shiny::tags$i(class = "fa fa-info-circle"),
            style = "font-size: 1.5rem;"
          ),
          "This histogram plot shows how many journeys of different lengths were recorded. There tends to be more shorter journeys than longer journeys.",
          placement = "bottom"
        )
      )
    ) %>%
      shiny::tagAppendAttributes(style = "position: relative;")
  )

  elevation_panel <- bslib::nav_panel(
    "Elevation",
    bslib::card_body(
      padding = c(15, 0, 10, 0),
      min_height = 400,
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(
          ns("elevation_histogram"),
          height = "100%"
        )
      ),
      div(
        style = "position: absolute; top: 5px; right: 20px;",
        bslib::popover(
          shiny::actionLink(
            ns("elevation_histogram_info"),
            shiny::tags$i(class = "fa fa-info-circle"),
            style = "font-size: 1.5rem;"
          ),
          "This histogram plot shows how many journeys were recorded at different elevation levels. The distribution shows the range of elevations where journeys typically occur.",
          placement = "bottom"
        )
      )
    ) %>%
      shiny::tagAppendAttributes(style = "position: relative;")
  )

  day_of_year_panel <- bslib::nav_panel(
    "Day of Year",
    bslib::card_body(
      padding = c(15, 0, 10, 0),
      min_height = 400,
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(
          ns("day_of_year_histogram"),
          height = "100%"
        )
      ),
      div(
        style = "position: absolute; top: 5px; right: 20px;",
        bslib::popover(
          shiny::actionLink(
            ns("day_of_year_histogram_info"),
            shiny::tags$i(class = "fa fa-info-circle"),
            style = "font-size: 1.5rem;"
          ),
          "This histogram plot shows how many journeys were recorded on different days of the year. The distribution shows seasonal patterns in journey activity.",
          placement = "bottom"
        )
      )
    ) %>%
      shiny::tagAppendAttributes(style = "position: relative;")
  )

  vehicle_type_panel <- bslib::nav_panel(
    "Vehicle type",
    bslib::card_body(
      padding = c(15, 0, 10, 0),
      min_height = 400,
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(
          ns("vehicle_bars"),
          height = "100%"
        )
      ),
      div(
        style = "position: absolute; top: 5px; right: 20px;",
        bslib::popover(
          shiny::actionLink(
            ns("vehicle_bars_info"),
            shiny::tags$i(class = "fa fa-info-circle"),
            style = "font-size: 1.5rem;"
          ),
          "This bar plot shows how many journeys were recorded in different vehicle types. Most journeys are recorded in cars.",
          placement = "bottom"
        )
      )
    ) %>%
      shiny::tagAppendAttributes(style = "position: relative;")
  )

  bslib::page(
    shiny::div(
      class = "data-header",
      shiny::h2(
        "Journeys",
        shiny::actionLink(
            ns("data_collection_info"),
            shiny::tags$i(class = "fa fa-info-circle")
        )
      )
    ),
    shiny::hr(class = "data-hr"),
    data_control_row,
    # shiny::hr(class = "data-header-hr"),
    shiny::div(
      class = "cards-container",
      shiny::div(
        style = "height: 100%; padding-bottom: var(--_padding); min-width: 350px;",
        map_card
      ),
      bslib::navset_card_pill(
        title = "Journey characteristics",
        # full_screen = TRUE,
        temperature_panel,
        distance_panel,
        elevation_panel,
        day_of_year_panel,
        vehicle_type_panel
      ) %>%
        htmltools::tagAppendAttributes(style = "flex: 1; margin-bottom: 0;")
    )
  )
}

#' journeys_map Server Functions
#'
#' @noRd
mod_explore_journeys_server <- function(id, conn, next_page) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(input$data_collection_info, {
      shiny::showModal(
        shiny::modalDialog(
          shiny::div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            shiny::h3("About Data Collection"),
            shiny::actionLink(
              ns("close_modal"),
              shiny::tags$i(class = "fa fa-xmark-circle"),
              style = "font-size: 1.5rem; margin-bottom: .5rem;"
            )
          ),
          shiny::p(
              "Prior to commencing a journey, citizen scientists clean the front number plate of their vehicle to
              remove any residual insects. The app requests a checkbox confirmation that the number plate has been
              cleaned. Upon starting a journey, citizen scientists tap a button in the app to begin recording the
              journey route using the mobile device’s GPS. This provides crucial data on the length, duration,
              location, and average speed of the journey. Insects are then sampled when they collide with the number
              plate throughout the duration of a journey. Upon completing a journey, citizen scientists tap a
              button in the app to finish recording the journey route. They record the number of insect splats on
              the front number plate of their vehicle. The journey route, the number of insect splats, and a photograph
              of the number plate are submitted via the app. Citizen scientists are asked to participate only on
              essential journeys and not to make journeys specifically to take part in the survey."
            ),
            shiny::p(
              "Prior to the analysis, some steps are taken to clean the data and remove outliers. Journeys with
              GPS errors are removed from the dataset. These errors are caused by a drop-out of background tracking
              due to GPS signal being lost by the device, and they appear as long straight lines between distant
                  locations. Very short journeys, very fast journeys, very slow journeys, or journeys with over 500
                  insect splats are removed from the dataset.  Finally, all journeys during which rainfall occurred
                  were omitted from the dataset due to the high chance that rainfall could dislodge insects from
                  number plates and create inaccurate splat counts."
            ),
            easyClose = TRUE,
            footer = NULL
        )
      )
    })

    shiny::observeEvent(input$close_modal, {
      shiny::removeModal()
    })

    

    region_codes <- reactive({
      #3 character values are country codes
      if (input$region == "world") {
        bugsMatterDashboard::regions %>%
          dplyr::pull(code)
      } else if (nchar(input$region) == 3) {
        bugsMatterDashboard::regions %>%
          dplyr::filter(country_code == input$region) %>%
          dplyr::pull(code)
      } else {
        input$region
      }
    })

    #---------------------journeys map-----------------------#

    output$map <- leaflet::renderLeaflet({
      map <- leaflet::leaflet(
        options = leaflet::leafletOptions(maxZoom = 12, zoomControl = FALSE)
      ) %>%
        leaflet::setMaxBounds(-12 - 10, 30 - 10, 35 + 10, 61 + 10) %>%
        leaflet::addProviderTiles("CartoDB.Positron")

      #get regions for the selected country
      country_code <- bugsMatterDashboard::regions %>%
        dplyr::filter(code %in% region_codes()) %>%
        dplyr::pull(country_code)

      region_boundaries <- "
      SELECT code, geom
      FROM ref.regions
      WHERE code in ({region_codes*})
      " %>%
        glue::glue_data_sql(
          list(
            region_codes = region_codes()
          ),
          .,
          .con = conn
        ) %>%
        DBI::dbGetQuery(conn, .) %>%
        dplyr::mutate(geom = sf::st_as_sfc(.$geom)) %>%
        sf::st_as_sf(crs = 4326)

      bbox <- sf::st_bbox(region_boundaries) %>%
        as.list()

      map <- map %>%
        leaflet::addPolygons(
          data = region_boundaries,
          weight = 0,
          fillOpacity = 0.1,
          color = "#75DA40"
        ) %>%
        leaflet::fitBounds(
          lng1 = bbox$xmin,
          lng2 = bbox$xmax,
          lat1 = bbox$ymin,
          lat2 = bbox$ymax
        )

      region_param <- if (input$region == "world") {
        "world"
      } else if (nchar(input$region) == 3) {
        paste0("countries/", input$region)
      } else {
        paste0("regions/", input$region)
      }

      year_param <- if (nchar(input$year) == 4) {
        paste0("/years/", input$year)
      } else {
        ""
      }

      vector_grid_js <- sprintf(
        "
        function(el, x) {
          // Leaflet.VectorGrid is loaded in header
          var vectorGrid = L.vectorGrid.protobuf(
            '%s/tiles/%s%s/{z}/{x}/{y}.pbf', {
              vectorTileLayerStyles: {
                  lines: function(properties, zoom) {
                      return {
                          weight: 2,
                          color: '#1D763B',
                          opacity: 0.3
                      };
                  }
              },
              maxZoom: 12, // Use your max zoom level
              // Use canvas renderer for better performance with many features
              rendererFactory: L.canvas.tile
          });

          // Add the vector grid layer to the map
          vectorGrid.addTo(this);
        }
      ",
        ifelse(golem::app_dev(), "http://127.0.0.1:5000", "https://journeys.bugsmatter.org"),
        region_param,
        year_param
      )

      map %>%
        htmlwidgets::onRender(vector_grid_js)
    })

    years <- reactive({
      if (nchar(input$year) > 4) {
        return(bugsMatterDashboard::years)
      } else {
        as.numeric(input$year)
      }
    })


    #---------------------distance histogram -----------------------#

    output$distance_histogram <- plotly::renderPlotly({
      "SELECT distance
        FROM journeys.processed
        WHERE region_code IN ({region_codes*}) AND year IN ({years*});" %>%
        glue::glue_data_sql(
          list(
            region_codes = region_codes(),
            years = years()
          ),
          .,
          .con = conn
        ) %>%
        DBI::dbGetQuery(conn, .) %>%
        dplyr::pull("distance") %>%
      plotly::plot_ly(
        x = .,
        type = "histogram",
        marker = list(color = "#147331"),
        name = ""
      ) %>%
      plotly::layout(
        dragmode = FALSE,
        yaxis = list(title = "Number of Journeys"),
        xaxis = list(title = "Distance (km)")
      ) %>%
      plotly::config(
        displayModeBar = FALSE
      )
    })

    #---------------------temperature histogram -----------------------#

    output$temperature_histogram <- plotly::renderPlotly({
      "SELECT temperature
        FROM journeys.processed
        WHERE region_code IN ({region_codes*}) AND year IN ({years*});" %>%
        glue::glue_data_sql(
          list(
            region_codes = region_codes(),
            years = years()
          ),
          .,
          .con = conn
        ) %>%
        DBI::dbGetQuery(conn, .) %>%
        dplyr::pull("temperature") %>%
      plotly::plot_ly(
        x = .,
        type = "histogram",
        marker = list(color = "#147331"),
        name = ""
      ) %>%
      plotly::layout(
        dragmode = FALSE,
        yaxis = list(title = "Number of Journeys"),
        xaxis = list(title = "Temperature (Day Mean °C)")
      ) %>%
      plotly::config(
        displayModeBar = FALSE
      )
    })

    #---------------------elevation histogram -----------------------#

    output$elevation_histogram <- plotly::renderPlotly({
      "SELECT elevation
        FROM journeys.processed
        WHERE region_code IN ({region_codes*}) AND year IN ({years*});" %>%
        glue::glue_data_sql(
          list(
            region_codes = region_codes(),
            years = years()
          ),
          .,
          .con = conn
        ) %>%
        DBI::dbGetQuery(conn, .) %>%
        dplyr::pull("elevation") %>%
      plotly::plot_ly(
        x = .,
        type = "histogram",
        marker = list(color = "#147331"),
        name = ""
      ) %>%
      plotly::layout(
        dragmode = FALSE,
        yaxis = list(title = "Number of Journeys"),
        xaxis = list(title = "Mean Elevation (m)")
      ) %>%
      plotly::config(
        displayModeBar = FALSE
      )
    })

    #---------------------day of year histogram -----------------------#

    output$day_of_year_histogram <- plotly::renderPlotly({
      "SELECT day_of_year
        FROM journeys.processed
        WHERE region_code IN ({region_codes*}) AND year IN ({years*});" %>%
        glue::glue_data_sql(
          list(
            region_codes = region_codes(),
            years = years()
          ),
          .,
          .con = conn
        ) %>%
        DBI::dbGetQuery(conn, .) %>%
        dplyr::pull("day_of_year") %>%
      plotly::plot_ly(
        x = .,
        type = "histogram",
        marker = list(color = "#147331"),
        name = ""
      ) %>%
      plotly::layout(
        dragmode = FALSE,
        yaxis = list(title = "Number of Journeys"),
        xaxis = list(title = "Day of Year")
      ) %>%
      plotly::config(
        displayModeBar = FALSE
      )
    })

    #---------------------vehicle type bars -----------------------#

    output$vehicle_bars <- plotly::renderPlotly({
      "WITH all_vehicles(vehicle_class) AS (
          SELECT v FROM (VALUES ('Other'), ('Car'), ('HCV'), ('LCV')) AS t(v)
        )
        SELECT
          CASE WHEN all_vehicles.vehicle_class = 'HCV' THEN 'HGV' ELSE all_vehicles.vehicle_class END AS vehicle_class,
          COALESCE(COUNT(j.vehicle_class), 0) AS count
        FROM all_vehicles
        LEFT JOIN journeys.processed j
          ON all_vehicles.vehicle_class = j.vehicle_class
         AND j.region_code IN ({region_codes*})
         AND j.year IN ({years*})
        GROUP BY all_vehicles.vehicle_class
        ORDER BY vehicle_class DESC;" %>%
        glue::glue_data_sql(
          list(
            region_codes = region_codes(),
            years = years()
          ),
          .,
          .con = conn
        ) %>%
        DBI::dbGetQuery(conn, .) %>%
        dplyr::mutate(
          vehicle_class = factor(vehicle_class, levels = rev(.$vehicle_class))
        ) %>%
      plotly::plot_ly(
        data = .,
        type = "bar",
        x = ~vehicle_class,
        y = ~count,
        marker = list(color = "#147331")
      ) %>%
      plotly::layout(
        dragmode = FALSE,
        yaxis = list(title = "Number of Journeys"),
        xaxis = list(title = "Vehicle Type")
      ) %>%
      plotly::config(
        displayModeBar = FALSE
      )
    })

    shiny::observeEvent(input$next_page, {
      next_page(next_page() + 1)
    })


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
