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
  bslib::page_sidebar(
    fillable = TRUE,
    sidebar = bslib::sidebar(
      shiny::selectInput(
        ns("year"),
        "Year",
        choices = c("2021 to 2024", bugsMatterDashboard::years),
        selected = "All"
      ),
      shiny::selectInput(
        ns("area"),
        "Area",
        choices = c(bugsMatterDashboard::region_choices),
        selected = "uk"
      )
    ),
    shiny::div(
      style = "height: calc(100svh - 70px); display: flex; flex-direction: column;",

      shiny::div(
        class = "data-header",
        shiny::h2("Data Collection"),
        shiny::div(
          class = "data-lead",
          "Lorem consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
        ),
      ),
      # shiny::hr(class = "data-header-hr"),
      shiny::div(
        style = "display: flex; gap: var(--_padding); height: 100%;",
        shiny::div(
          style = "width: 60%; height: 100%; padding-bottom: var(--_padding);",
          bslib::card(
            height = "100%",
            full_screen = TRUE,
            bslib::card_header(
              "Routes"
            ),
            bslib::card_body(
              class = "p-0",
              leaflet::leafletOutput(ns("map"))
            )
          )
        ),
        shiny::div(
          style = "width: 40%; height: 100%; display: flex; flex-direction: column;",
          bslib::navset_card_tab(
            # full_screen = TRUE, #causes page layout to break slightly :(
            title = "Sampling effort",
            bslib::nav_panel(
              "Journeys",
              bslib::card_body(
                class = "p-0",
                plotly::plotlyOutput(
                  ns("cumulative_journeys_plot"),
                  height = "100%"
                )
              )
            ),
            bslib::nav_panel(
              "Distance travelled",
              bslib::card_body(
                class = "p-0",
                plotly::plotlyOutput(
                  ns("cumulative_distance_plot"),
                  height = "100%"
                )
              )
            ),
            bslib::nav_panel(
              "Sign ups",
              bslib::card_body(
                class = "p-0",
                plotly::plotlyOutput(
                  ns("cumulative_sign_ups"),
                  height = "100%"
                )
              )
            )
            # shiny::actionButton(
            #   ns("open_animation"),
            #   "Animate"
            # )
          ) %>%
            htmltools::tagAppendAttributes(style = "flex: 1;"),
          bslib::navset_card_tab(
            title = "Journey characteristics",
            # full_screen = TRUE,
            bslib::nav_panel(
              "Distance",
              bslib::card_body(
                class = "p-0",
                plotly::plotlyOutput(
                  ns("distance_histogram"),
                  height = "100%"
                )
              )
            ),
            bslib::nav_panel(
              "Vehicle type",
              bslib::card_body(
                class = "p-0",
                plotly::plotlyOutput(
                  ns("vehicle_bars"),
                  height = "100%"
                )
              )
            )
          ) %>%
            htmltools::tagAppendAttributes(style = "flex: 1;")
        )
      )
    )
  )
}

#' journeys_map Server Functions
#'
#' @noRd
mod_explore_journeys_server <- function(id, conn) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #---------------------journeys map-----------------------#

    output$map <- leaflet::renderLeaflet({
      map <- leaflet::leaflet(
        options = leaflet::leafletOptions(maxZoom = 12)
      ) %>%
        leaflet::addProviderTiles("CartoDB.Positron")

      region_query <- if (tolower(input$area) %in% c("uk")) {
        "SELECT id, geom FROM bugs_matter.regions_app
            WHERE id IN (11, 10, 12, 4, 6, 7, 1, 2, 8, 9, 5, 3);"
      } else if (tolower(input$area) %in% c("england")) {
        "SELECT id, geom FROM bugs_matter.regions_app
            WHERE id IN (4, 6, 1, 2, 8, 9, 5, 3, 7);"
      } else {
        glue::glue_data_sql(
          list(
            id = as.numeric(input$area)
          ),
          "SELECT id, geom FROM bugs_matter.regions_app
              WHERE id = {id};",
          .con = conn
        )
      }

      region_boundaries <- region_query %>%
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


      url_param <- if (tolower(input$area) %in% c("uk", "england") & tolower(input$year) == "2021 to 2024") {
        tolower(input$area)
      } else if (tolower(input$area) %in% c("uk", "england") & tolower(input$year) != "2021 to 2024") {
        paste0(input$area, "/years/", input$year)
      } else if (tolower(input$year) == "2021 to 2024") {
        paste0("regions/", input$area)
      } else if (tolower(input$year) != "2021 to 2024") {
        paste0("regions/", input$area, "/years/", input$year)
      }

      vector_grid_js <- sprintf(
        "
        function(el, x) {
          // Leaflet.VectorGrid is loaded in header
          var vectorGrid = L.vectorGrid.protobuf(
            'http://localhost:3000/tiles/%s/{z}/{x}/{y}.pbf', {
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
        url_param
      )

      map %>%
        htmlwidgets::onRender(vector_grid_js)
    })

    #---------------------cumulative frequency plot-----------------------
    output$cumulative_journeys_plot <- plotly::renderPlotly({
      min_date <- if (input$year == "2021 to 2024") {
        "2021-05-01"
      } else if (input$year == "2024") {
        sprintf("%s-04-01", input$year)
      } else {
        sprintf("%s-05-01", input$year)
      }
      max_date <- if (input$year == "2021 to 2024") {
        "2024-11-01"
      } else if (input$year == "2024") {
        sprintf("%s-11-01", input$year)
      } else {
        sprintf("%s-10-01", input$year)
      }
      counts <- "
        WITH daily_counts AS (
        SELECT
          j.end::DATE AS date,
          COUNT(*) AS daily_count
        FROM bugs_matter.journeys_server j
        WHERE region_id IN ({region_ids*})
        GROUP BY j.end::DATE
      ), date_bounds AS (
        SELECT
          {min_date}::DATE AS min_date,
          {max_date}::DATE AS max_date
      ),  -- No need to select FROM the table here
      all_dates AS (
        SELECT generate_series(min_date, max_date, interval '1 day')::date AS date
        FROM date_bounds
      )
      SELECT
        all_dates.date,
        COALESCE(daily_counts.daily_count, 0) AS daily_count,
        SUM(COALESCE(daily_counts.daily_count, 0)) OVER (
          ORDER BY all_dates.date
          ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
        ) AS cumulative_count
      FROM all_dates
      LEFT JOIN daily_counts ON all_dates.date = daily_counts.date
      ORDER BY all_dates.date;" %>%
        glue::glue_data_sql(
          list(
            region_ids = get_region_ids(input$area),
          min_date = min_date,
          max_date = max_date
          ),
          .,
          .con = conn
        ) %>%
        DBI::dbGetQuery(conn, .) %>%
        dplyr::mutate(date = as.Date(date))

      plotly::plot_ly(
        type = "scatter",
        mode = "lines"
      ) %>%
        plotly::add_trace(
          showlegend = FALSE,
          y = counts$cumulative_count,
          x = counts$date,
          line = list(
            color = "#147331",
            width = 3
          )
        ) %>%
        plotly::layout(
          dragmode = FALSE,
          yaxis = list(title = "Total number of journeys"),
          xaxis = list(title = "Date")
        ) %>%
        plotly::config(
          displayModeBar = FALSE
        )
    })


    #---------------------distance travelled------------------------#
      output$cumulative_distance_plot <- plotly::renderPlotly({
      min_date <- if (input$year == "2021 to 2024") {
        "2021-05-01"
      } else if (input$year == "2024") {
        sprintf("%s-04-01", input$year)
      } else {
        sprintf("%s-05-01", input$year)
      }
      max_date <- if (input$year == "2021 to 2024") {
        "2024-11-01"
      } else if (input$year == "2024") {
        sprintf("%s-11-01", input$year)
      } else {
        sprintf("%s-10-01", input$year)
      }
      counts <- "
        WITH daily_counts AS (
        SELECT
          j.end::DATE AS date,
          SUM(j.distance) AS daily_count
        FROM bugs_matter.journeys_server j
        WHERE region_id IN ({region_ids*})
        GROUP BY j.end::DATE
      ), date_bounds AS (
        SELECT
          {min_date}::DATE AS min_date,
          {max_date}::DATE AS max_date
      ),  -- No need to select FROM the table here
      all_dates AS (
        SELECT generate_series(min_date, max_date, interval '1 day')::date AS date
        FROM date_bounds
      )
      SELECT
        all_dates.date,
        COALESCE(daily_counts.daily_count, 0) AS daily_count,
        ROUND((SUM(COALESCE(daily_counts.daily_count, 0)) OVER (
          ORDER BY all_dates.date
          ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
        ))::NUMERIC) AS cumulative_count
      FROM all_dates
      LEFT JOIN daily_counts ON all_dates.date = daily_counts.date
      ORDER BY all_dates.date;" %>%
        glue::glue_data_sql(
          list(
            region_ids = get_region_ids(input$area),
          min_date = min_date,
          max_date = max_date
          ),
          .,
          .con = conn
        ) %>%
        DBI::dbGetQuery(conn, .) %>%
        dplyr::mutate(date = as.Date(date))

      plotly::plot_ly(
        type = "scatter",
        mode = "lines"
      ) %>%
        plotly::add_trace(
          showlegend = FALSE,
          y = counts$cumulative_count,
          x = counts$date,
          line = list(
            color = "#147331",
            width = 3
          )
        ) %>%
        plotly::layout(
          dragmode = FALSE,
          yaxis = list(title = "Total distance travelled (miles)"),
          xaxis = list(title = "Date")
        ) %>%
        plotly::config(
          displayModeBar = FALSE
        )
    })

    #---------------signed up users----------------------------------#
     output$cumulative_sign_ups <- plotly::renderPlotly({
      min_date <- if (input$year == "2021 to 2024") {
        "2021-05-01"
      } else if (input$year == "2024") {
        sprintf("%s-04-01", input$year)
      } else {
        sprintf("%s-05-01", input$year)
      }
      max_date <- if (input$year == "2021 to 2024") {
        "2024-11-01"
      } else if (input$year == "2024") {
        sprintf("%s-11-01", input$year)
      } else {
        sprintf("%s-10-01", input$year)
      }
      counts <- "
        WITH daily_counts AS (
        SELECT
          u.sign_up_date::DATE AS date,
          COUNT(*) AS daily_count
        FROM bugs_matter.users_app u
        WHERE region_id IN ({region_ids*})
        GROUP BY u.sign_up_date::DATE
      ), date_bounds AS (
        SELECT
          {min_date}::DATE AS min_date,
          {max_date}::DATE AS max_date
      ),
      all_dates AS (
        SELECT generate_series(min_date, max_date, interval '1 day')::date AS date
        FROM date_bounds
      )
      SELECT
        all_dates.date,
        COALESCE(daily_counts.daily_count, 0) AS daily_count,
        SUM(COALESCE(daily_counts.daily_count, 0)) OVER (
          ORDER BY all_dates.date
          ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
        ) AS cumulative_count
      FROM all_dates
      LEFT JOIN daily_counts ON all_dates.date = daily_counts.date
      ORDER BY all_dates.date;" %>%
        glue::glue_data_sql(
          list(
            region_ids = get_region_ids(input$area),
          min_date = min_date,
          max_date = max_date
          ),
          .,
          .con = conn
        ) %>%
        DBI::dbGetQuery(conn, .) %>%
        dplyr::mutate(date = as.Date(date))

      plotly::plot_ly(
        type = "scatter",
        mode = "lines"
      ) %>%
        plotly::add_trace(
          showlegend = FALSE,
          y = counts$cumulative_count,
          x = counts$date,
          line = list(
            color = "#147331",
            width = 3
          )
        ) %>%
        plotly::layout(
          dragmode = FALSE,
          yaxis = list(title = "Total registered users"),
          xaxis = list(title = "Date")
        ) %>%
        plotly::config(
          displayModeBar = FALSE
        )
    })

    #---------------------distance histogram -----------------------#

    output$distance_histogram <- plotly::renderPlotly({
      "SELECT j.distance
                  FROM bugs_matter.journeys_server j
                  WHERE j.region_id IN ({region_ids*}) AND j.year IN ({years*});" %>%
        glue::glue_data_sql(
          list(
            region_ids = get_region_ids(input$area),
            years = get_years(input$year)
          ),
          .,
          .con = conn
        ) %>%
        DBI::dbGetQuery(conn, .) %>%
        dplyr::pull("distance") %>%
      plotly::plot_ly(
        x = .,
        type = "histogram",
        marker = list(color = "#147331")
      ) %>%
      plotly::layout(
        dragmode = FALSE,
        yaxis = list(title = "Number of Journeys"),
        xaxis = list(title = "Distance (miles)")
      ) %>%
      plotly::config(
        displayModeBar = FALSE
      )
    })

    #---------------------vehicle type bars -----------------------#

    output$vehicle_bars <- plotly::renderPlotly({
      "WITH all_vehicles AS(
          SELECT DISTINCT (j.vehicle_cl) AS vehicle_cl
          FROM bugs_matter.journeys_server j
        )
        SELECT all_vehicles.vehicle_cl, COALESCE(COUNT(*), 0) AS count
        FROM all_vehicles
        LEFT JOIN bugs_matter.journeys_server j ON all_vehicles.vehicle_cl = j.vehicle_cl
        WHERE j.region_id IN ({region_ids*}) AND j.year IN ({years*})
        GROUP BY j.vehicle_cl, all_vehicles.vehicle_cl;" %>%
        glue::glue_data_sql(
          list(
            region_ids = get_region_ids(input$area),
            years = get_years(input$year)
          ),
          .,
          .con = conn
        ) %>%
        DBI::dbGetQuery(conn, .) %>%
      plotly::plot_ly(
        data = .,
        type = "bar",
        x = ~vehicle_cl,
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
