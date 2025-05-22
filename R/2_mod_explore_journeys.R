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
  bslib::page(
      shiny::div(
        class = "data-header",
        shiny::h2(
          "Data Collection",
          shiny::actionLink(
              ns("data_collection_info"),
              shiny::tags$i(class = "fa fa-info-circle")
          )
        ),
         shiny::actionButton(
          ns("next_page"),
          shiny::span(
            style = "color: white;",
            "Analyse trends",
            shiny::tags$i(
              class = "fa fa-arrow-right"
            ),
          ),
          class = "btn-primary m-2",
          style = "flex-grow: 0; height: min-content; margin-bottom: 1rem !important;"
        )
      ),
      shiny::hr(class = "data-hr"),
      shiny::div(
        class = "data-control-row",
        shiny::div(
          class = "data-controls",
          shiny::selectInput(
            ns("year"),
            "Year",
            choices = c("2021 to 2024", bugsMatter::years),
            selected = "All",
            width = 150
          ),
          shiny::selectInput(
            ns("area"),
            "Area",
            choices = c(bugsMatter::region_choices),
            selected = "uk",
            width = 250
          )
        )
      ),
      # shiny::hr(class = "data-header-hr"),
      shiny::div(
        class = "cards-container",
        shiny::div(
          style = "height: 100%; padding-bottom: var(--_padding); min-width: 350px;",
          bslib::card(
            height = "100%",
            full_screen = TRUE,
            bslib::card_header(
              "Routes"
            ),
            bslib::card_body(
              padding = c(0, 0, 0, 0),
              shinycssloaders::withSpinner(
                leaflet::leafletOutput(ns("map"), height = "100%")
              )
            )
          )
        ),
        shiny::div(
          style = "height: 100%; display: flex; flex-direction: column; min-width: 350px;",
          bslib::navset_card_tab(
            # full_screen = TRUE, #causes page layout to break slightly :(
            title = "Sampling effort",
            bslib::nav_panel(
              "Journeys",
              bslib::card_body(
                padding = c(15, 0, 10, 0),
                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(
                    ns("cumulative_journeys_plot"),
                    height = "100%"
                  )
                ),
                div(
                  style = "position: absolute; top: 5px; right: 20px;",
                  bslib::popover(
                    shiny::actionLink(
                      ns("cumulative_journeys_info"),
                      shiny::tags$i(class = "fa fa-info-circle"),
                      style = "font-size: 1.5rem;"
                    ),
                    "This line plot shows the cumulative number of journeys recorded. Over time, more and more journeys are recorded.",
                    placement = "bottom"
                  )
                )
              ) %>%
                shiny::tagAppendAttributes(style = "position: relative;")
            ),
            bslib::nav_panel(
              "Distance travelled",
              bslib::card_body(
                padding = c(15, 0, 10, 0),
                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(
                    ns("cumulative_distance_plot"),
                    height = "100%"
                  )
                ),
                div(
                  style = "position: absolute; top: 5px; right: 20px;",
                  bslib::popover(
                    shiny::actionLink(
                      ns("cumulative_distance_info"),
                      shiny::tags$i(class = "fa fa-info-circle"),
                      style = "font-size: 1.5rem;"
                    ),
                    "This line plot shows the cumulative distance travelled during sampling journeys. Over time, total sampling distance increases.",
                    placement = "bottom"
                  )
                )
              ) %>%
                shiny::tagAppendAttributes(style = "position: relative;")
            ),
            bslib::nav_panel(
              "Sign ups",
              bslib::card_body(
                padding = c(15, 0, 10, 0),
                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(
                    ns("cumulative_sign_ups"),
                    height = "100%"
                  )
                ),
                div(
                  style = "position: absolute; top: 5px; right: 20px;",
                  bslib::popover(
                    shiny::actionLink(
                      ns("cumulative_sign_ups_info"),
                      shiny::tags$i(class = "fa fa-info-circle"),
                      style = "font-size: 1.5rem;"
                    ),
                    "This line plot shows the increase in registered citizen scientists over time.",
                    placement = "bottom"
                  )
                )
              ) %>%
                shiny::tagAppendAttributes(style = "position: relative;")
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
                padding = c(15, 0, 10, 0),
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
            ),
            bslib::nav_panel(
              "Vehicle type",
              bslib::card_body(
                padding = c(15, 0, 10, 0),
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
          ) %>%
            htmltools::tagAppendAttributes(style = "flex: 1; margin-bottom: 0;")
        )
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

    #---------------------journeys map-----------------------#

    output$map <- leaflet::renderLeaflet({
      map <- leaflet::leaflet(
        options = leaflet::leafletOptions(maxZoom = 12, zoomControl = FALSE)
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
            'https://bugs-matter-vector-tiles.azurewebsites.net/tiles/%s/{z}/{x}/{y}.pbf', {
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
        mode = "lines",
        name = ""
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
        mode = "lines",
        name = ""
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
        mode = "lines",
        name = ""
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
        marker = list(color = "#147331"),
        name = ""
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
    #   i <- which(bugsMatter::years %in% as.numeric(input$year))
    #   for (j in seq_len(length(bugsMatter::journeys[i][[1]]$data))) {
    #     map <- map %>%
    #       leaflet::hideGroup(as.character(bugsMatter::journeys[i][[1]]$dates))
    #     map <- map %>%
    #       leaflet::addPolylines(
    #         data = bugsMatter::journeys[i][[1]]$data[[j]]$lines,
    #         color = "#147331",
    #         group = bugsMatter::journeys[i][[1]]$data[[j]]$group,
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
    #       yaxis = list(range = c(0, max(bugsMatter::cumulative_count$cumulative_count)))
    #     )
    # })

    # output$plot <- plotly::renderPlotly({
    #   values$p
    # })

    # dates_in_year <- shiny::reactive({
    #   year_index <- which(bugsMatter::years %in% as.numeric(input$year))
    #   bugsMatter::journeys[year_index][[1]]$dates
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
    #       points_to_add <- bugsMatter::cumulative_count %>%
    #         dplyr::filter(date <= input$date & date > prev_date())
    #       plotly::plotlyProxy("plot", session, deferUntilFlush = FALSE) %>%
    #         plotly::plotlyProxyInvoke("extendTraces", list(
    #           x = list(as.list(points_to_add$date)),
    #           y = list(as.list(points_to_add$cumulative_count))
    #         ), list(1))
    #     } else {
    #       points_to_add <- bugsMatter::cumulative_count %>%
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
