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

mod_participation_ui <- function(id) {
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
        selected = "All years",
        width = 150
      ),
      shiny::selectInput(
        ns("regions_or_users"),
        "Regions or Users",
        choices = c(
          "Regions",
          "Users"
        ),
        selected = "Regions",
        width = 150
      )
    ),
    shiny::actionButton(
      ns("next_page"),
      shiny::span(
        style = "color: black;",
        "Get involved",
        shiny::tags$i(
          class = "fa fa-up-right-from-square"
        ),
      ),
      class = "btn-primary m-2",
      style = "flex-grow: 0; height: min-content; margin-bottom: 1rem !important;"
    )
  )

  table_card <- bslib::navset_card_pill(
    title = "Leaderboard",
    height = "100%",
    full_screen = TRUE,
    bslib::card_body(
      max_height = "calc(100svh - 330px)",
      padding = c(15, 15, 0, 15),
      shinycssloaders::withSpinner(
        reactable::reactableOutput(ns("leaderboard"), height = "100%")
      )
    )
  )

  sign_ups_panel <- bslib::nav_panel(
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
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(
          ns("cumulative_distance"),
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
          "Lorem ipsum.",
          placement = "bottom"
        )
      )
    ) %>%
      shiny::tagAppendAttributes(style = "position: relative;")
  )

  journeys_panel <- bslib::nav_panel(
    "Journeys",
    bslib::card_body(
      padding = c(15, 0, 10, 0),
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(
          ns("cumulative_journeys"),
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
          "Lorem ipsum.",
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
          "Participation",
          shiny::actionLink(
              ns("participation_info"),
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
          table_card
        ),
        bslib::navset_card_tab(
          title = "Cumulative Participation",
          # full_screen = TRUE,
          journeys_panel,
          distance_panel,
          sign_ups_panel
        ) %>%
          htmltools::tagAppendAttributes(style = "flex: 1; margin-bottom: 0; min-height: 600px;")
      )
  )
}

#' journeys_map Server Functions
#'
#' @noRd
mod_participation_server <- function(id, conn, next_page) {
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

    years <- reactive({
      if (nchar(input$year) > 4) {
        return(bugsMatterDashboard::years)
      } else {
        as.numeric(input$year)
      }
    })
    #---------------------leaderboard_table-----------------------#

    output$leaderboard <- reactable::renderReactable({
      if (input$regions_or_users == "Regions") {
        data <- "
        WITH sign_ups AS (
          SELECT s.region_code,
          COUNT(*) AS n_sign_ups
          FROM sign_ups.with_region s
          WHERE s.year in ({years*})
          GROUP BY s.region_code
        )
          SELECT r.name,
          r.country_name,
          COALESCE(s.n_sign_ups, 0) AS n_sign_ups,
          COALESCE(COUNT(*), 0) AS n_journeys,
          ROUND(COALESCE(SUM(j.distance), 0)::NUMERIC) AS distance
          FROM journeys.processed j
          LEFT JOIN ref.regions r ON j.region_code = r.code
          LEFT JOIN sign_ups s ON j.region_code = s.region_code
          WHERE j.year in ({years*})
          GROUP BY r.name, s.n_sign_ups, r.country_name
          ORDER BY n_journeys DESC;
        " %>%
          glue::glue_data_sql(
            list(
              years = years()
            ),
            .,
            .con = conn
          ) %>%
            DBI::dbGetQuery(conn, .) %>%
            dplyr::mutate(rank = 1:nrow(.)) %>%
            dplyr::select(rank, name, country_name, n_journeys, distance, n_sign_ups)
      } else {
        data <- "
        SELECT 'User in ' || r.name AS name,
        EXTRACT(YEAR FROM s.user_created_at)::INT AS sign_up_year,
        COALESCE(COUNT(j.id), 0) AS n_journeys,
        ROUND(COALESCE(SUM(j.distance), 0)::NUMERIC) AS distance
        FROM sign_ups.with_region s
        LEFT JOIN journeys.processed j ON s.id = j.user_id AND j.year in ({years*})
        LEFT JOIN ref.regions r ON s.region_code = r.code
        GROUP BY s.user_username, r.name, EXTRACT(YEAR FROM s.user_created_at)
        ORDER BY n_journeys DESC
        LIMIT 20;
        " %>%
          glue::glue_data_sql(
            list(
              years = years()
            ),
            .,
            .con = conn
          ) %>%
            DBI::dbGetQuery(conn, .) %>%
            dplyr::mutate(rank = 1:nrow(.)) %>%
            dplyr::select(rank, name, sign_up_year, n_journeys, distance)
      }

      # Configure table columns based on view type
      if (input$regions_or_users == "Regions") {
        reactable::reactable(
          data,
          columns = list(
            rank = reactable::colDef(
              name = "#",
              width = 50,
              align = "center"
            ),
            name = reactable::colDef(
              name = "Region"
            ),
            country_name = reactable::colDef(
              name = "Country"
            ),
            n_sign_ups = reactable::colDef(
              name = "Sign-ups",
              format = reactable::colFormat(separators = TRUE)
            ),
            n_journeys = reactable::colDef(
              name = "Journeys",
              format = reactable::colFormat(separators = TRUE)
            ),
            distance = reactable::colDef(
              name = "Distance (km)",
              format = reactable::colFormat(separators = TRUE)
            )
          ),
          defaultColDef = reactable::colDef(
            align = "left"
          ),
          sortable = TRUE,
          filterable = TRUE,
          defaultPageSize = 100,
          height = "100%",
          showSortable = TRUE
        )
      } else {
        reactable::reactable(
          data,
          columns = list(
            rank = reactable::colDef(
              name = "#",
              width = 50,
              align = "center"
            ),
            name = reactable::colDef(
              name = "Username"
            ),
            sign_up_year = reactable::colDef(
              name = "Sign-up Year"
            ),
            n_journeys = reactable::colDef(
              name = "Journeys",
              format = reactable::colFormat(separators = TRUE)
            ),
            distance = reactable::colDef(
              name = "Distance (km)",
              format = reactable::colFormat(separators = TRUE)
            )
          ),
          defaultColDef = reactable::colDef(
            align = "left"
          ),
          sortable = TRUE,
          filterable = TRUE,
          defaultPageSize = 100,
          showSortable = TRUE
        )
      }
    })

          #---------------------cumulative number of journeys-----------------------
      output$cumulative_journeys <- plotly::renderPlotly({
        years_vec <- if (nchar(input$year) > 4) {
          bugsMatterDashboard::years
        } else {
          as.numeric(input$year)
        }

        start_year <- min(years_vec, na.rm = TRUE)
        end_year <- max(years_vec, na.rm = TRUE)

        min_date <- sprintf("%s-04-01", start_year)
        if (end_year == as.numeric(format(Sys.Date(), "%Y"))) {
          max_date <- format(Sys.Date(), "%Y-%m-%d")
        } else {
          max_date <- sprintf("%s-10-30", end_year)
        }

        counts <- "
          WITH daily_counts AS (
            SELECT
              j.end_timestamp::DATE AS date,
              COUNT(*) AS daily_count
            FROM journeys.processed j
            WHERE EXTRACT(YEAR FROM j.end_timestamp) IN ({years*})
            GROUP BY j.end_timestamp::DATE
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
              years = years_vec,
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
            name = "Total",
            y = counts$cumulative_count,
            x = counts$date,
            line = list(
              color = "#147331",
              width = 3
            ),
            showlegend = FALSE
          ) %>%
          plotly::layout(
            dragmode = FALSE,
            yaxis = list(title = "Total number of journeys"),
            xaxis = list(
              title = "Date"
            )
          ) %>%
          plotly::config(
            displayModeBar = FALSE
          )
      })


    #---------------------distance travelled------------------------#
     output$cumulative_distance <- plotly::renderPlotly({
        years_vec <- if (nchar(input$year) > 4) {
          bugsMatterDashboard::years
        } else {
          as.numeric(input$year)
        }

        start_year <- min(years_vec, na.rm = TRUE)
        end_year <- max(years_vec, na.rm = TRUE)

        min_date <- sprintf("%s-04-01", start_year)
        if (end_year == as.numeric(format(Sys.Date(), "%Y"))) {
          max_date <- format(Sys.Date(), "%Y-%m-%d")
        } else {
          max_date <- sprintf("%s-10-30", end_year)
        }

        counts <- "
          WITH daily_counts AS (
            SELECT
              j.end_timestamp::DATE AS date,
              SUM(j.distance) AS daily_distance
            FROM journeys.processed j
            WHERE EXTRACT(YEAR FROM j.end_timestamp) IN ({years*})
            GROUP BY j.end_timestamp::DATE
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
            COALESCE(daily_counts.daily_distance, 0) AS daily_distance,
            SUM(COALESCE(daily_counts.daily_distance, 0)) OVER (
              ORDER BY all_dates.date
              ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
            ) AS cumulative_distance
          FROM all_dates
          LEFT JOIN daily_counts ON all_dates.date = daily_counts.date
          ORDER BY all_dates.date;" %>%
          glue::glue_data_sql(
            list(
              years = years_vec,
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
            name = "Total",
            y = counts$cumulative_distance,
            x = counts$date,
            line = list(
              color = "#147331",
              width = 3
            ),
            showlegend = FALSE
          ) %>%
          plotly::layout(
            dragmode = FALSE,
            yaxis = list(title = "Total distance travelled (km)"),
            xaxis = list(
              title = "Date"
            )
          ) %>%
          plotly::config(
            displayModeBar = FALSE
          )
      })


    #---------------signed up users----------------------------------#
    output$cumulative_sign_ups <- plotly::renderPlotly({
      years_vec <- if (nchar(input$year) > 4) {
        bugsMatterDashboard::years
      } else {
        as.numeric(input$year)
      }

      start_year <- min(years_vec, na.rm = TRUE)
      end_year <- max(years_vec, na.rm = TRUE)

      min_date <- sprintf("%s-04-01", start_year)
      if (end_year == as.numeric(format(Sys.Date(), "%Y"))) {
        max_date <- format(Sys.Date(), "%Y-%m-%d")
      } else {
        max_date <- sprintf("%s-10-30", end_year)
      }

      counts <- "
        WITH daily_counts AS (
          SELECT
            s.user_created_at::DATE AS date,
            COUNT(*) AS daily_count
          FROM sign_ups.raw s
          WHERE EXTRACT(YEAR FROM s.user_created_at) IN ({years*})
          GROUP BY s.user_created_at::DATE
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
            years = years_vec,
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
          name = "Total",
          y = counts$cumulative_count,
          x = counts$date,
          line = list(
            color = "#147331",
            width = 3
          ),
          showlegend = FALSE
        ) %>%
        plotly::layout(
          dragmode = FALSE,
          yaxis = list(title = "Total registered users"),
          xaxis = list(
            title = "Date"
          )
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
