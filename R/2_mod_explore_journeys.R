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

  data_header <- shiny::div(
    class = "data-header",
    shiny::h2(
      shiny::span(id = ns("journeys_title"), "Journeys"),
      shiny::actionLink(
          ns("data_collection_info"),
          shiny::tags$i(class = "fa fa-info-circle"),
          `aria-label` = "Information about data collection"
      )
    ),
    shiny::actionButton(
      ns("next_page"),
      shiny::span(
        style = "font-weight: 400;",
        "Analyse trends",
        shiny::tags$i(
          class = "fa fa-arrow-right"
        ),
      ),
      class = "btn-primary m-2",
      style = "flex-grow: 0; height: min-content; margin-bottom: 1rem !important;",
      `aria-label` = "Navigate to Analyse trends page"
    )
  )


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
      # uiOutput(ns("slider_input"))
    ),
    div(
      class = "data-control-value-row",
      bslib::value_box(
        class = "data-control-value-box",
        title = "Number of journeys",
        value = shiny::textOutput(ns("count")),
        theme = bslib::value_box_theme(
          bg = "#3C91E6",
          fg = "#000"
        ),
        showcase_layout = "top right"
      ),
      bslib::value_box(
        class = "data-control-value-box",
        title = "Distance sampled",
        value = shiny::textOutput(ns("distance")),
        theme = bslib::value_box_theme(
          bg = "#72DB41",
          fg = "#000"
        ),
        showcase_layout = "top right"
      )
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
            style = "font-size: 1.5rem;",
            `aria-label` = "Information about temperature histogram"
          ),
          "This histogram plot shows how many journeys were recorded in different ambient air temperatures.",
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
            style = "font-size: 1.5rem;",
            `aria-label` = "Information about distance histogram"
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
            style = "font-size: 1.5rem;",
            `aria-label` = "Information about elevation histogram"
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
            style = "font-size: 1.5rem;",
            `aria-label` = "Information about day of year histogram"
          ),
          "This histogram plot shows how many journeys were recorded on different days of the year. The distribution shows seasonal patterns in journey activity.",
          placement = "bottom"
        )
      )
    ) %>%
      shiny::tagAppendAttributes(style = "position: relative;")
  )

  speed_panel <- bslib::nav_panel(
    "Speed",
    bslib::card_body(
      padding = c(15, 0, 10, 0),
      min_height = 400,
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(
          ns("speed_histogram"),
          height = "100%"
        )
      ),
      div(
        style = "position: absolute; top: 5px; right: 20px;",
        bslib::popover(
          shiny::actionLink(
            ns("speed_histogram_info"),
            shiny::tags$i(class = "fa fa-info-circle"),
            style = "font-size: 1.5rem;",
            `aria-label` = "Information about speed histogram"
          ),
          "This histogram plot shows how many journeys were recorded at different average speeds.",
          placement = "bottom"
        )
      )
    ) %>%
      shiny::tagAppendAttributes(style = "position: relative;")
  )


  time_of_day_panel <- bslib::nav_panel(
    "Time of day",
    bslib::card_body(
      padding = c(15, 0, 10, 0),
      min_height = 400,
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(
          ns("time_of_day_histogram"),
          height = "100%"
        )
      ),
      div(
        style = "position: absolute; top: 5px; right: 20px;",
        bslib::popover(
          shiny::actionLink(
            ns("time_of_day_histogram_info"),
            shiny::tags$i(class = "fa fa-info-circle"),
            style = "font-size: 1.5rem;",
            `aria-label` = "Information about time of day histogram"
          ),
          "This histogram plot shows how many journeys were recorded at different times of the day. ",
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
            style = "font-size: 1.5rem;",
            `aria-label` = "Information about vehicle type bar chart"
          ),
          "This bar plot shows how many journeys were recorded in different vehicle types. Most journeys are recorded in cars.",
          placement = "bottom"
        )
      )
    ) %>%
      shiny::tagAppendAttributes(style = "position: relative;")
  )

  land_cover_panel <- bslib::nav_panel(
    "Land cover",
    bslib::card_body(
      padding = c(15, 0, 10, 0),
      min_height = 400,
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(
          ns("land_cover_violin"),
          height = "100%"
        )
      ),
      div(
        style = "position: absolute; top: 5px; right: 20px;",
        bslib::popover(
          shiny::actionLink(
            ns("land_cover_violin_info"),
            shiny::tags$i(class = "fa fa-info-circle"),
            style = "font-size: 1.5rem;",
            `aria-label` = "Information about land cover violin plot"
          ),
          "This violin plot shows the distribution of land cover proportions for each journey. The width represents the number of journeys that had that encountered that proportion of habitat. Horizontal lines show the mean habitat.",
          placement = "bottom"
        )
      )
    ) %>%
      shiny::tagAppendAttributes(style = "position: relative;")
  )

  bslib::page(
    data_header,
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
        speed_panel,
        time_of_day_panel,
        vehicle_type_panel,
        land_cover_panel
      ) %>%
        htmltools::tagAppendAttributes(style = "flex: 1; margin-bottom: 0;")
    )
  )
}

#' journeys_map Server Functions
#'
#' @noRd
mod_explore_journeys_server <- function(id, conn, next_page, email_filter, organisation_choices) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(email_filter(), {
      title <- if (is.null(email_filter())) {
        "Journeys"
      } else {
        paste0("Journeys - ", names(organisation_choices[organisation_choices == email_filter()]))
      }
      shinyjs::html(id = "journeys_title", html = title)
    }, ignoreNULL = FALSE)

    shiny::observeEvent(input$data_collection_info, {
      shiny::showModal(
        shiny::modalDialog(
          shiny::div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            shiny::h3("About Data Collection"),
            shiny::actionLink(
              ns("close_modal"),
              shiny::tags$i(class = "fa fa-xmark-circle"),
              style = "font-size: 1.5rem; margin-bottom: .5rem;",
              `aria-label` = "Close modal dialog"
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

    years <- reactive({
      if (nchar(input$year) > 4) {
        return(bugsMatterDashboard::years)
      } else {
        as.numeric(input$year)
      }
    })


    #---------------------stats boxes-----------------------#
    output$distance <- shiny::renderText({
      query <- if (is.null(email_filter())) {
        "SELECT ROUND(SUM(distance)::NUMERIC, 0) AS distance FROM journeys.processed
        WHERE region_code IN ({region_codes*}) AND year IN ({years*});" %>%
          glue::glue_data_sql(
            list(
              region_codes = region_codes(),
              years = years()
            ),
            .,
            .con = conn
          )
      } else {
        "SELECT ROUND(SUM(j.distance)::NUMERIC, 0) AS distance
        FROM journeys.processed j
        LEFT JOIN sign_ups.raw s ON j.user_id = s.id
        WHERE j.region_code IN ({region_codes*}) AND j.year IN ({years*}) AND s.user_email LIKE {email_pattern};" %>%
          glue::glue_data_sql(
            list(
              region_codes = region_codes(),
              years = years(),
              email_pattern = paste0("%", email_filter(), "%")
            ),
            .,
            .con = conn
          )
      }
      query %>%
        DBI::dbGetQuery(conn, .) %>%
        dplyr::pull("distance") %>%
        format(big.mark = ",") %>%
        paste("km")
    })

    output$count <- shiny::renderText({
      query <- if (is.null(email_filter())) {
        "SELECT COUNT(*) AS count FROM journeys.processed
        WHERE region_code IN ({region_codes*}) AND year IN ({years*});;" %>%
          glue::glue_data_sql(
            list(
              region_codes = region_codes(),
              years = years()
            ),
            .,
            .con = conn
          )
      } else {
        "SELECT COUNT(*) AS count FROM journeys.processed j
        LEFT JOIN sign_ups.raw s ON j.user_id = s.id
        WHERE j.region_code IN ({region_codes*}) AND j.year IN ({years*}) AND s.user_email LIKE {email_pattern};" %>%
          glue::glue_data_sql(
            list(
              region_codes = region_codes(),
              years = years(),
              email_pattern = paste0("%", email_filter(), "%")
            ),
            .,
            .con = conn
          )
      }
      query %>%
          DBI::dbGetQuery(conn, .) %>%
          dplyr::pull("count") %>%
          format(big.mark = ",")
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


    #---------------------distance histogram -----------------------#

    output$distance_histogram <- plotly::renderPlotly({
      query <- if (is.null(email_filter())) {
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
          )
      } else {
        "SELECT j.distance
        FROM journeys.processed j
        LEFT JOIN sign_ups.raw s ON j.user_id = s.id
        WHERE j.region_code IN ({region_codes*}) AND j.year IN ({years*}) AND s.user_email LIKE {email_pattern};" %>%
          glue::glue_data_sql(
            list(
              region_codes = region_codes(),
              years = years(),
              email_pattern = paste0("%", email_filter(), "%")
            ),
            .,
            .con = conn
          )
      }

      query %>%
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
      query <- if (is.null(email_filter())) {
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
          )
      } else {
        "SELECT j.temperature
        FROM journeys.processed j
        LEFT JOIN sign_ups.raw s ON j.user_id = s.id
        WHERE j.region_code IN ({region_codes*}) AND j.year IN ({years*}) AND s.user_email LIKE {email_pattern};" %>%
          glue::glue_data_sql(
            list(
              region_codes = region_codes(),
              years = years(),
              email_pattern = paste0("%", email_filter(), "%")
            ),
            .,
            .con = conn
          )
      }

      query %>%
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
      query <- if (is.null(email_filter())) {
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
          )
      } else {
        "SELECT j.day_of_year
        FROM journeys.processed j
        LEFT JOIN sign_ups.raw s ON j.user_id = s.id
        WHERE j.region_code IN ({region_codes*}) AND j.year IN ({years*}) AND s.user_email LIKE {email_pattern};" %>%
          glue::glue_data_sql(
            list(
              region_codes = region_codes(),
              years = years(),
              email_pattern = paste0("%", email_filter(), "%")
            ),
            .,
            .con = conn
          )
      }
      query %>%
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

    #---------------------speed histogram -----------------------#

    output$speed_histogram <- plotly::renderPlotly({
      query <- if (is.null(email_filter())) {
        "SELECT avg_speed_kmh
        FROM journeys.processed
        WHERE region_code IN ({region_codes*}) AND year IN ({years*});" %>%
          glue::glue_data_sql(
            list(
              region_codes = region_codes(),
              years = years()
            ),
            .,
            .con = conn
          )
      } else {
        "SELECT j.avg_speed_kmh
        FROM journeys.processed j
        LEFT JOIN sign_ups.raw s ON j.user_id = s.id
        WHERE j.region_code IN ({region_codes*}) AND j.year IN ({years*}) AND s.user_email LIKE {email_pattern};" %>%
          glue::glue_data_sql(
            list(
              region_codes = region_codes(),
              years = years(),
              email_pattern = paste0("%", email_filter(), "%")
            ),
            .,
            .con = conn
          )
      }
      query %>%
        DBI::dbGetQuery(conn, .) %>%
        dplyr::pull("avg_speed_kmh") %>%
      plotly::plot_ly(
        x = .,
        type = "histogram",
        marker = list(color = "#147331"),
        name = ""
      ) %>%
      plotly::layout(
        dragmode = FALSE,
        yaxis = list(title = "Number of Journeys"),
        xaxis = list(title = "Average speed (km/h)")
      ) %>%
      plotly::config(
        displayModeBar = FALSE
      )
    })

    #---------------------time of day histogram -----------------------#

    output$time_of_day_histogram <- plotly::renderPlotly({
      query <- if (is.null(email_filter())) {
        "SELECT time_of_day
        FROM journeys.processed
        WHERE region_code IN ({region_codes*}) AND year IN ({years*});" %>%
          glue::glue_data_sql(
            list(
              region_codes = region_codes(),
              years = years()
            ),
            .,
            .con = conn
          )
      } else {
        "SELECT j.time_of_day
        FROM journeys.processed j
        LEFT JOIN sign_ups.raw s ON j.user_id = s.id
        WHERE j.region_code IN ({region_codes*}) AND j.year IN ({years*}) AND s.user_email LIKE {email_pattern};" %>%
          glue::glue_data_sql(
            list(
              region_codes = region_codes(),
              years = years(),
              email_pattern = paste0("%", email_filter(), "%")
            ),
            .,
            .con = conn
          )
      }
      query %>%
        DBI::dbGetQuery(conn, .) %>%
        dplyr::pull("time_of_day") %>%
        # Build POSIXct timestamps using a fixed date so Plotly treats as date/time
        { paste0("2000-01-01 ", ., ":00") } %>%
        { as.POSIXct(., format = "%Y-%m-%d %H:%M:%S", tz = "UTC") } %>%
      plotly::plot_ly(
        x = .,
        type = "histogram",
        marker = list(color = "#147331"),
        name = ""
      ) %>%
      plotly::layout(
        dragmode = FALSE,
        yaxis = list(title = "Number of Journeys"),
        xaxis = list(
          title = "Time of day",
          type = "date",
          tickformat = "%H:%M"
        )
      ) %>%
      plotly::config(
        displayModeBar = FALSE
      )
    })

    #---------------------vehicle type bars -----------------------#

    output$vehicle_bars <- plotly::renderPlotly({
      query <- if (is.null(email_filter())) {
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
          )
      } else {
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
        LEFT JOIN sign_ups.raw s ON j.user_id = s.id
        WHERE s.user_id LIKE {email_pattern}
        GROUP BY all_vehicles.vehicle_class
        ORDER BY vehicle_class DESC;" %>%
          glue::glue_data_sql(
            list(
              region_codes = region_codes(),
              years = years(),
              email_pattern = paste0("%", email_filter(), "%")
            ),
            .,
            .con = conn
          )
      }
      query %>%
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

    #---------------------land cover violin -----------------------#

    output$land_cover_violin <- plotly::renderPlotly({
      query <- if (is.null(email_filter())) {
        "SELECT forest, shrubland, arable, urban, grassland, wetland, marine, pasture
        FROM journeys.processed
        WHERE region_code IN ({region_codes*}) AND year IN ({years*});" %>%
          glue::glue_data_sql(
            list(
              region_codes = region_codes(),
              years = years()
            ),
            .,
            .con = conn
          )
      } else {
        "SELECT j.forest, j.shrubland, j.arable, j.urban, j.grassland, j.wetland, j.marine, j.pasture
        FROM journeys.processed j
        LEFT JOIN sign_ups.raw s ON j.user_id = s.id
        WHERE j.region_code IN ({region_codes*}) AND j.year IN ({years*}) AND s.user_email LIKE {email_pattern};" %>%
          glue::glue_data_sql(
            list(
              region_codes = region_codes(),
              years = years(),
              email_pattern = paste0("%", email_filter(), "%")
            ),
            .,
            .con = conn
          )
      }

      land_cover_data <- query %>%
        glue::glue_data_sql(
          list(
            region_codes = region_codes(),
            years = years()
          ),
          .,
          .con = conn
        ) %>%
        DBI::dbGetQuery(conn, .) %>%
        # Reshape data from wide to long format for violin plot
        tidyr::pivot_longer(
          cols = everything(),
          names_to = "land_cover_type",
          values_to = "proportion"
        ) %>%
        # Clean up land cover type names
        dplyr::mutate(
          land_cover_type = dplyr::case_when(
            land_cover_type == "rural_garden" ~ "Rural Garden",
            land_cover_type == "shrubland" ~ "Shrubland",
            TRUE ~ stringr::str_to_title(land_cover_type)
          )
        )

      land_cover_levels <- sort(unique(land_cover_data$land_cover_type))

      # Calculate mean proportions for each land cover type
      mean_proportions <- land_cover_data %>%
        dplyr::group_by(land_cover_type) %>%
        dplyr::summarise(
          mean_proportion = mean(proportion, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::arrange(match(land_cover_type, land_cover_levels))

      # Create base plot
      p <- plotly::plot_ly()

      # Add violin plots for each land cover type
      for (i in seq_along(land_cover_levels)) {
        cover_type <- land_cover_levels[i]
        cover_data <- land_cover_data %>%
          dplyr::filter(land_cover_type == cover_type) %>%
          dplyr::pull(proportion)

        p <- p %>% plotly::add_trace(
          x = rep(i, length(cover_data)),
          y = cover_data,
          type = "violin",
          name = cover_type,
          side = "both",
          width = 0.6,
          fillcolor = "#147331",
          line = list(color = "#147331"),
          opacity = 0.6,
          showlegend = FALSE,
          hoverinfo = "y",
          points = FALSE,
          box = list(visible = FALSE),
          meanline = list(visible = FALSE)
        )
      }

      # Create horizontal line shapes for each mean
      mean_shapes <- lapply(seq_along(land_cover_levels), function(i) {
        list(
          type = "line",
          x0 = i - 0.3,
          x1 = i + 0.3,
          y0 = mean_proportions$mean_proportion[i],
          y1 = mean_proportions$mean_proportion[i],
          line = list(color = "black", width = 2),
          layer = "above"
        )
      })

      p <- p %>%
      plotly::layout(
        dragmode = FALSE,
        shapes = mean_shapes,
        yaxis = list(
          title = "Proportion Coverage",
          range = c(0, 1)
        ),
        xaxis = list(
          title = "Land cover type",
          tickangle = 45,
          tickmode = "array",
          tickvals = seq_along(land_cover_levels),
          ticktext = land_cover_levels
        )
      ) %>%
      plotly::config(
        displayModeBar = FALSE
      )

      p
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
