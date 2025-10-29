#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_fillable(
    shiny::div(
      class = "overview-page",

      shiny::div(
        class = "overview-page-content",
        shiny::div(
          class = "overview-header",
          shiny::div(
            class = "overview-message",
            "Welcome to the"
          ),
          shiny::h1(
            "Bugs Matter Dashboard"
          ),
          shiny::tags$br(),
          shiny::p(
            class = "overview-lead",
            "Stay up to date with", tags$strong("Bugs Matter"), " - the global citizen science survey tracking flying insect abundance through ‘bug splats’ on vehicle number plates.
            Explore where and how many journeys have been recorded, the distances travelled, and the types of vehicles used.
            Discover trends in bug splats across locations and time, and see how our global community of recorders is contributing to the project."
          )
        ),
        shiny::tags$br(),
        shiny::div(
          class = "overview-body",
          shiny::h3("At a Glance"),
          shiny::div(
            class = "overview-figures",
            shiny::div(
              class = "row overview-value-row",
              shiny::div(
                class = "col-sm",
                bslib::value_box(
                  class = "overview-value-box",
                  title = "Distance sampled",
                  value = shiny::textOutput(ns("distance")),
                  showcase = tags$i(class = "fa fa-route fa-solid"),
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
                  class = "overview-value-box",
                  title = "Splats counted",
                  value = shiny::textOutput(ns("splats")),
                  showcase = tags$i(class = "fa fa-mosquito fa-solid"),
                  theme = bslib::value_box_theme(
                    bg = "#72DB41",
                    fg = "#000"
                  ),
                  showcase_layout = "top right"
                )
              ),
              shiny::div(
                class = "col-sm",
                bslib::value_box(
                  class = "overview-value-box",
                  title = "Yearly change in splat rate 2021 - 2025",
                  value = shiny::textOutput(ns("trend")),
                  showcase = tags$i(class = "fa fa-chart-line-down fa-solid"),
                  theme = bslib::value_box_theme(
                    bg = "#000000",
                    fg = "#FFF"
                  ),
                  showcase_layout = "top right"
                )
              )
            ),
            shiny::tags$br(),
            bslib::navset_card_pill(
              title = "Modelled change in splat rate, 2021 to 2025",
              bslib::card_body(
                class = "p-0",
                leaflet::leafletOutput(ns("map"), height = "100%"),
                min_height = 600
              ),
              full_screen = TRUE
            ),
          ),
          shiny::tags$br(),
          shiny::h3("About the Project"),
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
been reported from empirical data",
            bslib::tooltip(shiny::a("[1]", class = "ref-link ref-link-nospace"), "Møller, 2019", placement = "top"),
            ", have been
interpreted as an indicator of major global declines in insect
abundance."
          ),
          shiny::p(
            "A growing body of evidence",
            bslib::tooltip(shiny::a("[2]", class = "ref-link"), "Fox et al., 2013; Hallmann et al.,
2017; Goulson, D. 2019; Sánchez-Bayo et al., 2019; Thomas
et al., 2019; van der Sluijs, 2020; Macadam et al., 2020;
Outhwaite, McCann and Newbold, 2022", placement = "top"),
            "highlights population
declines in insects and other invertebrates at UK and global
scales. These declines, which are evident across all functional
groups of insects (herbivores, detritivores, parasitoids,
predators and pollinators), could have catastrophic impacts
on the Earth’s natural systems and human survivability
on our planet. Invertebrates are functionally of greater
importance than large-bodied fauna, and in terms of biomass,
bioabundance and species diversity, they make up the
greatest proportion of life on Earth."
          ),
          shiny::p(
            htmltools::HTML("The Bugs Matter citizen science survey took place throughout the UK between 1<sup>st</sup> June and 31<sup>st</sup>
            August in 2021, 2022 and 2023, and between 1<sup>st</sup> May and 30<sup>th</sup> September in 2024, using the Bugs Matter
            mobile application. In 2021 and 2022, users received a standardised sampling grid,
            termed a ‘splatometer’, in the post after they had signed up in the application.
            However, in 2023 the whole number plate was used to count insect splats."
          )),
          shiny::p(
            "The Bugs Matter app is available to download for free from the Apple App Store and Google Play.
            The app was built by Natural Apptitude and uses the Coreo data collection system. Within the app,
            users add details about the vehicle used for sampling, and are asked to confirm whether their number
            plate measures to standard UK dimensions, and if not, asked to manually input the
            dimensions of their number plate. Multiple vehicles can be added by a single user. Vehicle specification
            information is used in the analysis to determine if different types of vehicles sample insects differently."
          ),
          shiny::div(
            style = "display: flex; justify-content: right; margin-bottom: 60px;",
            shiny::actionButton(
              ns("next_page"),
              shiny::span(
                style = "color: black;",
                "Explore journeys",
                shiny::tags$i(
                  class = "fa fa-arrow-right"
                ),
              ),
              class = "btn-primary m-2"
            )
          )
        )
      )
    )
  )
}

#' overview Server Functions
#'
#' @noRd
mod_overview_server <- function(id, conn, next_page) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pal <- leaflet::colorNumeric("Spectral", -50:50)

    output$distance <- shiny::renderText({
        "SELECT ROUND(SUM(distance)::NUMERIC, 0) AS length FROM journeys.processed;" %>%
        DBI::dbGetQuery(conn, .) %>%
        dplyr::pull("length") %>%
        format(big.mark = ",") %>%
        paste("km")
    })

    output$splats <- shiny::renderText({
      "SELECT SUM(splat_count) AS n_splats FROM journeys.processed;" %>%
        DBI::dbGetQuery(conn, .) %>%
        dplyr::pull("n_splats") %>%
        prettyNum(big.mark = ",")
    })

    output$trend <- shiny::renderText({
      DBI::dbGetQuery(
        conn,
        "SELECT est FROM analysis.yearly_change WHERE region = 'global' LIMIT 1;"
      ) %>%
        dplyr::pull("est") %>%
        round(1) %>%
        paste0("%")
    })

    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(
        options = leaflet::leafletOptions(zoomControl = FALSE)
      ) %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::setView(lng = -3.244293, lat = 54.350497, zoom = 6)
    })

    shiny::observe({
      data <- sf::st_read(
        conn,
        query = "SELECT -1 * round(t.est, 1) AS est,
        r.name,
        -1 * round(t.low, 1) AS low,
        -1 * round(t.high, 1) AS high,
        t.p_value,
        st_x(st_centroid(r.geom)) AS lon,
        st_y(st_centroid(r.geom)) AS lat,
        r.geom
        FROM ref.regions r
        LEFT JOIN analysis.yearly_change t ON r.code = t.region
        WHERE r.code IN (
          SELECT DISTINCT region_code FROM journeys.processed
        );"
      )
      leaflet::leafletProxy("map") %>%
        leaflet::addPolygons(
          data = data,
          color = "gray",
          weight = 1,
          opacity = 1,
          fillOpacity = 0.5,
          fillColor = ~ pal(est),
          popup = ~ mapply(
            function(region_name,
                     est,
                     low,
                     high,
                     p_value) {
              if (!is.na(est)) {
                return(sprintf(
                  '<div class="popup-title">%s</div>
                  <hr class="popup-hr" />
                  <div class="map-stat-small">Statistically significant trend<div>
                  <span class="map-stat-large">%s%% </span>per year<br></br>
                  <div class="map-stat-small">95%% confidence interval: %s%% to %s%%</div>',
                  region_name,
                  est,
                  low,
                  high
                ))
              } else {
                return(sprintf(
                  '<div class="popup-title">%s</div>
                <hr class="popup-hr" />
                <div class="map-stat-small">No statistically significant trend</div>',
                  region_name
                ))
              }
            },
            name,
            est,
            low,
            high,
            p_value,
            SIMPLIFY = FALSE
          ) %>%
            unname()
        ) %>%
        leaflet::addLabelOnlyMarkers(
          lat = data$lat,
          lng = data$lon,
          label = mapply(
            function(low, high) {
              if (is.na(low) | is.na(high)) {
                return()
              }
              if (low < 0 & high < 0) {
                return('<i class="fa fa-solid fa-down map-data-icon" style="font-size: 1.5rem; color: #000"></i>')
              }
              if (low > 0 & high > 0) {
                return('<i class="fa fa-solid fa-up map-data-icon" style="font-size: 1.5rem; color: #000"></i>')
              }
              return('<i class="fa fa-solid fa-minus map-data-icon" style="font-size: 1.5rem; color: #000"></i>')
            },
            low = data$low,
            high = data$high,
            SIMPLIFY = FALSE
          ) %>%
            lapply(htmltools::HTML),
          labelOptions = leaflet::labelOptions(
            noHide = TRUE, # Show label all the time
            direction = "center",
            textOnly = TRUE
          )
        ) %>%
        leaflet::addLegend(
          pal = pal,
          values = data$est,
          opacity = 1,
          title = "Annual change in splat rate",
          labFormat = leaflet::labelFormat(suffix = "%")
        )
    })

    shiny::observeEvent(input$next_page, {
      next_page(next_page() + 1)
    })
  })
}

## To be copied in the UI
# mod_overview_ui("overview_1")

## To be copied in the server
# mod_overview_server("overview_1")
