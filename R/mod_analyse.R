#' Analyse UI Function
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

mod_analyse_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_sidebar(
    fillable = TRUE,
    sidebar = bslib::sidebar(
      shiny::selectInput(
        ns("area"),
        "Area",
        choices = c(bugsMatterDashboard::region_choices),
        selected = "uk"
      ),
      shiny::sliderInput(
        ns("year"),
        "Year",
        min = 2021,
        max = 2024,
        value = c(2021, 2024),
        step = 1,
        sep = ""
      )
    ),
    shiny::div(
      style = "height: calc(100svh - 70px); display: flex; flex-direction: column;",

      shiny::div(
        class = "data-header",
        shiny::h2("Bug Splat Analysis"),
        shiny::div(
          class = "data-lead",
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
        ),
      ),
      # shiny::hr(class = "data-header-hr"),
      shiny::div(
        style = "display: flex; gap: var(--_padding); height: 100%;",
        shiny::div(
          style = "width: 30%; height: 100%; padding-bottom: var(--_padding);",
          bslib::card(
            height = "100%",
            full_screen = TRUE,
            bslib::card_header(
              "Change in splat rate in response to variables"
            ),
            bslib::card_body(
              class = "p-0",
              div(
                style = "height: calc(100% + 30px); margin-top: -30px;",
                plotly::plotlyOutput(
                  ns("forest"),
                  height = "100%"
                )
              )
            )
          )
        ),
        shiny::div(
          style = "width: 70%; height: 100%; display: flex; flex-direction: column;",
          bslib::navset_card_tab(
            # full_screen = TRUE, #causes page layout to break slightly :(
            title = "Splat Rate",
            bslib::nav_panel(
              "Date",
              bslib::card_body(
                class = "p-0",
                plotly::plotlyOutput(
                  ns("splat_rate_line"),
                  height = "100%"
                )
              )
            ),
            bslib::nav_panel(
              "Year",
              bslib::card_body(
                class = "p-0",
                plotly::plotlyOutput(
                  ns("splat_rate_box"),
                  height = "100%"
                )
              )
            )
          ) %>%
            htmltools::tagAppendAttributes(style = "flex: 1;"),
          bslib::card(
            bslib::card_header("Model predictions of splat rate")
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
mod_analyse_server <- function(id, conn) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mod <- shiny::reactive({
      journeys <- "SELECT
        splatcount,
        year,
        distance,
        avg_speed,
        vehicle_cl,
        vehicle_he,
        hours_since_midnight,
        dayofyear,
        elevation,
        temp,
        lon,
        lat,
        forest,
        shrubland,
        grassland,
        wetland,
        marine,
        arable,
        plantation,
        urban,
        log_cm_miles_offset
      FROM bugs_matter.journeys_server
      WHERE region_id IN ({region_ids*})
      AND year >= {baseline_year} AND year <= {comparison_year};" %>%
        glue::glue_data_sql(
          list(
            region_ids = get_region_ids(input$area),
            baseline_year = input$year[1],
            comparison_year = input$year[2]
          ),
          .,
          .con = conn
        ) %>%
        DBI::dbGetQuery(conn, .) %>%
        dplyr::mutate(year = relevel(as.factor(.$year), ref = as.character(input$year[1])))
      tryCatch(
        MASS::glm.nb(
          splatcount ~ year +
            distance +
            avg_speed +
            vehicle_cl +
            vehicle_he +
            hours_since_midnight +
            dayofyear +
            elevation +
            temp +
            lon +
            lat +
            forest +
            shrubland +
            grassland +
            wetland +
            marine +
            arable +
            plantation +
            urban +
            stats::offset(log_cm_miles_offset),
          data = journeys
        ) %>%
          broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
          dplyr::filter(!term %in% c("(Intercept)", "stats::offset(log_cm_miles_offset)")) %>%
          dplyr::arrange(dplyr::desc(estimate)) %>%
          dplyr::mutate(term = dplyr::case_when(
            term == "hours_since_midnight" ~ "Time of Day",
            term == "temp" ~ "Temperature",
            term == "vehicle_clLCV" ~ "Vehicle [LCV]",
            term == "vehicle_clOther" ~ "Vehicle [Other]",
            term == "vehicle_clHCV" ~ "Vehicle [HCV]",
            term == "vehicle_he" ~ "Vehicle Height",
            term == "year2022" ~ "Year [2022]",
            term == "year2023" ~ "Year [2023]",
            term == "year2024" ~ "Year [2024]",
            term == "dayofyear" ~ "Day of Year",
            term == "lon" ~ "Longitude",
            term == "lat" ~ "Latitude",
            term == "avg_speed" ~ "Average Speed",
            .default = snakecase::to_title_case(term)
          )),
        error = function(e) {
          warning(paste("Model failed for region:", input$area, "Error:", e$message))
          return(NULL)
        }
      )
    }) %>%
      shiny::bindCache(
        input$area,
        input$year,
        cache = cachem::cache_disk(app_sys("./app-cache"))
      )

    add_forest_trace <- function(p, model_data, color){
      plotly::add_trace(
        p,
        showlegend = FALSE,
        data = model_data,
        x = ~estimate,
        y = ~term,
        type = "scatter",
        mode = "markers+text",
        error_x = list(
          type = "data",
          symmetric = FALSE,
          array = model_data$conf.high - model_data$conf.low,
          arrayminus = model_data$estimate - model_data$conf.low,
          color = color
        ),
        marker = list(size = 10, color = color),
        text = ~label,
        textposition = "top center",
        hoverinfo = "text",
        hovertext = ~paste0(
          "Variable: ", term, "<br>",
          "Estimate: ", label, "<br>",
          "CI: [", round(conf.low, 2), ", ", round(conf.high, 2), "]<br>"
        )
      )
    }

    output$forest <- plotly::renderPlotly({
      model_data <- mod() %>%
          dplyr::mutate(
            term = factor(term, levels = term),
            sig = dplyr::case_when(
              p.value < 0.001 ~ "***",
              p.value < 0.01 ~ "**",
              p.value < 0.05 ~ "*",
              TRUE ~ ""
            ),
            color = dplyr::case_when(
              conf.high < 1 ~ "red",
              conf.low > 1 ~ "green",
              .default = "gray"
            )
          ) %>%
          dplyr::mutate(
            label = ifelse(
              estimate < 0.01,
              paste("< 0.01", sig),
              paste(format(round(estimate, 2), nsmall = 2), sig)
            )
          )
      p <- plotly::plot_ly() %>%
        # Add green points (sig > 1)
        add_forest_trace(
          model_data = dplyr::filter(model_data, color == "green"),
          color = "green"
        ) %>%
        add_forest_trace(
          model_data = dplyr::filter(model_data, color == "red"),
          color = "red"
        ) %>%
        add_forest_trace(
          model_data = dplyr::filter(model_data, color == "gray"),
          color = "gray"
        ) %>%
        plotly::layout(
            xaxis = list(title = "Indcidence rate ratios", type = "log"),
            yaxis = list(title = "Explanatory variable"),
            # shapes = list(list(
            #   type = "line",
            #   x0 = 1, x1 = 1, y0 = -0.5, y1 = length(model_data$term) - 0.5,
            #   line = list(dash = "dash", width = 1)
            # )),
            dragmode = FALSE
          ) %>%
          plotly::config(
            displayModeBar = FALSE
          )
    })


  })
}

## To be copied in the UI
# mod_journeys_map_ui("journeys_map_1")

## To be copied in the server
# mod_journeys_map_server("journeys_map_1")
