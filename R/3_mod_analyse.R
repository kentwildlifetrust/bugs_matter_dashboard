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
  bslib::page(
    shiny::div(
      class = "data-header",
      shiny::h2(
        "Bug Splat Analysis",
        shiny::actionLink(
          ns("analysis_info"),
          shiny::tags$i(class = "fa fa-info-circle")
        )
      ),
      shiny::actionButton(
        ns("next_page"),
        shiny::span(
          style = "color: white;",
          "Get involved",
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
          ns("area"),
          "Area",
          choices = c(bugsMatterDashboard::region_choices),
          selected = "uk"
        ),
        shiny::sliderInput(
          ns("year"),
          NULL,
          min = 2021,
          max = 2024,
          value = c(2021, 2024),
          step = 1,
          sep = ""
        ) %>%
        shiny::tagAppendAttributes(style = "margin-left: 15px; margin-bottom: calc(1rem - 12px);")
      )
    ),
    # shiny::hr(class = "data-header-hr"),
    shiny::div(
      class = "cards-container",
      shiny::div(
        style = "display: flex; flex-direction: column; min-width: 350px; flex: 1;",
        bslib::navset_card_tab(
          # full_screen = TRUE, #causes page layout to break slightly :(
          title = "Change in splats over time",
          height = "100%",
          bslib::nav_panel(
            "Model predictions",
            bslib::card_body(
              padding = c(0, 0, 10, 0),
              height = "100%",
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(
                  ns("model_predicted"),
                  height = "100%"
                )
              ),
              div(
                style = "position: absolute; top: 20px; right: 20px;",
                bslib::popover(
                  shiny::actionLink(
                    ns("model_predicted_info"),
                    shiny::tags$i(class = "fa fa-info-circle"),
                    style = "font-size: 1.5rem;"
                  ),
                  "This plot shows the predicted splat counts from the Negative Binomial statistical model for each year. These are the most reliable results because the statistical model takes into account other factors that could affect how many insects are splatted.",
                  placement = "bottom"
                )
              )
            ) %>%
              shiny::tagAppendAttributes(style = "position: relative;")
          ),
          bslib::nav_panel(
            "Cumulative average",
            bslib::card_body(
              padding = c(0, 0, 10, 0),
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(
                  ns("splat_rate_line"),
                  height = "100%"
                )
              ),
              div(
                style = "position: absolute; top: 20px; right: 20px;",
                bslib::popover(
                  shiny::actionLink(
                    ns("cumulative_average_info"),
                    shiny::tags$i(class = "fa fa-info-circle"),
                    style = "font-size: 1.5rem;"
                  ),
                  "This line plot shows how the mean splat rate changes over time. This result doesn't take into account all the other factors that could affect how many insects are splatted, so it should be interpreted with caution.",
                  placement = "bottom"
                )
              )
            ) %>%
              shiny::tagAppendAttributes(style = "position: relative;")
          ) # ,
          # bslib::nav_panel(
          #   "By year",
          #   bslib::card_body(
          #     padding = c(0, 0, 10, 0),
          #     plotly::plotlyOutput(
          #       ns("splat_rate_box"),
          #       height = "100%"
          #     )
          #   )
          # )
        )
      ),
        bslib::card(
          height = "calc(100% - 12px)",
          full_screen = TRUE,
          bslib::card_header(
            "Change in splat rate in response to variables"
          ),
          bslib::card_body(
            padding = c(0, 0, 10, 0),
            height = "100%",
            div(
              style = "max-height: 100%; overflow-y: auto;",
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(
                  ns("forest"),
                  height = 1000
                )
              ),
            ),
            div(
              style = "position: absolute; top: 20px; right: 20px;",
              bslib::popover(
                shiny::actionLink(
                  ns("forest_info"),
                  shiny::tags$i(class = "fa fa-info-circle"),
                  style = "font-size: 1.5rem;"
                ),
                "This forest plot of incidence rate ratios from the Negative Binomial statistical model shows the quantity of change (a multiplier) in the splat rate (splats per cm per mile) given a one-unit change in the independent variables, while holding other variables in the model constant. Significant relationships between splat rate and independent variables are shown by asterisks (* p < 0.05, ** p < 0.01, *** p < 0.001). Vehicle types are compared to the reference category of ‘cars’.",
                placement = "bottom"
              )
            )
          ) %>%
            shiny::tagAppendAttributes(style = "position: relative;")
        ) %>%
        shiny::tagAppendAttributes(style = "flex: 1;")
    )
  )
}

#' journeys_map Server Functions
#'
#' @noRd
mod_analyse_server <- function(id, conn, next_page) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(input$analysis_info, {
      shiny::showModal(
        shiny::modalDialog(
          shiny::div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            shiny::h3("About Bug Splat Analysis"),
            shiny::actionLink(
              ns("close_modal"),
              shiny::tags$i(class = "fa fa-xmark-circle"),
              style = "font-size: 1.5rem; margin-bottom: .5rem;"
            )
          ),
          shiny::p(
            "To begin exploring the data and calculate simple summary statistics, insect splat counts
            recorded by citizen scientists are converted to a ‘splat rate’ by dividing the insect splat count by
            the number plate sampling area and the journey distance, expressed in a unit of ‘splats per cm² per mile’.
            This metric makes the data comparable between journeys and is defined as the number of insects sampled
            per cm² of the number plate every mile. The response variable (insect count) shows a heavily right-skewed
            distribution due to the high number of zero and low values, as is typical for count-derived data.
            Therefore, a negative binomial generalized linear model (NB) was used to examine the relative effects
            of survey year, time of day of the journey, calendar date of the journey, average journey temperature,
            average journey speed, journey distance, vehicle type, elevation, local land cover, and road type, on
                 splat count."
          ),
          shiny::p(
            "The analysis was performed using the MASS package (Venables and Ripley, 2002) in RStudio
            (R Core Team, 2022), following established techniques (Sokal and Rolf, 1995; Crawley, 2007). After
            running the model, variance inflation factor (VIF) scores were calculated to check for multicollinearity
            between independent variables. Comparisons of the number of insect splats between different timescales
            is achieved by rerunning the models with different reference years. The results of the ZINB model show
            the quantity of change in the response variable given a one-unit change in the independent variable,
            while holding other variables in the model constant. These values are called incidence rate ratios
            and they can be visualised effectively in a forest plot. Also presented, are plots of adjusted
            predictions of splat count across years, corrected for number plate sampling area and journey distance."
          ),
          easyClose = TRUE,
          footer = NULL
        )
      )
    })

    mod <- shiny::reactive({
      journeys <- "SELECT
        splat_count,
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
          splat_count ~ year +
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
            urban +
            stats::offset(log_cm_miles_offset),
          data = journeys
        ),
        error = function(e) {
          warning(paste("Model failed for region:", input$area, "Error:", e$message))
          return(NULL)
        }
      )
    })

    forest_data <- shiny::reactive({
      mod() %>%
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
        ))
    }) %>%
      shiny::bindCache(
        input$area,
        input$year,
        cache = "app"
      )

    add_forest_trace <- function(p, model_data, color) {
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
        hovertext = ~ paste0(
          "Variable: ", term, "<br>",
          "Estimate: ", label, "<br>",
          "CI: [", round(conf.low, 2), ", ", round(conf.high, 2), "]<br>"
        )
      )
    }

    output$forest <- plotly::renderPlotly({
      model_data <- forest_data() %>%
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
            .default = "darkgray"
          )
        ) %>%
        dplyr::mutate(
          label = ifelse(
            estimate < 0.01,
            paste("< 0.01", sig),
            ifelse(
              estimate > 100,
              paste("> 100", sig),
              paste(format(round(estimate, 2), nsmall = 2), sig)
            )
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
          model_data = dplyr::filter(model_data, color == "darkgray"),
          color = "darkgray"
        ) %>%
        plotly::layout(
          xaxis = list(
            title = "Incidence rate ratios",
            type = "log",
            tickvals = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100),
            ticktext = c("0.0001", "0.001", "0.01", "0.1", "1", "10", "100"),
            showgrid = FALSE,
            tickfont = list(size = 10),
            ticks = "outside"
          ),
          yaxis = list(title = "Explanatory variable"),
          shapes = list(list(
            type = "line",
            x0 = 1, x1 = 1, y0 = -0.5, y1 = length(model_data$term) - 0.5,
            line = list(color = "lightgray", width = 1),
            layer = "below"
          )),
          dragmode = FALSE
        ) %>%
        plotly::config(
          displayModeBar = FALSE
        )
    })

    model_predictions <- shiny::reactive({
      sjPlot::get_model_data(mod(), type = "pred", terms = "year")
    }) %>%
      shiny::bindCache(
        input$area,
        input$year,
        cache = "app"
      )

    output$model_predicted <- plotly::renderPlotly({
      model_data <- as.data.frame(model_predictions())
      int_ticks <- sort(unique(model_data$x))
      plotly::plot_ly(
        showlegend = FALSE,
        data = model_data,
        x = ~x,
        y = ~predicted,
        type = "scatter",
        error_y = list(
          type = "data",
          symmetric = FALSE,
          array = model_data$conf.high - model_data$conf.low,
          arrayminus = model_data$predicted - model_data$conf.low,
          color = "black"
        ),
        marker = list(size = 10, color = "black"),
        hoverinfo = "text",
        hovertext = ~ paste0(
          "Year: ", x, "<br>",
          "Predicted splat count: ", round(predicted, 2), "<br>",
          "CI: [", round(conf.low, 2), ", ", round(conf.high, 2), "]<br>"
        )
      ) %>%
        plotly::layout(
          yaxis = list(
            title = "Splat count"
          ),
          xaxis = list(
            title = "Year",
            tickvals = int_ticks,
            ticktext = int_ticks,
            showgrid = TRUE,
            gridcolor = "lightgray",
            gridwidth = 1
          ),
          dragmode = FALSE
        ) %>%
        plotly::config(
          displayModeBar = FALSE
        )
    })

    splat_rate_line_data <- shiny::reactive({
      "SELECT
        midpoint_time,
        splat_rate
        --AVG(splat_rate) OVER (ORDER BY midpoint_time ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS splat_rate
      FROM
        bugs_matter.journeys_server
      WHERE splat_rate IS NOT NULL AND midpoint_time IS NOT NULL
      ORDER BY
        midpoint_time;
      " %>%
        glue::glue_data_sql(
          list(
            region_ids = get_region_ids(input$area),
            baseline_year = input$year[1],
            comparison_year = input$year[2]
          ),
          .,
          .con = conn
        ) %>%
        DBI::dbGetQuery(conn, .)
    }) %>%
      shiny::bindCache(
        input$area,
        input$year,
        cache = "app"
      )

    output$splat_rate_line <- plotly::renderPlotly({
      plotly::plot_ly(
        data = splat_rate_line_data(),
        x = ~midpoint_time,
        y = ~ dplyr::cummean(splat_rate),
        type = "scatter",
        mode = "lines",
        line = list(color = "black")
      ) %>%
        plotly::layout(
          yaxis = list(
            title = "Splat rate (splats/cm/mile)"
          ),
          xaxis = list(
            title = "Journey Date",
            tickvals = input$year[1]:input$year[2],
            ticktext = input$year[1]:input$year[2],
            showgrid = TRUE,
            gridcolor = "lightgray",
            gridwidth = 1
          ),
          dragmode = FALSE
        ) %>%
        plotly::config(
          displayModeBar = FALSE
        )
    })

    shiny::observeEvent(input$next_page, {
      next_page(next_page() + 1)
    })
  })
}
