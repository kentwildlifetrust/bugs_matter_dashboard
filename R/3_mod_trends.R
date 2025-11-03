#' Trends UI Function
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

mod_trends_ui <- function(id) {
  ns <- shiny::NS(id)

  data_header <- shiny::div(
    class = "data-header",
    shiny::h2(
      "Trend Analysis",
      shiny::actionLink(
        ns("analysis_info"),
        shiny::tags$i(class = "fa fa-info-circle")
      )
    ),
    tags$a(
      shiny::actionButton(
        ns("next_page"),
        shiny::span(
          style = "color: black;",
          "Track participation",
          shiny::tags$i(
            class = "fa fa-arrow-right"
          ),
        ),
        class = "btn-primary m-2",
          style = "flex-grow: 0; height: min-content; margin-bottom: 1rem !important;"
      ),
      href = "https://www.kentwildlifetrust.org.uk/get-involved/our-projects/bugs-matter",
      target = "_blank"
    )
  )

  data_control_row <- shiny::div(
    class = "data-control-row",
    shiny::div(
      class = "data-controls",
      shiny::selectInput(
        ns("region"),
        "Region",
        choices = bugsMatterDashboard::region_choices,
        selected = "world",
        width = 250
      ),
      # uiOutput(ns("slider_input"))
    ),
    bslib::value_box(
      class = "data-control-value-box",
      title = "Journey count",
      value = shiny::textOutput(ns("journey_count")),
      # showcase = tags$i(class = "fa fa-chart-line-down fa-solid"),
      theme = bslib::value_box_theme(
        bg = "#000000",
        fg = "#FFF"
      ),
      showcase_layout = "top right"
    )
  )

  scatters_card <- bslib::navset_card_pill(
    # full_screen = TRUE, #causes page layout to break slightly :(
    title = "Change in splats over time",
    height = "100%",
    bslib::nav_panel(
      "Model predictions",
      bslib::card_body(
        padding = c(0, 0, 10, 0),
        height = "100%",
        min_height = 400,
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
    )
  )

  forest_card <- bslib::navset_card_pill(
    height = "calc(100% - 12px)",
    full_screen = TRUE,
    title = "Change in splat rate in response to variables",
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
          "This forest plot of incidence rate ratios from the Negative Binomial statistical model shows the quantity of change (a multiplier) in the splat rate (splats per cm per km) given a one-unit change in the independent variables, while holding other variables in the model constant. Significant relationships between splat rate and independent variables are shown by asterisks (* p < 0.05, ** p < 0.01, *** p < 0.001). Vehicle types are compared to the reference category of ‘cars’.",
          placement = "bottom"
        )
      )
    ) %>%
      shiny::tagAppendAttributes(style = "position: relative;")
  ) %>%
  shiny::tagAppendAttributes(style = "flex: 1;")

  bslib::page(
    data_header,
    shiny::hr(class = "data-hr"),
    data_control_row,
    # shiny::hr(class = "data-header-hr"),
    shiny::div(
      class = "cards-container",
      shiny::div(
        style = "display: flex; flex-direction: column; min-width: 350px; flex: 1;",
        scatters_card
      ),
      forest_card
    )
  )
}

#' journeys_map Server Functions
#'
#' @noRd
mod_trends_server <- function(id, conn, next_page) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # output$slider_input <- renderUI({
    #   range <- "SELECT min(year), max(year)
    #   FROM journeys.processed
    #   WHERE region_code IN ({region_codes*})
    #   AND region_code IS NOT NULL;" %>%
    #     glue::glue_data_sql(
    #       list(
    #         region_codes = region_codes()
    #       ),
    #       .,
    #       .con = conn
    #     ) %>%
    #     DBI::dbGetQuery(conn, .)

    #   shiny::sliderInput(
    #     ns("year"),
    #     NULL,
    #     min = range$min,
    #     max = range$max,
    #     value = c(range$min, range$max),
    #     step = 1,
    #     sep = ""
    #   ) %>%
    #     shiny::tagAppendAttributes(style = "margin-left: 15px; margin-bottom: calc(1rem - 12px);")
    # })

    shiny::observeEvent(input$analysis_info, {
      shiny::showModal(
        shiny::modalDialog(
          shiny::div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            shiny::h3("About Trend Analysis"),
            shiny::actionLink(
              ns("close_modal"),
              shiny::tags$i(class = "fa fa-xmark-circle"),
              style = "font-size: 1.5rem; margin-bottom: .5rem;"
            )
          ),
          shiny::p(
            "To begin exploring the data and calculate simple summary statistics, insect splat counts
            recorded by citizen scientists are converted to a ‘splat rate’ by dividing the insect splat count by
            the number plate sampling area and the journey distance, expressed in a unit of ‘splats per cm² per km’.
            This metric makes the data comparable between journeys and is defined as the number of insects sampled
            per cm² of the number plate every kilometre. The response variable (insect count) shows a heavily right-skewed
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

    mod <- shiny::reactive({
      journeys <- "SELECT year,
          distance,
          avg_speed_kmh,
          vehicle_class,
          vehicle_height,
          time_of_day,
          day_of_year,
          elevation,
          temperature,
          center_lat,
          center_lon,
          midpoint_time,
          forest * 100 AS forest,
          shrubland * 100 AS shrubland,
          grassland * 100 AS grassland,
          wetland * 100 AS wetland,
          arable * 100 AS arable,
          urban * 100 AS urban,
          pasture * 100 AS pasture,
          marine * 100 AS marine,
          log_cm_km_offset,
          splat_count
        FROM journeys.processed
        WHERE region_code IS NOT NULL AND region_code IN ({region_codes*});" %>%
        glue::glue_data_sql(
          list(
            region_codes = region_codes()
          ),
          .,
          .con = conn
        ) %>%
        DBI::dbGetQuery(conn, .) %>%
        dplyr::mutate(
          vehicle_class = as.factor(vehicle_class),
          year = as.integer(year),
          time_of_day = as.numeric(lubridate::hms(format(midpoint_time, "%H:%M:%S"))) / 3600,
          temperature = ifelse(is.na(temperature), mean(.$temperature, na.rm = TRUE), temperature)
        )
      tryCatch(
        model(journeys),
        error = function(e) {
          warning(paste("Model failed for region:", input$region, "Error:", e$message))
          return(NULL)
        }
      )
    })


    forest_data <- shiny::reactive({
      est <- confint(mod(), parm = "beta_", level = 0.95) %>%
        as.data.frame()
      est1 <- est[rownames(est) != "(Intercept)", , drop = FALSE]
      pvals <- summary(mod())$coefficients$cond[, 4]
      pvals <- data.frame(
        term = names(pvals),
        p_value = unname(pvals)
      )
      est2 <- 1 - exp(est1)
      est2 <- est2 %>%
          as.data.frame() %>%
          dplyr::mutate(term = row.names(est1)) %>%
          dplyr::left_join(
            pvals,
            by = "term"
          ) %>%
          dplyr::rename(
            low = "2.5 %",
            high = "97.5 %",
            estimate = "Estimate"
          )
      row.names(est2) <- est2$term
      est2 %>%
        dplyr::filter(!term %in% c("(Intercept)", "stats::offset(log_cm_km_offset)")) %>%
        dplyr::arrange(estimate) %>%
        dplyr::mutate(term = dplyr::case_when(
          term == "Time.of.day" ~ "Time of Day",
          term == "Temperature" ~ "Temperature",
          term == "Vehicle.typeLCV" ~ "Vehicle [LCV]",
          term == "Vehicle.typeOther" ~ "Vehicle [Other]",
          term == "Vehicle.typeHCV" ~ "Vehicle [HCV]",
          term == "Day.of.year" ~ "Day of Year",
          term == "Average.speed" ~ "Average Speed (km/h)",
          .default = snakecase::to_title_case(term)
        ))
    }) %>%
      shiny::bindCache(
        x = .,
        input$region,
        cache = cachem::cache_disk("./.cache")
      )

    output$trend <- shiny::renderText({
      val <- forest_data() %>%
        dplyr::filter(term == "Year") %>%
        dplyr::pull("estimate") %>%
        round(1)

      paste0("-", val * 100, "%")
    })

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
          array = model_data$estimate - model_data$low,
          arrayminus = model_data$estimate - model_data$low,
          color = color
        ),
        marker = list(size = 10, color = color),
        text = ~label,
        textposition = "top center",
        hoverinfo = "text",
        hovertext = ~ paste0(
          "Variable: ", term, "<br>",
          "Estimate: ", label, "<br>",
          "CI: [", round(low, 2), ", ", round(high, 2), "]<br>"
        )
      )
    }

    output$forest <- plotly::renderPlotly({
      model_data <- forest_data() %>%
        dplyr::mutate(
          term = factor(term, levels = term),
          sig = dplyr::case_when(
            p_value < 0.001 ~ "***",
            p_value < 0.01 ~ "**",
            p_value < 0.05 ~ "*",
            TRUE ~ ""
          ),
          color = dplyr::case_when(
            p_value > 0.05 ~ "darkgray",
            estimate < 0 ~ "red",
            estimate > 0 ~ "green",
            .default = "darkgray"
          )
        ) %>%
        dplyr::mutate(
          label = paste(format(round(estimate, 2), nsmall = 2), sig)
        )
      print(model_data)
      p <- plotly::plot_ly() %>%
        # Add green points (sig > 1)
        add_forest_trace(
          model_data = dplyr::filter(model_data, color == "red"),
          color = "red"
        ) %>%
        add_forest_trace(
          model_data = dplyr::filter(model_data, color == "darkgray"),
          color = "darkgray"
        ) %>%
        add_forest_trace(
          model_data = dplyr::filter(model_data, color == "green"),
          color = "green"
        ) %>%
        plotly::layout(
          xaxis = list(
            title = "Effect",
            # type = "log",
            # tickvals = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100),
            # ticktext = c("0.0001", "0.001", "0.01", "0.1", "1", "10", "100"),
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
      sjPlot::get_model_data(mod(), type = "pred", terms = "Year")
    }) %>%
      shiny::bindCache(
        input$region,
        cache = "app"
      )

    output$model_predicted <- plotly::renderPlotly({
      model_data <- as.data.frame(model_predictions()) %>%
        dplyr::rename(low = conf.low, high = conf.high)
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
          array = model_data$high - model_data$low,
          arrayminus = model_data$predicted - model_data$low,
          color = "black"
        ),
        marker = list(size = 10, color = "black"),
        hoverinfo = "text",
        hovertext = ~ paste0(
          "Year: ", x, "<br>",
          "Predicted splat count: ", round(predicted, 2), "<br>",
          "CI: [", round(low, 2), ", ", round(high, 2), "]<br>"
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
        journeys.processed
      WHERE splat_rate IS NOT NULL AND midpoint_time IS NOT NULL
      AND region_code IN ({region_codes*})
      AND region_code IS NOT NULL
      ORDER BY
        midpoint_time;
      " %>%
        glue::glue_data_sql(
          list(
            region_codes = region_codes()
          ),
          .,
          .con = conn
        ) %>%
        DBI::dbGetQuery(conn, .)
    }) %>%
      shiny::bindCache(
        input$region,
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
            title = "Splat rate (splats/cm/km)"
          ),
          xaxis = list(
            title = "Journey Date",
            tickvals = 2021:2025,
            ticktext = 2021:2025,
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
