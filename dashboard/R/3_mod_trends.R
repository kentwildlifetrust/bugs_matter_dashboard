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
        shiny::tags$i(class = "fa fa-info-circle"),
        `aria-label` = "Information about trend analysis"
      )
    ),
    shiny::actionButton(
      ns("next_page"),
      shiny::span(
        style = "font-weight: 400;",
        "Track participation",
        shiny::tags$i(
          class = "fa fa-arrow-right"
        ),
      ),
      class = "btn-primary m-2",
      style = "flex-grow: 0; height: min-content;",
      `aria-label` = "Go to Track participation page"
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
        selected = "global",
        width = 250
      ),
      # uiOutput(ns("slider_input"))
    ),
    bslib::value_box(
      class = "data-control-value-box",
      title = "Yearly change in splat rate",
      value = uiOutput(ns("trend")),
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
              style = "font-size: 1.5rem;",
              `aria-label` = "Information about model predictions plot"
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
              style = "font-size: 1.5rem;",
              `aria-label` = "Information about cumulative average plot"
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
    bslib::nav_panel(
      "Forest plot",
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
              style = "font-size: 1.5rem;",
              `aria-label` = "Information about forest plot"
            ),
            "This forest plot of incidence rate ratios from the Negative Binomial statistical model shows the quantity of change (a multiplier) in the splat rate (splats per cm per km) given a one-unit change in the independent variables, while holding other variables in the model constant. Significant relationships between splat rate and independent variables are shown by asterisks (* p < 0.05, ** p < 0.01, *** p < 0.001). Vehicle types are compared to the reference category of 'cars'.",
            placement = "bottom"
          )
        )
      ) %>%
        shiny::tagAppendAttributes(style = "position: relative;")
    ),
    bslib::nav_panel(
      "Text description",
      bslib::card_body(
        padding = c(0, 0, 10, 0),
        height = "100%",
        div(
          style = "max-height: 100%; overflow-y: auto;",
          shinycssloaders::withSpinner(
            reactable::reactableOutput(ns("predictors_table"))
          ),
        )
      ) %>%
        shiny::tagAppendAttributes(style = "position: relative;")
    )
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
              style = "font-size: 1.5rem; margin-bottom: .5rem;",
              `aria-label` = "Close modal dialog"
            )
          ),
          shiny::p(
            "To begin exploring the data and calculate simple summary statistics, insect splat counts
            recorded by citizen scientists are converted to a ‘splat rate’ by dividing the insect splat count by
            the number plate sampling area and the journey distance, expressed in a unit of ‘splats per cm² per km’.
            This metric makes the data comparable between journeys and is defined as the number of insects sampled
            per cm² of the number plate every kilometre. The response variable (insect count) shows a heavily
            right-skewed distribution due to the high number of zero and low values, as is typical for count-derived data.
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
      # 3 character values are country codes
      if (input$region == "global") {
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

    estimates <- shiny::reactive({
      "SELECT * FROM analysis.estimates
      WHERE region = {input$region}" %>%
        glue::glue_sql(.con = conn) %>%
        DBI::dbGetQuery(conn = conn, .)
    })

    forest_data <- shiny::reactive({
      estimates() %>%
        dplyr::arrange(est) %>%
        dplyr::mutate(
          variable = dplyr::case_when(
            variable == "time_of_day" ~ "Time of Day",
            variable == "temperature" ~ "Temperature",
            variable == "vehicle_classLCV" ~ "Vehicle [LCV]",
            variable == "vehicle_classOther" ~ "Vehicle [Other]",
            variable == "vehicle_classHCV" ~ "Vehicle [HGV]",
            variable == "day_of_year" ~ "Day of Year",
            variable == "avg_speed_kmh" ~ "Average Speed (km/h)",
            variable == "center_lat" ~ "Latitude",
            variable == "center_lon" ~ "Longitude",
            .default = snakecase::to_title_case(variable)
          )
        )
    })

    output$trend <- shiny::renderUI({
      vals <- forest_data() %>%
        dplyr::filter(variable == "Year") %>%
        dplyr::mutate(
          est = paste0(format(round(est, 1), nsmall = 1), "%"),
          low = paste0(format(round(low, 1), nsmall = 1), "%"),
          high = paste0(format(round(high, 1), nsmall = 1), "%"),
          p_value = scales::pvalue(p_value, accuracy = 0.001)
        )

      div(
        vals$est,
        bslib::tooltip(
          shiny::a(
            "[1]",
            class = "ref-link ref-link-nospace ref-link-light",
            `aria-label` = sprintf(
              "Statistical information: 95%% confidence interval: %s to %s, p %s",
              vals$low,
              vals$high,
              vals$p_value
            )
          ),
          sprintf(
            "%s confidence interval: %s to %s, p %s",
            "95%",
            vals$low,
            vals$high,
            vals$p_value
          ),
          placement = "bottom"
        )
      )
    })

    output$predictors_table <- reactable::renderReactable({
      forest_data() %>%
        dplyr::mutate(
          more_or_less = ifelse(est > 0, "more", "fewer"),
          percentage = paste0(format(round(abs(est), 1), nsmall = 1), "%"),
          sig_description = dplyr::case_when(
            variable == "Year" ~ sprintf(
              "For each additional year, there were %s %s insects recorded.",
              percentage,
              more_or_less
            ),
            variable == "Distance" ~ sprintf(
              "For each additional kilometre of journey distance, there were %s %s insects recorded.",
              percentage,
              more_or_less
            ),
            variable == "Average Speed (km/h)" ~ sprintf(
              "For each additional km/h of average speed, there were %s %s insects recorded.",
              percentage,
              more_or_less
            ),
            variable == "Vehicle [LCV]" ~ sprintf(
              "Light commercial vehicles recorded %s %s insects than cars.",
              percentage,
              more_or_less
            ),
            variable == "Vehicle [Other]" ~ sprintf(
              "Other vehicle types recorded %s %s insects than cars.",
              percentage,
              more_or_less
            ),
            variable == "Vehicle [HGV]" ~ sprintf(
              "Heavy goods vehicles recorded %s %s insects than cars.",
              percentage,
              more_or_less
            ),
            variable == "Time of Day" ~ sprintf(
              "For each additional hour of the day, there were %s %s insects recorded.",
              percentage,
              more_or_less
            ),
            variable == "Day of Year" ~ sprintf(
              "For each additional day of the year, there were %s %s insects recorded.",
              percentage,
              more_or_less
            ),
            variable == "Elevation" ~ sprintf(
              "For each additional metre of elevation, there were %s %s insects recorded.",
              percentage,
              more_or_less
            ),
            variable == "Temperature" ~ sprintf(
              "For each one degree increase in temperature, there were %s %s insects recorded.",
              percentage,
              more_or_less
            ),
            variable == "Longitude" ~ sprintf(
              "For each additional degree of longitude, there were %s %s insects recorded.",
              percentage,
              more_or_less
            ),
            variable == "Latitude" ~ sprintf(
              "For each additional degree of latitude, there were %s %s insects recorded.",
              percentage,
              more_or_less
            ),
            variable == "Proportion Forest" ~ sprintf(
              "For each additional percentage point of forest cover, there were %s %s insects recorded.",
              percentage,
              more_or_less
            ),
            variable == "Proportion Shrubland" ~ sprintf(
              "For each additional percentage point of shrubland cover, there were %s %s insects recorded.",
              percentage,
              more_or_less
            ),
            variable == "Proportion Grassland" ~ sprintf(
              "For each additional percentage point of grassland cover, there were %s %s insects recorded.",
              percentage,
              more_or_less
            ),
            variable == "Proportion Wetland" ~ sprintf(
              "For each additional percentage point of wetland cover, there were %s %s insects recorded.",
              percentage,
              more_or_less
            ),
            variable == "Proportion Marine" ~ sprintf(
              "For each additional percentage point of marine cover, there were %s %s insects recorded.",
              percentage,
              more_or_less
            ),
            variable == "Proportion Arable" ~ sprintf(
              "For each additional percentage point of arable land cover, there were %s %s insects recorded.",
              percentage,
              more_or_less
            ),
            variable == "Proportion Urban" ~ sprintf(
              "For each additional percentage point of urban cover, there were %s %s insects recorded.",
              percentage,
              more_or_less
            ),
            .default = sprintf(
              "For each unit increase in %s, there were %s %s insects recorded.",
              variable,
              percentage,
              more_or_less
            )
          ),
          nonsig_description = dplyr::case_when(
            variable ==
              "Year" ~ "The number of insects recorded did not change significantly with year.",
            variable ==
              "Distance" ~ "The number of insects recorded did not change significantly with journey distance.",
            variable ==
              "Average Speed (km/h)" ~ "The number of insects recorded did not change significantly with average speed.",
            variable ==
              "Vehicle [LCV]" ~ "Light commercial vehicles did not record significantly different numbers of insects than cars.",
            variable ==
              "Vehicle [Other]" ~ "Other vehicle types did not record significantly different numbers of insects than cars.",
            variable ==
              "Vehicle [HGV]" ~ "Heavy goods vehicles did not record significantly different numbers of insects than cars.",
            variable ==
              "Time of Day" ~ "The number of insects recorded did not change significantly with time of day.",
            variable ==
              "Day of Year" ~ "The number of insects recorded did not change significantly with day of year.",
            variable ==
              "Elevation" ~ "The number of insects recorded did not change significantly with elevation.",
            variable ==
              "Temperature" ~ "The number of insects recorded did not change significantly with temperature.",
            variable ==
              "Longitude" ~ "The number of insects recorded did not change significantly with longitude.",
            variable ==
              "Latitude" ~ "The number of insects recorded did not change significantly with latitude.",
            variable ==
              "Proportion Forest" ~ "The number of insects recorded did not change significantly with proportion of forest cover.",
            variable ==
              "Proportion Shrubland" ~ "The number of insects recorded did not change significantly with proportion of shrubland cover.",
            variable ==
              "Proportion Grassland" ~ "The number of insects recorded did not change significantly with proportion of grassland cover.",
            variable ==
              "Proportion Wetland" ~ "The number of insects recorded did not change significantly with proportion of wetland cover.",
            variable ==
              "Proportion Marine" ~ "The number of insects recorded did not change significantly with proportion of marine cover.",
            variable ==
              "Proportion Arable" ~ "The number of insects recorded did not change significantly with proportion of arable land cover.",
            variable ==
              "Proportion Urban" ~ "The number of insects recorded did not change significantly with proportion of urban cover.",
            .default = sprintf(
              "The number of insects recorded did not change significantly with %s.",
              variable
            )
          ),
          description = ifelse(
            p_value < 0.05,
            sig_description,
            nonsig_description
          ),
          confidence_interval = mapply(
            function(low, high) {
              interval <- sort(c(low, high))
              sprintf(
                "%s to %s",
                format(round(interval[1] / 100, 3), nsmall = 3),
                format(round(interval[2] / 100, 3), nsmall = 3)
              )
            },
            low,
            high
          ),
          p_value = scales::pvalue(p_value, accuracy = 0.001),
          est = round(est / 100, 3)
        ) %>%
        dplyr::select(
          variable,
          description,
          est,
          confidence_interval,
          p_value
        ) %>%
        dplyr::arrange(
          p_value > 0.05,
          dplyr::desc(abs(est)),
        ) %>%
        reactable::reactable(
          columns = list(
            variable = reactable::colDef(
              name = "Variable"
            ),
            description = reactable::colDef(
              name = "Description"
            ),
            est = reactable::colDef(
              name = "Effect estimate",
              format = reactable::colFormat(digits = 3)
            ),
            confidence_interval = reactable::colDef(
              name = "Effect confidence interval"
            ),
            p_value = reactable::colDef(
              name = "p value"
            )
          ),
          defaultColDef = reactable::colDef(
            align = "left"
          ),
          sortable = TRUE,
          filterable = FALSE,
          defaultPageSize = 100,
          height = "100%",
          showSortable = TRUE,
          rownames = FALSE
        )
    })

    add_forest_trace <- function(p, model_data, color) {
      plotly::add_trace(
        p,
        showlegend = FALSE,
        data = model_data,
        x = ~est,
        y = ~variable,
        type = "scatter",
        mode = "markers+text",
        error_x = list(
          type = "data",
          symmetric = FALSE,
          array = model_data$est - model_data$low,
          arrayminus = model_data$est - model_data$low,
          color = color
        ),
        marker = list(size = 10, color = color),
        text = ~label,
        textposition = "top center",
        hoverinfo = "text",
        hovertext = ~ paste0(
          "Variable: ",
          variable,
          "<br>",
          "Estimate: ",
          label,
          "<br>",
          "CI: [",
          round(low, 2),
          ", ",
          round(high, 2),
          "]<br>"
        )
      )
    }

    output$forest <- plotly::renderPlotly({
      model_data <- forest_data() %>%
        dplyr::mutate(
          variable = factor(variable, levels = variable),
          sig = dplyr::case_when(
            p_value < 0.001 ~ "***",
            p_value < 0.01 ~ "**",
            p_value < 0.05 ~ "*",
            TRUE ~ ""
          ),
          color = dplyr::case_when(
            p_value > 0.05 ~ "darkgray",
            est < 0 ~ "red",
            est > 0 ~ "green",
            .default = "darkgray"
          )
        ) %>%
        dplyr::mutate(
          label = paste(format(round(est, 2), nsmall = 2), sig)
        )
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
            x0 = 1,
            x1 = 1,
            y0 = -0.5,
            y1 = length(model_data$variable) - 0.5,
            line = list(color = "lightgray", width = 1),
            layer = "below"
          )),
          dragmode = FALSE
        ) %>%
        plotly::config(
          displayModeBar = FALSE
        )
    })

    output$model_predicted <- plotly::renderPlotly({
      model_data <- "SELECT * FROM analysis.model_predictions WHERE region = {input$region};" %>%
        glue::glue_sql(.con = conn) %>%
        DBI::dbGetQuery(conn, .)

      int_ticks <- sort(unique(model_data$year))
      plotly::plot_ly(showlegend = FALSE) %>%
        # Shaded confidence interval (ribbon)
        plotly::add_ribbons(
          data = model_data,
          x = ~year,
          ymin = ~low,
          ymax = ~high,
          line = list(color = "transparent"),
          fillcolor = "rgba(0,0,0,0.2)",
          name = "95% CI",
          hoverinfo = "none"
        ) %>%
        # Line for predicted values
        plotly::add_lines(
          data = model_data,
          x = ~year,
          y = ~predicted,
          line = list(color = "black", width = 3),
          hoverinfo = "text",
          hovertext = ~ paste0(
            "Year: ",
            year,
            "<br>",
            "Predicted splat count: ",
            round(predicted, 2),
            "<br>",
            "CI: [",
            round(low, 2),
            ", ",
            round(high, 2),
            "]"
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
        plotly::config(displayModeBar = FALSE)
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
      AND end_timestamp >= '2021-06-01'::TIMESTAMP
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
    })

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
