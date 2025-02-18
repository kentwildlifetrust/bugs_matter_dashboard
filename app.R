# app colours to match mobile app
# find some journeys from other countries? 

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(tidyr)
library(pool)
library(RPostgres)
library(ggplot2)
library(MASS)
library(sjPlot)
library(scales)
library(shinycssloaders)
library(shinyFeedback)
library(shinydashboard)
library(shinyjs)
library(bslib)

kwt_portal_theme <- function(){
  bslib::bs_theme(
    bg = "#C6E5E8",
    fg = "#0F0F0F",
    primary = "#2374FF",
    secondary = "#02606F",
    success = "#00A36A",
    warning = "#FFAB00",
    danger = "#FF5630",
    info = "#00272D",
    base_font = "Rubik",
    font_scale = 0.9,
    "btn-padding-x" = ".5rem",
    "btn-padding-y" = ".25rem",
    "border-radius" = "4px",
    "bslib-spacer" = "12px",
    "card-bg" = "#FFFFFF",
    "table-bg" = "#FFFFFF",
    "dropdown-padding-x" = ".5rem",
    "dropdown-bg" = "#FFFFFF"
  ) %>%
    bslib::bs_add_rules(
      readLines(system.file("www", "bs_rules.css", package = "shinyHelper")) %>%
        paste(collapse = "")
    )
}

colorBlind7  <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Define UI
ui <- fluidPage(
  useShinyjs(),
  theme = kwt_portal_theme(),  # Apply the custom theme
  
  # Custom CSS to add border around the Leaflet map
  tags$style(HTML("
    .bordered-map {
      border: 2px solid black; /* Black border */
      padding: 0px; /* Optional: Add padding inside the border */
    }
    /* Add custom positioning for the logo */
    #logo {
      position: fixed;
      bottom: 10px;
      left: -60px;
      z-index: 1000; /* Ensure it's on top of other elements */
      width: 100px; /* Adjust the size of the logo */
    }
  ")),
  
  # Navbar with tabs
  navbarPage(id = "tabs",
             title = "Bugs Matter Results Dashboard",
             tabPanel("Welcome", 
                      h2("Welcome to the Bugs Matter Dashboard!"),
                      p("Here you can explore the Bugs Matter data and analyze trends. You can pan and zoom the map and hover over plots to learn more. ")
             ),
             
             tabPanel("Data Explorer",
                      sidebarLayout(
                        sidebarPanel(
                          selectizeInput("country", "Select a Country:", choices = NULL,
                                         options = list(
                                           placeholder = 'Please select an option',
                                           onInitialize = I('function() { this.setValue(""); }')
                                         )
                          ),
                          selectInput("region", "Select a Region:", choices = NULL),
                          selectInput("year", "Select a Year", choices = NULL),
                          br(),
                          textOutput("de_text")
                        ),
                        
                        mainPanel(
                          # Wrap the leafletOutput in a div with the custom class
                          tags$div(class = "bordered-map", 
                                   leafletOutput("map", height = "50vh")),  # Leaflet map
                          br(),  # Add space
                          fluidRow(
                            column(4, plotOutput("plot1", height = '30vh') |> tooltip("This line plot shows the cumulative number of journeys recorded. Over time, more and more journeys are recorded.", placement = "auto") %>% 
                                     withSpinner(type=7, color = "#75DA40")),
                            column(4, plotOutput("plot2", height = '30vh') |> tooltip("This histogram plot shows how many journeys of different lengths were recorded. There tends to be more shorter journeys than longer journeys.", placement = "auto") %>%
                                     withSpinner(type=7, color = "#75DA40")),
                            column(4, plotOutput("plot3", height = '30vh') |> tooltip("This bar plot shows how many journeys were recorded in different vehicle types. Most journeys are recorded in cars.", placement = "auto") %>% 
                                     withSpinner(type=7, color = "#75DA40"))
                          )
                        )
                      )
             ),
             
             tabPanel("Trend Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          selectizeInput("country_trend", "Select a Country:", choices = NULL,
                                         options = list(
                                           placeholder = 'Please select an option',
                                           onInitialize = I('function() { this.setValue(""); }')
                                         )
                          ),
                          selectInput("region_trend", "Select a Region:", choices = NULL),
                          selectInput("year_baseline", "Select a baseline year:", choices = NULL),
                          selectInput("year_comparison", "Select a comparison year:", choices = NULL),
                          loadingButton("calculate_trend", "Calculate Trend", style = "color: white; background-color: #75DA40;"),
                          br(),
                          br(),
                          textOutput("ta_text")
                        ),
                        
                        mainPanel(
                          fluidRow(
                            column(5, plotOutput("trendPlot1", height = '40vh') |> tooltip("This line plot shows how the mean splat rate changes over time. This result doesn't take into account all the other factors that could affect how many insects are splatted, so it should be interpreted with caution.", placement = "auto")),
                            column(5, plotOutput("trendPlot2", height = '40vh') |> tooltip("This boxplot with jittered data points shows the spread of the insect splat rate (splats per cm per mile) data. The boxes indicate the interquartile range (central 50% of the data), either side of the median splat rate which is shown by the horizontal line inside the box. The vertical lines extend out by 1.5 times the interquartile range, and the data points themselves are ‘horizontally jittered’ so they do not overlap to improve visualization. The thick green line at y = 0 are the data points for journeys where no bug splats were recorded.", placement = "auto")),
                            column(2, valueBoxOutput("trendStat2", width = NULL))
                            ),
                          br(),  # Add space
                          fluidRow(
                            column(5, plotOutput("trendPlot3", height = '40vh') |> tooltip("This forest plot of incidence rate ratios from the Negative Binomial statistical model shows the quantity of change (a multiplier) in the splat rate (splats per cm per mile) given a one-unit change in the independent variables, while holding other variables in the model constant. Significant relationships between splat rate and independent variables are shown by asterisks (* p < 0.05, ** p < 0.01, *** p < 0.001). Vehicle types are compared to the reference category of ‘cars’.", placement = "auto")),
                            column(5, plotOutput("trendPlot4", height = '40vh') |> tooltip("This plot shows the predicted splat counts from the Negative Binomial statistical model for each year. These are the most reliable results because the statistical model takes into account other factors that could affect how many insects are splatted.", placement = "auto")),
                            column(2, valueBoxOutput("trendStat1", width = NULL))
                            )
                        )
                      )
             ),
             
             tabPanel("Participation",
                      sidebarLayout(
                        sidebarPanel(
                          selectizeInput("country_user", "Select a Country:", choices = NULL,
                                         options = list(
                                           placeholder = 'Please select an option',
                                           onInitialize = I('function() { this.setValue(""); }')
                                         )
                          ),
                          selectInput("region_user", "Select a Region:", choices = NULL),
                          br(),
                          textOutput("us_text")
                        ),
                        
                        mainPanel(
                          # Wrap the leafletOutput in a div with the custom class
                          tags$div(class = "bordered-map", 
                                   leafletOutput("usermap", height = "50vh")),  # Leaflet map
                          br(),  # Add space
                          fluidRow(column(4, plotOutput("userPlot1", height = '30vh') |> tooltip("This line plot shows the increase in registered citizen scientists over time.", placement = "auto") %>% 
                                     withSpinner(type=7, color = "#75DA40")),
                                   column(4, valueBoxOutput("userStat1", width = NULL)),
                                   column(4, valueBoxOutput("userStat2", width = NULL))
                          )
                        )
                      )
             )
  ),
  # Add the logo at the bottom-left of the page
  tags$div(
    id = "logo", 
    tags$img(src = "https://cdn.buglife.org.uk/2022/04/Bugs-Matter-Landscape.png", height = "150px")  # Adjust height as needed
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observeEvent(c(input$tabs), {
    runjs("window.dispatchEvent(new Event('resize'));")
  })
  
  # set up pool for postgis connections
  pool <- dbPool(drv = RPostgres::Postgres(), 
                 host = "kwt-postgresql-azdb-1.postgres.database.azure.com", 
                 port = 5432, dbname = "shared", 
                 user = Sys.getenv("BUGS_MATTER_USER"),
                 password = Sys.getenv("BUGS_MATTER_PASSWORD"),
                 sslmode = "prefer")
  
  #### Data Explorer ####
  
  # Populate country dropdown
  observe({
    query <- "SELECT DISTINCT country FROM bugs_matter.regionboundaries"
    countries <- sort(dbGetQuery(pool, query)$country)
    updateSelectInput(session, "country", choices = countries)
  })
  
  # Update regions based on selected country
  observeEvent(input$country, {
    req(input$country)
    query <- paste0("SELECT DISTINCT nuts118nm FROM bugs_matter.regionboundaries 
    WHERE country = '", input$country, "'")
    regions <- dbGetQuery(pool, query)$nuts118nm
    # Add "All" only if there are multiple regions
    if (length(regions) > 1) {
      regions <- c("All", regions)
    }
    updateSelectInput(session, "region", choices = regions, selected = ifelse("All" %in% regions, "All", regions[1]))
  })
  
  # Populate year dropdown based on selected country and region
  observeEvent(input$region, {
    req(input$country)  # Ensure country is selected
    
    if (input$region == "All") {
      # Query for years across all regions in the selected country
      query <- sprintf(
        "SELECT DISTINCT year FROM bugs_matter.journeys5 WHERE country = '%s' AND CAST(year AS INTEGER) >= 2021",
        input$country
      )
    } else {
      # Query for years in the selected region and country
      query <- sprintf(
        "SELECT DISTINCT year FROM bugs_matter.journeys5 WHERE region = '%s' AND country = '%s' AND CAST(year AS INTEGER) >= 2021",
        input$region, input$country
      )
    }
    
    years <- sort(dbGetQuery(pool, query)$year, decreasing = TRUE)
    updateSelectInput(session, "year", choices = years)
  })
  
  output$de_text <- renderText({paste0("Displaying ", input$year, " results for ", input$region, ", ", input$country) })

  # Render Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 0, lat = 50, zoom = 3) # Default view
  })
  
  # Update map when a region or year is selected
  observeEvent(c(input$region, input$year), {
    req(input$region, input$country, input$year)  # Ensure all inputs are selected
    
    # Fetch geometry for the selected region
    query <- if(input$region == "All") {
      sprintf(
        "SELECT nuts118nm, country, ST_Transform(ST_Simplify(geometry, 1000), 4326) AS geom 
   FROM bugs_matter.regionboundaries 
   WHERE country = '%s'",
        input$country
      )
    }
    else{
      sprintf(
        "SELECT nuts118nm, country, ST_Transform(ST_Simplify(geometry, 1000), 4326) AS geom 
   FROM bugs_matter.regionboundaries 
   WHERE nuts118nm = '%s' AND country = '%s'",
        input$region, input$country
      )
    }
    
    region_poly <- st_read(pool, query = query)
    
    # Fetch journeys for the selected region and year
    query <- if(input$region == "All") {
      sprintf(
        "SELECT id, region, year, ST_Transform(ST_Simplify(geometry, 1000), 4326) AS geom 
     FROM bugs_matter.journeys5 
     WHERE country = '%s' AND year = '%s'",
        input$country, input$year
      )
    }
    else{
      sprintf(
      "SELECT id, region, year, ST_Transform(ST_Simplify(geometry, 1000), 4326) AS geom 
     FROM bugs_matter.journeys5 
     WHERE region = '%s' AND country = '%s' AND year = '%s'",
      input$region, input$country, input$year
    )
    }
    region_journeys <- st_read(pool, query = query)
    
    # Get the bounding box of the polygon
    bbox <- st_bbox(region_journeys) %>% as.list()  # Bounding box: xmin, ymin, xmax, ymax
    
    # Update map with region boundary
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = region_poly, color = "#000000", weight = 2, fillOpacity = 0) %>%
      fitBounds(
        lng1 = bbox$xmin, lat1 = bbox$ymin,
        lng2 = bbox$xmax, lat2 = bbox$ymax
      ) %>%
      addPolylines(data = region_journeys, color = "#75DA40", weight = 1)
  })
  
  # Render the three plots
  output$plot1 <- renderPlot({
    req(input$region, input$country, input$year)  # Ensure inputs are available
    # Fetch specific data for Plot 1 based on selected filters
    query <- if(input$region == "All") {
      sprintf(
        "SELECT start, dayofyear, region, country, year FROM bugs_matter.journeys5 
      WHERE country = '%s' AND year = '%s'",
        input$country, input$year
      )
    }
    else{
      sprintf(
      "SELECT start, dayofyear, region, country, year FROM bugs_matter.journeys5 
      WHERE region = '%s' AND country = '%s' AND year = '%s'",
      input$region, input$country, input$year
    )
    }
    plot_data <- dbGetQuery(pool, query)
    
    # Ensure daily journey counts and cumulative sum are computed
    plot_data <- plot_data %>% arrange(start) %>% group_by(dayofyear) %>% 
      summarise(n = n(), .groups = "drop") %>%  # Count journeys for each day
      mutate(cumulative_n = cumsum(n))  # Calculate cumulative sum
    
    # Cumulative journey counts 
    ggplot(plot_data, aes(x=dayofyear, y=cumulative_n)) + 
      geom_line(lwd=0.5, color = "black") + 
      labs(title = "Number of Recorded Journeys", x = "Journey Date", y = "Number of Journeys") +
      scale_x_continuous(breaks = c(1,32,60,91,121,152,182,213,244,274,305,335), 
                         minor_breaks = c(1,32,60,91,121,152,182,213,244,274,305,335),
                         labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
      theme_minimal(base_size = 12) + 
      theme(
        plot.margin = margin(rep(10,4)),
        plot.background = element_rect(fill = "#C6E5E8", color = NA),  # Set background color
        panel.background = element_blank(),  # No panel background
        panel.border = element_blank(),  # Remove panel border
        text = element_text(family = "Rubik"),
        # Add space between title and plot
        plot.title = element_text(margin = margin(b = 20)),  # Increase space between title and plot
        # Make axis tick labels bold
        axis.text.x = element_text(face = "bold", color = "black"),
        axis.text.y = element_text(face = "bold", color = "black"),
        # Remove grid lines and background for minor axis
        panel.grid.major = element_line(color = "#9FC8B8", size = 0.5),
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        axis.title.x = element_text(margin = margin(t = 15)),  # Add space between x-axis title and plot
        axis.title.y = element_text(margin = margin(r = 15))   # Add space between y-axis title and plot
      )
  })
  
  output$plot2 <- renderPlot({
    req(input$region, input$country, input$year)  # Ensure inputs are available
    
    # Fetch specific data for Plot 2 based on selected filters
    query <- if(input$region == "All") {
      sprintf(
        "SELECT start, region, distance, country, year FROM bugs_matter.journeys5 
      WHERE country = '%s' AND year = '%s'",
        input$country, input$year
      )
    }
    else{
      sprintf(
      "SELECT start, region, distance, country, year FROM bugs_matter.journeys5 
      WHERE region = '%s' AND country = '%s' AND year = '%s'",
      input$region, input$country, input$year
    )
    }
    plot_data <- dbGetQuery(pool, query)
    
    # Histogram of journey distances
    ggplot(plot_data, aes(x=distance)) + 
      geom_histogram(binwidth = 5, fill = "black") +
      labs(title = "Journey Distances", x = "Distance (miles)", y = "Number of Journeys") +
      theme_minimal(base_size = 12) + 
      theme(
        plot.margin = margin(rep(10,4)),
        plot.background = element_rect(fill = "#C6E5E8", color = NA),  # Set background color
        panel.background = element_blank(),  # No panel background
        panel.border = element_blank(),  # Remove panel border
        text = element_text(family = "Rubik"),
        # Add space between title and plot
        plot.title = element_text(margin = margin(b = 20)),  # Increase space between title and plot
        # Make axis tick labels bold
        axis.text.x = element_text(face = "bold", color = "black"),
        axis.text.y = element_text(face = "bold", color = "black"),
        # Remove grid lines and background for minor axis
        panel.grid.major = element_line(color = "#9FC8B8", size = 0.5),
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        axis.title.x = element_text(margin = margin(t = 15)),  # Add space between x-axis title and plot
        axis.title.y = element_text(margin = margin(r = 15))   # Add space between y-axis title and plot
      )
    
  })
  
  output$plot3 <- renderPlot({
    req(input$region, input$country, input$year)  # Ensure inputs are available
    
    # Fetch specific data for Plot 2 based on selected filters
    query <- if(input$region == "All") {
      sprintf(
        "SELECT start, region, vehicle_cl, country, year FROM bugs_matter.journeys5 
      WHERE country = '%s' AND year = '%s'",
        input$country, input$year
      )
    }
    else{
      sprintf(
      "SELECT start, region, vehicle_cl, country, year FROM bugs_matter.journeys5 
      WHERE region = '%s' AND country = '%s' AND year = '%s'",
      input$region, input$country, input$year
    )
    }
    plot_data <- dbGetQuery(pool, query)
    
    # Count the number of journeys for each vehicle type
    journey_counts <- plot_data %>%
      group_by(vehicle_cl) %>%
      summarise(journey_count = n())
    
    # Create the bar chart
    ggplot(journey_counts, aes(x = vehicle_cl, y = journey_count, fill = vehicle_cl)) +
      geom_bar(stat = "identity", fill = c("black")) +  # Use 'identity' to use the actual journey count values
      labs(title = "Vehicle Types", x = "Vehicle Type", y = "Number of journeys") +
      theme_minimal(base_size = 12) + 
      theme(
        plot.margin = margin(rep(10,4)),
        plot.background = element_rect(fill = "#C6E5E8", color = NA),  # Set background color
        panel.background = element_blank(),  # No panel background
        panel.border = element_blank(),  # Remove panel border
        text = element_text(family = "Rubik"),
        # Add space between title and plot
        plot.title = element_text(margin = margin(b = 20)),  # Increase space between title and plot
        # Make axis tick labels bold
        axis.text.x = element_text(face = "bold", color = "black"),
        axis.text.y = element_text(face = "bold", color = "black"),
        # Remove grid lines and background for minor axis
        panel.grid.major = element_line(color = "#9FC8B8", size = 0.5),
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        axis.title.x = element_text(margin = margin(t = 15)),  # Add space between x-axis title and plot
        axis.title.y = element_text(margin = margin(r = 15))   # Add space between y-axis title and plot
      )

  })
  
  #### Trend Analysis ####
  
  # Populate country dropdown
  observe({
    query <- "SELECT DISTINCT country FROM bugs_matter.regionboundaries"
    countries <- sort(dbGetQuery(pool, query)$country)
    updateSelectInput(session, "country_trend", choices = countries)
  })
  
  # Update regions based on selected country
  observeEvent(input$country_trend, {
    req(input$country_trend)
    query <- paste0("SELECT DISTINCT nuts118nm FROM 
                    bugs_matter.regionboundaries WHERE 
                    country = '", input$country_trend, "'")
    regions <- dbGetQuery(pool, query)$nuts118nm
    # Add "All" only if there are multiple regions
    if (length(regions) > 1) {
      regions <- c("All", regions)
    }
    updateSelectInput(session, "region_trend", choices = regions, selected = ifelse("All" %in% regions, "All", regions[1]))
  })
  
  # Populate year dropdown based on selected country and region
  observeEvent(input$region_trend, {
    req(input$country_trend)  # Ensure country is selected
    if (input$region_trend == "All") {
      # Query for years across all regions in the selected country
      query <- sprintf(
        "SELECT DISTINCT year FROM bugs_matter.journeys5 WHERE country = '%s' AND CAST(year AS INTEGER) >= 2021",
        input$country_trend
      )
    } else {
      # Query for years in the selected region and country
      query <- sprintf(
        "SELECT DISTINCT year FROM bugs_matter.journeys5 WHERE region = '%s' AND country = '%s' AND CAST(year AS INTEGER) >= 2021",
        input$region_trend, input$country_trend
      )
    }
    years <- sort(dbGetQuery(pool, query)$year, decreasing = FALSE)
    updateSelectInput(session, "year_baseline", choices = years)
  })
  
  # Populate year dropdown based on selected country and region
  observeEvent(input$year_baseline, {
    req(input$country_trend, input$year_baseline)  # Ensure both inputs are selected
    if (input$region_trend == "All") {
      # Query for years across all regions in the selected country
      query <- sprintf(
        "SELECT DISTINCT year FROM bugs_matter.journeys5 WHERE country = '%s' AND CAST(year AS INTEGER) >= 2021",
        input$country_trend
      )
    } else {
      # Query for years in the selected region and country
      query <- sprintf(
        "SELECT DISTINCT year FROM bugs_matter.journeys5 WHERE region = '%s' AND country = '%s' AND CAST(year AS INTEGER) >= 2021",
        input$region_trend, input$country_trend
      )
    }
    years <- sort(dbGetQuery(pool, query)$year, decreasing = TRUE)
    years <- years[years > input$year_baseline]
    updateSelectInput(session, "year_comparison", choices = years)
  })
  
  observeEvent(input$calculate_trend, {
    req(input$region_trend, input$country_trend, input$year_baseline, input$year_comparison)
    
    output$ta_text <- renderText({paste0("Displaying ", input$year_baseline, "-", input$year_comparison, " results for ", input$region_trend, ", ", input$country_trend) })
    
    query <- if (input$region_trend == "All") {
      sprintf(
        "SELECT region, country, splatcount, splat_rate, year, distance, avg_speed, vehicle_cl, vehicle_he, vehicle_wi, midpoint_time, dayofyear, \"X\", \"Y\", log_cm_miles_offset 
      FROM bugs_matter.journeys5 
      WHERE country = '%s' AND year >= '%s' AND year <= '%s'",
        input$country_trend, input$year_baseline, input$year_comparison
      )
    }
    else{
      sprintf(
      "SELECT region, country, splatcount, splat_rate, year, distance, avg_speed, vehicle_cl, vehicle_he, vehicle_wi, midpoint_time, dayofyear, \"X\", \"Y\", log_cm_miles_offset 
      FROM bugs_matter.journeys5 
      WHERE region = '%s' AND country = '%s' AND year >= '%s' AND year <= '%s'",
      input$region_trend, input$country_trend, input$year_baseline, input$year_comparison
    )
    }
    mod_data <- dbGetQuery(pool, query)
    mod_data <- mod_data %>% rename(
      Year = year,
      Distance = distance,
      Average.speed = avg_speed,
      Vehicle.type = vehicle_cl,
      Vehicle.height = vehicle_he,
      Vehicle.width = vehicle_wi, 
      Day.of.year = dayofyear, 
      Longitude = X,
      Latitude = Y
    )
    mod_data$Time.of.day <- as.numeric(difftime(mod_data$midpoint_time, trunc(mod_data$midpoint_time, units="days"), units="hours"))
    mod_data$Year <- relevel(as.factor(mod_data$Year), ref=input$year_baseline)
    mod <- tryCatch(glm.nb(splatcount ~ Year + 
                             Distance + 
                             Average.speed + 
                             Time.of.day +
                             Day.of.year +
                             #Vehicle.type +
                             Vehicle.height +
                             Vehicle.width +
                             Longitude +
                             Latitude +
                             offset(log_cm_miles_offset), data = mod_data),
                    error = function(e) {
                      warning(paste("Model failed for region:", input$region_trend, "Error:", e$message))
                      return(NULL)
                    }
    )
    
    # print(summary(mod))
    # VIFtable <- check_collinearity(mod, component = "count")
    # print(VIFtable)
    est <- cbind(Estimate = exp(coef(mod)), exp(confint(mod)))
    comparison_year_coefs <- est[grepl(input$year_comparison, rownames(est)), ]
    comparison_year_coefs1 <- round(((1 - comparison_year_coefs) * 100) * -1, 1)
    comparison_year_coefs1 <- ifelse(comparison_year_coefs1 > 0, paste("+", comparison_year_coefs1), paste("-", abs(comparison_year_coefs1)))
    
    # Plot 1 - Mean splat rate over time
    output$trendPlot1 <- renderPlot({
      ggplot(data = mod_data, aes(x = midpoint_time)) + 
        geom_line(lwd=0.5, data = mod_data, aes(y=cummean(splat_rate)), color="black") + 
        scale_color_manual(values=c(colorBlind7), name="Year") + 
        theme_minimal(base_size = 12) +
        labs(title = "Average Splat Rate Over Time", x = "Journey date", y = "Splat rate (splats/cm/mile)") +
        theme(
          plot.margin = margin(rep(10,4)),
          plot.background = element_rect(fill = "#C6E5E8", color = NA),  # Set background color
          panel.background = element_blank(),  # No panel background
          panel.border = element_blank(),  # Remove panel border
          text = element_text(family = "Rubik"),
          # Add space between title and plot
          plot.title = element_text(margin = margin(b = 20)),  # Increase space between title and plot
          # Make axis tick labels bold
          axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"),
          # Remove grid lines and background for minor axis
          panel.grid.major = element_line(color = "#9FC8B8", size = 0.5),
          panel.grid.minor = element_blank(),  # Remove minor gridlines
          axis.title.x = element_text(margin = margin(t = 15)),  # Add space between x-axis title and plot
          axis.title.y = element_text(margin = margin(r = 15))   # Add space between y-axis title and plot
        )
    })
    
    # Plot 2 - Boxplot with jittered data points
    output$trendPlot2 <- renderPlot({
      mysqrt_trans <- function() {
        trans_new("mysqrt",
                  transform = base::sqrt,
                  inverse = function(x) ifelse(x < 0, 0, x^2),
                  domain = c(0, Inf))
      }
      # Generate the plot with the custom transformation applied to y-axis
      ggplot(mod_data, aes(x = Year, y = splat_rate, fill = Year)) +
        stat_boxplot(geom = 'errorbar', width = 0.5) +
        geom_boxplot(outlier.shape = NA, fill = NA) +
        geom_jitter(position = position_jitter(width = 0.2, height = 0), alpha = 0.3, color = "black") +
        scale_y_continuous(trans = mysqrt_trans()) + 
        theme_minimal(base_size = 12) +
        labs(title = "Splat Rate by Year", x = "Year", y = "Splat rate (sqrt transformation)") +
        expand_limits(y = 0) + 
        theme(
          legend.position="none",
          plot.margin = margin(rep(10,4)),
          plot.background = element_rect(fill = "#C6E5E8", color = NA),  # Set background color
          panel.background = element_blank(),  # No panel background
          panel.border = element_blank(),  # Remove panel border
          text = element_text(family = "Rubik"),
          # Add space between title and plot
          plot.title = element_text(margin = margin(b = 20)),  # Increase space between title and plot
          # Make axis tick labels bold
          axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"),
          # Remove grid lines and background for minor axis
          panel.grid.major = element_line(color = "#9FC8B8", size = 0.5),
          panel.grid.minor = element_blank(),  # Remove minor gridlines
          axis.title.x = element_text(margin = margin(t = 15)),  # Add space between x-axis title and plot
          axis.title.y = element_text(margin = margin(r = 15))   # Add space between y-axis title and plot
        )
    })
    
    # Plot 3 - Forest plot
    output$trendPlot3 <- renderPlot({
      plot_model(mod,
                 vline.color = "grey",
                 type="est",
                 show.values = T,
                 p.val="wald",
                 show.p = T,
                 sort.est=T,
                 title="",
                 ci.lvl=0.95,
                 line.size=0.6,
                 show.zeroinf = F,
                 digits = 3,
                 value.offset = 0.35,
                 value.size = 3.5,
                 dot.size=1) +
        theme_minimal(base_size = 12) + 
        labs(title = "Change in splats in response to variables", x = "Explanatory variable") +
        theme(
          plot.margin = margin(rep(10,4)),
          plot.background = element_rect(fill = "#C6E5E8", color = NA),  # Set background color
          panel.background = element_blank(),  # No panel background
          panel.border = element_blank(),  # Remove panel border
          text = element_text(family = "Rubik"),
          # Add space between title and plot
          plot.title = element_text(margin = margin(b = 20)),  # Increase space between title and plot
          # Make axis tick labels bold
          axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"),
          # Remove grid lines and background for minor axis
          panel.grid.major = element_line(color = "#9FC8B8", size = 0.5),
          panel.grid.minor = element_blank(),  # Remove minor gridlines
          axis.title.x = element_text(margin = margin(t = 15)),  # Add space between x-axis title and plot
          axis.title.y = element_text(margin = margin(r = 15))   # Add space between y-axis title and plot
        )
    })
    
    # Plot 4 - Change in splat count over time
    output$trendPlot4 <- renderPlot({
      plot_model(mod, 
                 type="pred", 
                 terms="Year", 
                 colors = "black") +
        theme_minimal(base_size = 12) +
        labs(title = "Model predictions of splat count", y = "Splat count") +
        theme(
          plot.margin = margin(rep(10,4)),
          plot.background = element_rect(fill = "#C6E5E8", color = NA),  # Set background color
          panel.background = element_blank(),  # No panel background
          panel.border = element_blank(),  # Remove panel border
          text = element_text(family = "Rubik"),
          # Add space between title and plot
          plot.title = element_text(margin = margin(b = 20)),  # Increase space between title and plot
          # Make axis tick labels bold
          axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"),
          # Remove grid lines and background for minor axis
          panel.grid.major = element_line(color = "#9FC8B8", size = 0.5),
          panel.grid.minor = element_blank(),  # Remove minor gridlines
          axis.title.x = element_text(margin = margin(t = 15)),  # Add space between x-axis title and plot
          axis.title.y = element_text(margin = margin(r = 15))   # Add space between y-axis title and plot
        )
    })
    
    output$trendStat1 <- renderValueBox({
      valueBox(
        value = HTML(paste0("<b>", comparison_year_coefs1[1], "%", "</b>", "<br>", "between", "<br>", input$year_baseline, "-", input$year_comparison)),
        subtitle = HTML(paste0("Confidence Interval (CI 95%): ", "<br>", comparison_year_coefs1[3], "%", 
                          " to ", comparison_year_coefs1[2], "%")),
        icon = icon("arrow-trend-down"),
      )
    })
    
    output$trendStat2 <- renderValueBox({
      valueBox(
        value = paste0(format(sum(mod_data$splatcount), big.mark = ",", scientific = FALSE), " bug splats"),
        subtitle = paste0("over ", format(round(sum(mod_data$Distance), 0), big.mark = ",", scientific = FALSE), " miles"),
        icon = icon("car"),
      )
    })
    
    
    resetLoadingButton("calculate_trend")
    
  })
  
  #### Participation ####
  
  # Populate country dropdown
  observe({
    query <- "SELECT DISTINCT country FROM bugs_matter.regionboundaries"
    countries <- sort(dbGetQuery(pool, query)$country)
    updateSelectInput(session, "country_user", choices = countries)
  })
  
  # Update regions based on selected country
  observeEvent(input$country_user, {
    req(input$country_user)
    query <- paste0("SELECT DISTINCT nuts118nm FROM bugs_matter.regionboundaries 
    WHERE country = '", input$country_user, "'")
    regions <- dbGetQuery(pool, query)$nuts118nm
    regions <- gsub(" *\\[.*?\\]| *\\(.*?\\)", "", regions)
    # Add "All" only if there are multiple regions
    if (length(regions) > 1) {
      regions <- c("All", regions)
    }
    updateSelectInput(session, "region_user", choices = regions, selected = ifelse("All" %in% regions, "All", regions[1]))
  })
  
  # Run the query and fetch the results as a dataframe
  query <- "
  SELECT country, region, COUNT(*) AS user_count
  FROM bugs_matter.user_data2
  GROUP BY country, region
  ORDER BY country, region;
  "
  user_counts <- dbGetQuery(pool, query) %>% mutate(region = coalesce(region, country))
  
  query <- "
  SELECT nuts118nm, country, ST_Transform(ST_MakeValid(ST_Simplify(geometry, 1000)), 4326) AS geom FROM bugs_matter.regionboundaries
  "
  region_boundaries <- st_read(pool, query = query)
  region_boundaries$nuts118nm <- gsub(" *\\[.*?\\]| *\\(.*?\\)", "", region_boundaries$nuts118nm)
  region_user_counts <- left_join(region_boundaries %>% rename(region = nuts118nm), user_counts, by = c("country", "region"))
  region_user_counts$user_count <- as.numeric(region_user_counts$user_count)
  region_user_counts_centroids <- region_user_counts %>% mutate(geom = st_centroid(geom))  # Replace multipolygons with their centroids
  
  # Render Leaflet map
  output$usermap <- renderLeaflet({
    pal <- colorNumeric(palette = "viridis", domain = region_user_counts$user_count)
    # Render the map
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = region_user_counts,
                  color = "#000000",  # Border color of polygons
                  weight = 2,
                  fillOpacity = 0.7,   # Fill opacity
                  fillColor = ~pal(user_count)) %>%  # Fill color based on 'user_count'
      addLegend(position = "bottomright",  # Position of the legend
                pal = pal,
                values = region_user_counts$user_count,
                title = "User Count",
                opacity = 0.7) %>%
      addLabelOnlyMarkers(data = region_user_counts_centroids,  # Use centroids for labeling
                          lng = ~st_coordinates(geom)[,1],  # Longitude of centroid
                          lat = ~st_coordinates(geom)[,2],  # Latitude of centroid
                          label = ~as.character(user_count),  # Label user count
                          labelOptions = labelOptions(noHide = TRUE, 
                                                      direction = "center", 
                                                      textOnly = TRUE, 
                                                      style = list("font-weight" = "bold", "color" = "black"))) %>%
      setView(lng = 0, lat = 50, zoom = 3)  # Default view
  })
  
  # Update map when a region is selected
  observeEvent(c(input$region_user), {
    req(input$region_user, input$country_user)  # Ensure all inputs are selected

    # Get the bounding box of the polygon
    bbox <- if(input$region_user == "All") {
      st_bbox(subset(region_user_counts, country == input$country_user)) %>% as.list()
    } 
    else { 
      st_bbox(subset(region_user_counts, region == input$region_user)) %>% as.list()  # Bounding box: xmin, ymin, xmax, ymax
    }
    
    # Update map with region boundary
    leafletProxy("usermap") %>%
      fitBounds(
        lng1 = bbox$xmin, lat1 = bbox$ymin,
        lng2 = bbox$xmax, lat2 = bbox$ymax
      )
  })
  
  output$userPlot1 <- renderPlot({
    req(input$region_user, input$country_user)  # Ensure inputs are available
    query <- if(input$region_user == "All") {  # Fetch specific data for Plot based on selected filters
      sprintf(
        "SELECT user_id, sign_up_date, region, country FROM bugs_matter.user_data2 
        WHERE country = '%s'",
        input$country_user
      )
    }
    else{
      sprintf(
        "SELECT user_id, sign_up_date, region, country FROM bugs_matter.user_data2
        WHERE (region = '%s' OR region IS NULL) AND country = '%s'",
        input$region_user, input$country_user
      )
    }
    
    plot_data <- dbGetQuery(pool, query)
    
    # Calculate cumulative sum
    plot_data <- plot_data %>% 
      arrange(sign_up_date) %>%  # Correct column reference
      filter(sign_up_date > "2021-01-01") %>%
      group_by(sign_up_date) %>% # Group by date before summarizing
      summarise(n = n(), .groups = "drop") %>%  # Count sign-ups per day
      mutate(cumulative_n = cumsum(n))  # Calculate cumulative sum
    
    # Cumulative count of user registration
    ggplot(plot_data, aes(x=sign_up_date, y=cumulative_n)) + 
      geom_line(lwd=0.5, color = "black") + 
      labs(title = "Cumulative Count of Sign-ups", x = "Sign-up Date", y = "Count of Sign-ups") +
      theme_minimal(base_size = 12) + 
      theme(
        plot.margin = margin(rep(10,4)),
        plot.background = element_rect(fill = "#C6E5E8", color = NA),  # Set background color
        panel.background = element_blank(),  # No panel background
        panel.border = element_blank(),  # Remove panel border
        text = element_text(family = "Rubik"),
        # Add space between title and plot
        plot.title = element_text(margin = margin(b = 20)),  # Increase space between title and plot
        # Make axis tick labels bold
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        # Remove grid lines and background for minor axis
        panel.grid.major = element_line(color = "#9FC8B8", size = 0.5),
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        axis.title.x = element_text(margin = margin(t = 15)),  # Add space between x-axis title and plot
        axis.title.y = element_text(margin = margin(r = 15))   # Add space between y-axis title and plot
      )
  })
  
  
  output$userStat1 <- renderValueBox({
    req(input$region_user, input$country_user)  # Ensure inputs are available
    query <- if(input$region_user == "All") {  # Fetch specific data for output based on selected filters
      sprintf(
        "SELECT user_id, sign_up_date, region, country, journeys FROM bugs_matter.user_data2
        WHERE country = '%s'",
        input$country_user
      )
    }
    else{
      sprintf(
        "SELECT user_id, sign_up_date, region, country, journeys FROM bugs_matter.user_data2
        WHERE (region = '%s' OR region IS NULL) AND country = '%s'",
        input$region_user, input$country_user
      )
    }

    statbox_data <- dbGetQuery(pool, query)

    #participant_count <- sum(is.na(statbox_data$journeys))
    #lazyuser_count <- sum(!is.na(statbox_data$journeys))
    conversion_rate <- sum(!is.na(statbox_data$journeys)) / nrow(statbox_data)
    conversion_rate1 <- paste0(round(conversion_rate*100, 1), "%")
    
    valueBox(
      value = conversion_rate1,
      subtitle = HTML("Participation rate <br> <br> This is the percentage of users that have recorded one or more journeys."),
      icon = icon("users"),
    )
  })
  
  output$userStat2 <- renderValueBox({
    req(input$region_user, input$country_user)  # Ensure inputs are available
    query <- if(input$region_user == "All") {  # Fetch specific data for Plot based on selected filters
      sprintf(
        "SELECT user_id, sign_up_date, region, country, journeys FROM bugs_matter.user_data2
        WHERE country = '%s'",
        input$country_user
      )
    }
    else{
      sprintf(
        "SELECT user_id, sign_up_date, region, country, journeys FROM bugs_matter.user_data2
        WHERE (region = '%s' OR region IS NULL) AND country = '%s'",
        input$region_user, input$country_user
      )
    }
    
    statbox_data <- dbGetQuery(pool, query)
    
    toprecorders <- sort(statbox_data$journeys, decreasing=TRUE)[1:3]
    
    top3_text <- paste0(
      "1st: ", toprecorders[1], " journeys<br>",
      "2nd: ", toprecorders[2], " journeys<br>",
      "3rd: ", toprecorders[3], " journeys"
    )
    
    valueBox(
      value = HTML(top3_text),
      subtitle = "Top recorders",
      icon = icon("trophy"),
    )
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
