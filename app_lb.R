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
library(slickR)

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
      paste0(
        readLines(system.file("www", "bs_rules.css", package = "shinyHelper")) %>%
          paste(collapse = ""),
        "
        .navbar-brand {
          font-family: 'Adelle', sans-serif !important;
          font-size: 1.4rem !important;
          font-weight: bold !important;
          color: #0F0F0F !important;
        }
        "
      )
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

tags$head(
  tags$style(HTML("
    body {
      background-image: url('Bugs_matter_footer.jpg'); /* Image file */
      background-size: 100% auto; /* Full width, automatic height */
      background-repeat: no-repeat; /* Prevent tiling */
      background-position: bottom center; /* Align at the top center */
      background-attachment: fixed; /* Keep it fixed while scrolling */
    }
  "))
),

  # Navbar with tabs
  navbarPage(id = "tabs",
             title = tagList(
               tags$img(src = "bm_bug_icon1.png", height = "50px", style = "margin-right: 10px;"),
               "Bugs Matter ", tags$b("Dashboard")
             ),
             windowTitle = "Bugs Matter Dashboard",  # Clean browser tab title
             tabPanel("Welcome",
                      slickROutput("welcome_slick_output", width='50%',height='500px')
             ),

             tabPanel("Journeys",
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
                          shiny::tags$br(),
                          textOutput("de_text")
                        ),

                        mainPanel(
                          # Wrap the leafletOutput in a div with the custom class
                          tags$div(class = "bordered-map",
                                   leafletOutput("map", height = "50vh")),  # Leaflet map
                          shiny::tags$br(),  # Add space
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

             tabPanel("Bug Splats",
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
                          shiny::tags$br(),
                          shiny::tags$br(),
                          textOutput("ta_text")
                        ),

                        mainPanel(
                          fluidRow(
                            column(5, plotOutput("trendPlot1", height = '40vh') |> tooltip("This line plot shows how the mean splat rate changes over time. This result doesn't take into account all the other factors that could affect how many insects are splatted, so it should be interpreted with caution.", placement = "auto")),
                            column(5, plotOutput("trendPlot2", height = '40vh') |> tooltip("This boxplot with jittered data points shows the spread of the insect splat rate (splats per cm per mile) data. The boxes indicate the interquartile range (central 50% of the data), either side of the median splat rate which is shown by the horizontal line inside the box. The vertical lines extend out by 1.5 times the interquartile range, and the data points themselves are ‘horizontally jittered’ so they do not overlap to improve visualization. The thick green line at y = 0 are the data points for journeys where no bug splats were recorded.", placement = "auto")),
                            column(2, valueBoxOutput("trendStat2", width = NULL))
                            ),
                          shiny::tags$br(),  # Add space
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
                          shiny::tags$br(),
                          textOutput("us_text")
                        ),

                        mainPanel(
                          # Wrap the leafletOutput in a div with the custom class
                          tags$div(class = "bordered-map",
                                   leafletOutput("usermap", height = "50vh")),  # Leaflet map
                          shiny::tags$br(),  # Add space
                          fluidRow(column(4, plotOutput("userPlot1", height = '30vh') |> tooltip("This line plot shows the increase in registered citizen scientists over time.", placement = "auto") %>%
                                     withSpinner(type=7, color = "#75DA40")),
                                   column(4, valueBoxOutput("userStat1", width = NULL)),
                                   column(4, valueBoxOutput("userStat2", width = NULL))
                          )
                        )
                      )
             ),
             tabPanel("Science",
                      ("This is a test sentence")
             )
  ),

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

  #### Welcome Page ####

  output$welcome_slick_output <- renderSlickR({
    text_slides <- list(
      div(style = "padding: 20px; background-color: #C6E5E8; border-radius: 0px;",
          h3(style = "text-align: center; color: #0F0F0F;", "Welcome to the Bugs Matter Dashboard"),
          p(style = "font-size: 18px; text-align: center; color: #0F0F0F;",
            HTML("Here, you can keep up-to-date with the findings of Bugs Matter, the global citizen science
            survey of ‘bug splats’ on vehicle number plates to monitor flying insect abundance. You can explore
            where and how many journeys have been recorded, and see information about the lengths of journeys,
            and the types of vehicles used. You can explore trends in the number of bugs splatted by location and over different time periods.
            You can also find information on the numbers of users in different parts of the world and how many journeys have been recorded by our top recorders!"))
      ),
      div(style = "padding: 20px; background-color: #C6E5E8; border-radius: 0px;",
          h3(style = "text-align: center; color: #0F0F0F;", "What is Bugs Matter?"),
          p(style = "font-size: 18px; text-align: center; color: #0F0F0F;",
            HTML("The Bugs Matter citizen science survey uses an innovative
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
been reported from empirical data (Møller, 2019), have been
interpreted as an indicator of major global declines in insect
abundance. ")),
          div("Invisible test 16:43",style="display:none;",id="test-div")

      ),
      div(style = "padding: 20px; background-color: #C6E5E8; border-radius: 0px;",
          h3(style = "text-align: center; color: #0F0F0F;", "Global insect declines"),
          p(style = "font-size: 18px; text-align: center; color: #0F0F0F;",
            HTML("A growing body of evidence (Fox et al., 2013; Hallmann et al.,
2017; Goulson, D. 2019; Sánchez-Bayo et al., 2019; Thomas
et al., 2019; van der Sluijs, 2020; Macadam et al., 2020;
Outhwaite, McCann and Newbold, 2022) highlights population
declines in insects and other invertebrates at UK and global
scales. These declines, which are evident across all functional
groups of insects (herbivores, detritivores, parasitoids,
predators and pollinators), could have catastrophic impacts
on the Earth’s natural systems and human survivability
on our planet. Invertebrates are functionally of greater
importance than large-bodied fauna, and in terms of biomass,
bioabundance and species diversity, they make up the
greatest proportion of life on Earth."))
      ),
      div(style = "padding: 20px; background-color: #C6E5E8; border-radius: 0px;",
          h3(style = "text-align: center; color: #0F0F0F;", "Drivers of insect declines"),
          p(style = "font-size: 18px; text-align: center; color: #0F0F0F;",
            HTML("Habitat loss and fragmentation has caused declines in
biodiversity across the world. The conversion of natural
habitats into agriculture, urban areas, and infrastructure
development leads to the loss of suitable habitats for insects,
making it difficult for them to feed and reproduce. The
widespread use of chemical pesticides, including insecticides,
herbicides, and fungicides, can have detrimental effects on
insect populations. Climate change alters weather patterns,
affecting insect life cycles, behaviour, and distribution. Some
insects may struggle to adapt to rapidly changing conditions
or may lose suitable habitats due to shifting climate zones."))
      ),
      div(style = "padding: 20px; background-color: #C6E5E8; border-radius: 0px;",
          h3(style = "text-align: center; color: #0F0F0F;", "How can you help?"),
          p(style = "font-size: 18px; text-align: center; color: #0F0F0F;",
            HTML("Using alternatives to peat fertilizer can help reduce CO2 emissions and slow the impact
            of climate change on insects and our environment. By eliminating or reducing our use of pesticides,
            we can stop the decline of thousands of insects in an instant. You can help the insects in your garden
            by letting the grass grow longer,  sowing wildflowers, and generally being less tidy. Climate change is
            a growing threat to a wide range of wildlife, including insects. The two best things you can do is eat
            less meat and make fewer vehicle journeys. Join an organisation such as your local Wildlife Trust or
            Buglife. Charities like these do vital work to protect and restore our most important wildlife sites,
            restore lost habitats at scale and reconnect our countryside. Most importantly, make every journey count,
                 by taking part in Bugs Matter!"))
      ),
      div(style = "padding: 20px; background-color: #C6E5E8; border-radius: 0px;",
          h3(style = "text-align: center; color: #0F0F0F;", "When did the survey start?"),
          p(style = "font-size: 18px; text-align: center; color: #0F0F0F;",
            HTML("The Bugs Matter citizen science survey took place throughout the UK between 1st June and 31st
            August in 2021, 2022 and 2023, and between 1st May and 30th September in 2024, using the Bugs Matter
            mobile application. In 2021 and 2022, users received a standardised sampling grid,
            termed a ‘splatometer’, in the post after they had signed up in the application.
            However, in 2023 the whole number plate was used to count insect splats."))
      ),
      div(style = "padding: 20px; background-color: #C6E5E8; border-radius: 0px;",
          h3(style = "text-align: center; color: #0F0F0F;", "The Bugs Matter mobile app"),
          p(style = "font-size: 18px; text-align: center; color: #0F0F0F;",
            HTML("The Bugs Matter app is available to download for free from the Apple App Store and Google Play.
            The app was built by Natural Apptitude and uses the Coreo data collection system. Within the app,
            users add details about the vehicle used for sampling, and are asked to confirm whether their number
            plate measures to standard UK dimensions, and if not, asked to manually input the
            dimensions of their number plate. Multiple vehicles can be added by a single user. Vehicle specification
            information is used in the analysis to determine if different types of vehicles sample insects differently."))
      ),
      div(style = "padding: 20px; background-color: #C6E5E8; border-radius: 0px;",
          h3(style = "text-align: center; color: #0F0F0F;", "How do you record a journey?"),
          p(style = "font-size: 18px; text-align: center; color: #0F0F0F;",
            HTML("Prior to commencing a journey, citizen scientists clean the front number plate of their vehicle to
            remove any residual insects. The app requests a checkbox confirmation that the number plate has been
            cleaned. Upon starting a journey, citizen scientists tap a button in the app to begin recording the
            journey route using the mobile device’s GPS. This provides crucial data on the length, duration,
            location, and average speed of the journey. Insects are then sampled when they collide with the number
            plate throughout the duration of a journey. Upon completing a journey, citizen scientists tap a
            button in the app to finish recording the journey route. They record the number of insect splats on
            the front number plate of their vehicle. The journey route, the number of insect splats, and a photograph
            of the number plate are submitted via the app. Citizen scientists are asked to participate only on
            essential journeys and not to make journeys specifically to take part in the survey."))
      ),
      div(style = "padding: 20px; background-color: #C6E5E8; border-radius: 0px;",
          h3(style = "text-align: center; color: #0F0F0F;", "How is the data analysed? Part I"),
          p(style = "font-size: 18px; text-align: center; color: #0F0F0F;",
            HTML("Prior to the analysis, some steps are taken to clean the data and remove outliers. Journeys with
            GPS errors are removed from the dataset. These errors are caused by a drop-out of background tracking
            due to GPS signal being lost by the device, and they appear as long straight lines between distant
                 locations. Very short journeys, very fast journeys, very slow journeys, or journeys with over 500
                 insect splats are removed from the dataset.  Finally, all journeys during which rainfall occurred
                 were omitted from the dataset due to the high chance that rainfall could dislodge insects from
                 number plates and create inaccurate splat counts."))
      ),
      div(style = "padding: 20px; background-color: #C6E5E8; border-radius: 0px;",
          h3(style = "text-align: center; color: #0F0F0F;", "How is the data analysed? Part II"),
          p(style = "font-size: 18px; text-align: center; color: #0F0F0F;",
            HTML("To begin exploring the data and calculate simple summary statistics, insect splat counts
            recorded by citizen scientists are converted to a ‘splat rate’ by dividing the insect splat count by
            the number plate sampling area and the journey distance, expressed in a unit of ‘splats per cm2 per mile’.
            This metric makes the data comparable between journeys and is defined as the number of insects sampled
            per cm2 of the number plate every mile. The response variable (insect count) shows a heavily right-skewed
            distribution due to the high number of zero and low values, as is typical for count-derived data.
            Therefore, a negative binomial generalized linear model (NB) was used to examine the relative effects
            of survey year, time of day of the journey, calendar date of the journey, average journey temperature,
            average journey speed, journey distance, vehicle type, elevation, local land cover, and road type, on
                 splat count."))
      ),

      div(style = "padding: 20px; background-color: #C6E5E8; border-radius: 0px;",
          h3(style = "text-align: center; color: #0F0F0F;", "How is the data analysed? Part III"),
          p(style = "font-size: 18px; text-align: center; color: #0F0F0F;",
            HTML("The analysis was performed using the MASS package (Venables and Ripley, 2002) in RStudio
            (R Core Team, 2022), following established techniques (Sokal and Rolf, 1995; Crawley, 2007). After
            running the model, variance inflation factor (VIF) scores were calculated to check for multicollinearity
            between independent variables. Comparisons of the number of insect splats between different timescales
            is achieved by rerunning the models with different reference years. The results of the ZINB model show
            the quantity of change in the response variable given a one-unit change in the independent variable,
            while holding other variables in the model constant. These values are called incidence rate ratios
            and they can be visualised effectively in a forest plot. Also presented, are plots of adjusted
            predictions of splat count across years, corrected for number plate sampling area and journey distance."))
      )
    )


    slickR(text_slides, slideId = 'slick1', height = 500, width = '50%') +
      settings(dots = TRUE, slidesToShow = 1, centerMode = F)
  })

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
        panel.grid.major = element_line(color = "#9FC8B8", linewidth  = 0.5),
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
        panel.grid.major = element_line(color = "#9FC8B8", linewidth = 0.5),
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
        panel.grid.major = element_line(color = "#9FC8B8", linewidth = 0.5),
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
      WHERE country = '%s' AND year >= '%s' AND year <= '%s' ORDER BY midpoint_time",
        input$country_trend, input$year_baseline, input$year_comparison
      )
    }
    else{
      sprintf(
      "SELECT region, country, splatcount, splat_rate, year, distance, avg_speed, vehicle_cl, vehicle_he, vehicle_wi, midpoint_time, dayofyear, \"X\", \"Y\", log_cm_miles_offset
      FROM bugs_matter.journeys5
      WHERE region = '%s' AND country = '%s' AND year >= '%s' AND year <= '%s' ORDER BY midpoint_time",
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
      ggplot2::ggplot(data = mod_data, ggplot2::aes(x = midpoint_time)) +
        ggplot2::geom_line(lwd=0.5, data = mod_data,  ggplot2::aes(y=dplyr::cummean(mod_data$splat_rate)), color="black") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(title = "Average Splat Rate Over Time", x = "Journey date", y = "Splat rate (splats/cm/mile)") +
        ggplot2::theme(
          plot.margin = ggplot2::margin(rep(10,4)),
          plot.background = ggplot2::element_rect(fill = "#C6E5E8", color = NA),  # Set background color
          panel.background = ggplot2::element_blank(),  # No panel background
          panel.border = ggplot2::element_blank(),  # Remove panel border
          text = ggplot2::element_text(family = "Rubik"),
          # Add space between title and plot
          plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 20)),  # Increase space between title and plot
          # Make axis tick labels bold
          axis.text.x = ggplot2::element_text(face = "bold"),
          axis.text.y = ggplot2::element_text(face = "bold"),
          # Remove grid lines and background for minor axis
          panel.grid.major = ggplot2::element_line(color = "#9FC8B8", linewidth = 0.5),
          panel.grid.minor = ggplot2::element_blank(),  # Remove minor gridlines
          axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 15)),  # Add space between x-axis title and plot
          axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 15))   # Add space between y-axis title and plot
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
          panel.grid.major = element_line(color = "#9FC8B8", linewidth = 0.5),
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
          panel.grid.major = element_line(color = "#9FC8B8", linewidth = 0.5),
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
          panel.grid.major = element_line(color = "#9FC8B8", linewidth = 0.5),
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
        panel.grid.major = element_line(color = "#9FC8B8", linewidth = 0.5),
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
