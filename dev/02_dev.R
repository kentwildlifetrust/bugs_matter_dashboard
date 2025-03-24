# Building a Prod-Ready, Robust Shiny Application.
#
# README: these dev scripts help ensure KWT Webapps are consistent, reliable and production-ready
# By following each step, you'll be helping your future self and any other maintainers!
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# use source("dev/03_deploy.R") each time you want to merge into main
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Version control ---------------------------------------------------------
## You will probably want to be using a Git GUI, such as:
## - GitKraken
## - GitHub Desktop
## - Visual Studio Code Source Control
## - Visual Studio Code GitLens extension

## Before you start developing, commit your progress so far to the main branch
## Next, create a new branch for developing and name it helpfully, e.g. dev, or dev-<your-initials>
## Commit little and commit often.
## By working on a dev branch you won't be slowing down our app server every time you commit
## Merge into main when you want to deploy to ShinyProxy.

## Dependencies ------------------------------------------------------------

## CRAN dependencies --
## most of your dependencies will come from CRAN
## CRAN has at least some checks to make sure that packages are safe to use
## You can install CRAN dependencies & add to the DESCRIPTION like this:
usethis::use_package("bslib")
usethis::use_pipe()

## Github dependencies --
## sometimes we need to use a package which is not on CRAN
## for example, one of KWT's packages like shinyHelper
## or another repository, where we trust the creator not to be spreading malware!!
## To add a Github dependency, use a different function to install and add to DESCRIPTION
remotes::install_github("kentwildlifetrust/shiny_helper")
usethis::use_dev_package("shinyHelper")

## run renv snapshot to record the exact package version you used and the source
## this allows the app to run reliably on another machine :)
renv::snapshot()

## Add modules --------------------------------------------------------------

## Create a module infrastructure in R/
## Only required if the app ui and server code would be very large or repetitive
## Break down into logical components, like one module per page on a dashboard
## If there is any sort of sequence to the modules, it can be really nice to add numbered prefixes to the file names
golem::add_module(name = "journeys_map") # Name of the module
golem::add_module(name = "name_of_module2") # Name of the module

## External resources ---------------------------------------------------------
## Creates .css file in inst/app/www
golem::add_css_file("styles")

golem::add_fct("theme")


## Data -----------------------------------------------------------------------
## There are several options to get tabular data into your shiny app...

## 1 - Package data ---
## The easiest way to access data is using a method built into R Packages.
## Use this method if all the following are true:
## - Data rarely needs updating (as this will require a commit & push to main)
## - Data won't benefit from breaking down into multiple tables (as PostGIS ERD editors are best for this)
## - Data won't need to be accessed outside of R, e.g. QGIS or Excel
## - Data is smaller than 10 Mb

## Instructions
## Create a data.R file in the dev folder
## Write a script to load the data into one or more R objects
## You might want to tidy it up as well
## Add comments for helpful information, especially data sources and data purpose
## Add to the package like this:

conn <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  host = "kwt-postgresql-azdb-1.postgres.database.azure.com",
  port = 5432,
  dbname = "shared",
  user = Sys.getenv("USER"),
  password = Sys.getenv("PASSWORD"),
  sslmode = "prefer"
)

years <- "SELECT DISTINCT EXTRACT(YEAR FROM j.end) AS year
          FROM bugs_matter.journeys_app j;" %>%
  DBI::dbGetQuery(conn, .) %>%
  dplyr::pull("year") %>%
  sort()
years <- years
usethis::use_data(years, overwrite = TRUE)


# interval <- ceiling(length(dates) / 15)

# dates <- seq.Date(min(dates) - interval, max(dates) + interval, by = sprintf("%s days", interval+2))

# if (length(dates) > 15) stop("too many")

journeys <- list()

for (year in years) {
  dates <- "SELECT DISTINCT j.end::DATE AS date
            FROM bugs_matter.journeys_app j
            WHERE EXTRACT(YEAR FROM j.end) = {year}
            ORDER BY j.end::DATE;" %>%
    glue::glue_data_sql(
      list(year = year),
      .,
      .con = conn
    ) %>%
    DBI::dbGetQuery(conn, .) %>%
    dplyr::pull("date") %>%
    as.Date()

  years_journeys <- list()

  for (i in seq_len(length(dates) - 1)) {
    start <- as.character(dates[i])
    end <- as.character(dates[i + 1])

    lines <- "SELECT
                j.id,
                j.geom
    FROM bugs_matter.journeys_app j
    WHERE j.end::DATE >= {start} AND
    j.end::DATE < {end};" %>%
      glue::glue_data_sql(
        list(start = start, end = end),
        .,
        .con = conn
      ) %>%
      DBI::dbGetQuery(conn, .) %>%
      dplyr::mutate(geom = sf::st_as_sfc(.$geom)) %>%
      sf::st_as_sf(crs = 4326)

    if (nrow(lines) > 0) {
      years_journeys <- c(
        years_journeys,
        list(list(
          group = start,
          lines = lines
        ))
      )
    }
  }

  journeys <- c(
    journeys,
    list(list(
      year = year,
      data = years_journeys,
      dates = dates
    ))
  )
}


usethis::use_data(journeys, overwrite = TRUE)

cumulative_count <- "WITH daily_counts AS (
  SELECT
    j.end::DATE AS date,
    COUNT(*) AS daily_count
  FROM bugs_matter.journeys_app j
  GROUP BY j.end::DATE
), date_bounds AS (
  SELECT
    '2021-05-01'::DATE AS min_date,
    '2024-12-31'::DATE AS max_date
),  -- No need to select FROM the table here
all_dates AS (
  SELECT generate_series(min_date, max_date, interval '1 day')::date AS date
  FROM date_bounds
)
SELECT
  all_dates.date,
  COALESCE(daily_counts.daily_count, 0) AS daily_count,
  SUM(COALESCE(daily_counts.daily_count, 0)) OVER (
    PARTITION BY EXTRACT(YEAR FROM all_dates.date)
    ORDER BY all_dates.date
    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
  ) AS cumulative_count
FROM all_dates
LEFT JOIN daily_counts ON all_dates.date = daily_counts.date
ORDER BY all_dates.date;
          " %>%
          DBI::dbGetQuery(conn, .) %>%
          dplyr::mutate(date = as.Date(date))

usethis::use_data(cumulative_count, overwrite = TRUE)



## Your data can be accessed anywhere in the package code using
packageName::dataName
## This will be very quick to load

## 2 - PostGIS data ---
## If package data doesn't make sense for any reason you'll probably want to go with PostGIS

## Instructions
## First ask either Kia or Euan to create a PostGIS user for your app.
## You may want to discuss if it makes sense to create a new schema or database to hold your data,
## if it doesn't already exist and doesn't make sense in an existing shared database schema.

## The database tables can be setup in QGIS, PGAdmin or with your own SQL script in the dev folder
## SQL scripts can be run from RStudio or Visual Studio Code - speak to Euan if you need help with this
## Be sure to grant the minimum privileges needed on those tables to your app user
## Save the username and password in your Renviron file, following this naming convention:
## PACKAGE_NAME_USER=xxxxx
## PACKAGE_NAME_PASSWORD=xxxxx
## Go to the top of server.R and edit the pool connection, BLANK_WEBAPP_USER and BLANK_WEBAPP_PASSWORD with your variable names.
## Pass the pool connection into any module servers where it is needed.

## 3 - API ---
## The other option is an API. The recommended package to send HTTP requests is {httr2}
## API secrets & redirect URIs should be saved in your Renviron file with the following naming convention:
## PACKAGE_NAME_SECRET_NAME=xxxxxxx
## Ideally you want to put your code for interacting with the API in functions and write tests
## Speak to Euan if you are unsure about this!

## ESSENTIAL: send any secrets or other environment variables to Kia or Euan to be added to ShinyProxy

## Error Handling ---------------------------------------------------------------
## The app template comes with a function handle_error
## Use this with a tryCatch to gracefully handle errors
## it is recommended to apply these around database and API queries
devtools::load_all()
?handle_error()

# example
tryCatch(
  {
    a <- 1 + 2
    b <- 3 + 5
    stop("Something went wrong here!")
    c <- a + b
  },
  error = function(e) {
    handle_error(e, script_name = "app_ui.R")
  }
)


# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
