#' get_data
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @export
conn <- pool::dbPool(
  drv = RPostgres::Postgres(),
  host = "kwt-postgresql-azdb-1.postgres.database.azure.com",
  port = 5432, dbname = "shared",
  user = Sys.getenv("USER"),
  password = Sys.getenv("PASSWORD"),
  sslmode = "prefer"
)

url <- "https://api.coreo.io/graphql"
token <- Sys.getenv("BUGS_MATTER_API_TOKEN")
project_id <- 343

# Generic GraphQL request function
graphql_request <- function(query, variables) {
  response <- httr::POST(
    url,
    body = list(query = query, variables = variables),
    encode = "json",
    httr::add_headers(Authorization = paste("JWT", token))
  )

  content_text <- httr::content(response, as = "text")
  jsonlite::fromJSON(content_text, flatten = TRUE)
}

get_data <- function(conn, project_id, startId, endId) {
    journeysQuery <- '
        query BugsMatterNightlyJourneysQuery($projectId: Int!, $startId: Int!, $endId: Int!){
            records(where: {
                projectId: $projectId,
                id: { gte: $startId, lte: $endId }
            }, limit: 10000){
                id
                geometry {
                    type
                    coordinates
                }
                data
                userId
            }
        }
    '
    data <- graphql_request(journeysQuery, list(
        projectId = projectId,
        startId = startId,
        endId = endId
    )) %>%
        as.data.frame() %>%
        tibble()
    print(data)
}

data <- get_data(conn, project_id, startId = 9000000, endId = 10000000)


journeys <- data %>%
    dplyr::rename_all(
        function(col) stringr::str_replace_all(col, "data.records.", "")
    ) %>%
    dplyr::rename_all (
        function(col) stringr::str_replace_all(col, "data.", "")
    ) %>%
    dplyr::rename_all (
        snakecase::to_snake_case
    ) %>%
    dplyr::filter(geometry_type == "LineString") %>%
    dplyr::mutate(
        duration = as.numeric(time) / 60 / 60, #seconds to hours
        avg_speed = distance / duration,
        splatcount = as.numeric(count),
        start = as.POSIXct(start, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
        end = as.POSIXct(end, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
        year = format(start, format="%Y")
    )

## data cleaning
    ## Remove points
    ## Remove empty/invalid geometry
    ## Remove lines with segments > 1000m (GPS errors)
    ## Remove journeys that pass within 50m of a car ferry route
    ## Remove short journeys (<= 1 mile or <= 0.050 hours/180 s)
    ## Remove fast journeys with speed >= 60 mph
    ## Remove slow journeys with speed <= 3 mph
    ## Remove splat counts >= 500
    ## save stats

## region classification





