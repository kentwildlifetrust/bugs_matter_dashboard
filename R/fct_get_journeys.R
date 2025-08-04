#' get_jouneys
#'
#' @description Retrieve journey records between two dates.
#'
#' @return Number of records updated
#'
#' @export

# Generic GraphQL request function
get_journeys <- function(start_date = Sys.Date() - 1, end_date = Sys.Date()) {
    #### Setup
    conn <- pool::dbPool(
        drv = RPostgres::Postgres(),
        host = "kwt-postgresql-azdb-1.postgres.database.azure.com",
        port = 5432, dbname = "bugs_matter",
        user = Sys.getenv("USER"),
        password = Sys.getenv("PASSWORD"),
        sslmode = "prefer"
    )

    ### Retrieve recent journey records

    #Journey records are provided via the Coreo GraphQL API. We filter by the createdAt field to capture records from a 24 hour window.

    #### Query API

    # API parameters
    url <- "https://api.coreo.io/graphql"
    token <- Sys.getenv("BUGS_MATTER_API_TOKEN")
    project_id <- 343

    # Query to get journeys by date
    journeys_query <- '
            query BugsMatterNightlyJourneysQuery($projectId: Int!, $startDate: String!, $endDate: String!){
                records(where: { projectId: $projectId, createdAt: { gte: $startDate, lte: $endDate } }, limit: 40000){
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
    response <- httr2::request(url) %>%
        httr2::req_headers(
            Authorization = paste("JWT", token)
        ) %>%
        httr2::req_body_json(
            list(
                query = journeys_query,
                variables = list(
                    projectId = project_id,
                    startDate = start_date,
                    endDate = end_date
                )
            )
        ) %>%
        httr2::req_method('POST') %>%
        httr2::req_perform()

    # Extract content
    content_text <- httr2::resp_body_string(response)

    # Check for error
    if (grepl("<!DOCTYPE html>", content_text)) {
        stop("Received HTML response instead of JSON. Check API endpoint and authentication.")
    }

    # Convert to data frame
    response_data <- content_text %>%
        jsonlite::fromJSON(flatten = TRUE)

    # Check for error response
    if ("errors" %in% names(response_data)) {
        stop(sprintf("Following error recieved: %s", response_data$errors$message))
    }

    # Tidy names
    journeys <- response_data[["data"]][["records"]] %>%
        as.data.frame() %>%
        tibble::as_tibble() %>%
        dplyr::rename_all(
            function(col) stringr::str_replace_all(col, "data.records.", "")
        ) %>%
        dplyr::rename_all(
            function(col) stringr::str_replace_all(col, "data.", "")
        ) %>%
        dplyr::rename_all(
            snakecase::to_snake_case
        )

    if ("geometry_type" %in% colnames(journeys)) {
        journeys <- journeys %>%
            dplyr::filter(geometry_type != "Point")
    }

    # Convert to sf
    if (nrow(journeys) == 0) {
        journeys$geom <- NA
        message("No records returned")
        return(0)
    } else {
        # Convert coordinates to sf geometries
        journeys <- journeys %>%
            dplyr::mutate(
                geom = purrr::map(geometry_coordinates, function(coords) {
                    if (!is.array(coords)) {
                        return(sf::st_point(coords))
                    }
                    if (nrow(coords) == 1) {
                        # Single point
                        sf::st_point(coords[1, ])
                    } else {
                        # Linestring
                        sf::st_linestring(coords)
                    }
                }),
                geom = sf::st_sfc(geom, crs = 4326)
            ) %>%
            sf::st_sf()
    }

    #column drops and changes
    journeys <- journeys %>%
        dplyr::select(
            -dplyr::any_of(c(
                "geometry_coordinates",
                "speed",
                "bearing",
                "accuracy",
                "timestamp"
            ))
        ) %>%
        #start & end not good for sql column names,
        #splat_count preferred over count
        #duration preffed over time
        dplyr::rename_with(
            function(name) dplyr::case_when(
                name == "end" ~ "end_timestamp",
                name == "start" ~ "start_timestamp",
                name == "count" ~ "splat_count",
                name == "time" ~ "duration",
                .default = name
            )
        )

    #### Data schema checks

    #As its based on GraphQL, the Coreo API does not have a fixed data schema. The table `op.journeys` accepts raw data from the API, covering all the expected fields. The following tests are applied to response fields:

    #*Field in response and in table* - save to database
    #*Field in response but not in table* - Unexpected fields, throw error. Database needs updating.
    #*Field in table but missing from response* - Missign fields, leave column values null. Step 2 will test if journeys have minimum required fields for data analysis.


    response_fields <- colnames(journeys)
    expected_fields <- DBI::dbGetQuery(
        conn,
        statement = "SELECT column_name FROM information_schema.columns WHERE table_name = 'raw' ORDER BY ordinal_position;"
    ) %>%
        dplyr::pull()

    unexpected_fields <- setdiff(response_fields, expected_fields)
    if (length(unexpected_fields) != 0) {
        stop(sprintf("Unexpected fields returned: %s", paste(unexpected_fields, collapse = ", ")))
    }
    missing_fields <- setdiff(expected_fields, response_fields)
    if (length(missing_fields) > 0) {
        message(sprintf("The following fields are missing from the response: %s", paste0(missing_fields, collapse = ", ")))
    }

    DBI::dbExecute(conn, "DELETE FROM journeys.temp;")
    sf::st_write(journeys, conn, DBI::Id("journeys", "temp"), append = TRUE)
    # Run dedup insert (only new rows will be inserted)
    DBI::dbExecute(conn, "
        INSERT INTO journeys.raw
        SELECT * FROM journeys.temp
        ON CONFLICT (id) DO NOTHING;
    ")
}
