#' get_data
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @export

# Generic GraphQL request function
get_journeys <- function(conn, url, project_id, start_id) {
    message(sprintf("Fetching journeys after ID %s", start_id))

    journeysQuery <- "
        query BugsMatterNightlyJourneysQuery($projectId: Int!, $startId: Int!){
            records(where: { projectId: $projectId, id: { gte: $startId } }, limit: 10000){
                id
                geometry {
                    type
                    coordinates
                }
                data
                userId
            }
        }
    "

    response <- httr::POST(
        url,
        body = list(
            query = journeysQuery,
            variables = list(
                projectId = project_id,
                startId = start_id
            )
        ),
        encode = "json",
        httr::add_headers(Authorization = paste("JWT", token))
    )

    content_text <- httr::content(response, as = "text")

    # Check if we got HTML instead of JSON
    if (grepl("<!DOCTYPE html>", content_text)) {
        stop("Received HTML response instead of JSON. Check API endpoint and authentication.")
    }

    new_journeys <-  jsonlite::fromJSON(content_text, flatten = TRUE) %>%
        as.data.frame()

    if (nrow(new_journeys) == 0) {
        return(new_journeys)
    }

    new_journeys <- new_journeys %>%
        tibble::tibble() %>%
        dplyr::rename_all(
            function(col) stringr::str_replace_all(col, "data.records.", "")
        ) %>%
        dplyr::rename_all(
            function(col) stringr::str_replace_all(col, "data.", "")
        ) %>%
        dplyr::rename_all(
            snakecase::to_snake_case
        )

    if (!"time" %in% colnames(new_journeys)) {
        new_journeys$time <- NA
    }
    if (!"distance" %in% colnames(new_journeys)) {
        new_journeys$distance <- NA
    }
    if (!"count" %in% colnames(new_journeys)) {
        new_journeys$count <- NA
    }
    if (!"start" %in% colnames(new_journeys)) {
        new_journeys$start <- NA
    }
    if (!"end" %in% colnames(new_journeys)) {
        new_journeys$end <- NA
    }

    new_journeys <- new_journeys %>%
        dplyr::mutate(
            duration = as.numeric(time) / 60 / 60, # seconds to hours
            avg_speed = distance / duration,
            splat_count = as.numeric(count),
            start_timestamp = as.POSIXct(start, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
            end_timestamp = as.POSIXct(end, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
            year = format(start_timestamp, format = "%Y")
        ) %>%
        dplyr::filter(!is.na(splat_count))

    if (nrow(new_journeys) == 0) {
        new_journeys$geom <- NA
    } else {
        # Convert coordinates to sf geometries
        new_journeys <- new_journeys %>%
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
    new_journeys <- new_journeys %>%
        dplyr::rename(app_timestamp = timestamp) %>%
        dplyr::select(
            -dplyr::any_of(c(
                "geometry_type",
                "geometry_coordinates",
                "start",
                "end",
                "time",
                "vehicle_year_month_registered",
                "count"
            ))
        )

    if ("photo" %in% colnames(new_journeys)) {
        new_journeys <- new_journeys %>%
            dplyr::rename(photo_url = photo)
    }
    sf::st_write(new_journeys, conn, DBI::Id("op", "all_journeys"), append = TRUE)

    message(sprintf("Added %s journeys.", nrow(new_journeys)))
    return(new_journeys)
}
