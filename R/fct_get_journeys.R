#' get_data
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @export

# Generic GraphQL request function
get_journeys <- function(conn, url, project_id, date) {
    message(sprintf("Fetching journeys for %s", date))
    # Construct end date by adding one day
    end_date <- as.Date(date) + 1
    
    journeysQuery <- "
        query BugsMatterNightlyJourneysQuery($projectId: Int!, $date: String!, $endDate: String!){
            records(where: {
                projectId: $projectId,
                data: { 
                    time: { 
                        gte: $date,
                        lt: $endDate
                    }
                }
            }, limit: 1000){
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
                date = date,
                endDate = end_date
            )
        ),
        encode = "json",
        httr::add_headers(Authorization = paste("JWT", token))
    )

    # Check for HTTP errors
    if (httr::http_error(response)) {
        stop(sprintf("HTTP error: %s", httr::http_status(response)$message))
    }

    content_text <- httr::content(response, as = "text")
    
    # Check if we got HTML instead of JSON
    if (grepl("<!DOCTYPE html>", content_text)) {
        stop("Received HTML response instead of JSON. Check API endpoint and authentication.")
    }

    new_journeys <-  jsonlite::fromJSON(content_text, flatten = TRUE) %>%
        as.data.frame()

    print(new_journeys)

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
        ) %>%
        dplyr::filter(geometry_type == "LineString") %>%
        dplyr::mutate(
            duration = as.numeric(time) / 60 / 60, # seconds to hours
            avg_speed = distance / duration,
            splat_count = as.numeric(count),
            start_timestamp = as.POSIXct(start, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
            end_timestamp = as.POSIXct(end, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
            year = format(start_timestamp, format = "%Y")
        )

    # Convert coordinates to sf geometries
    new_journeys <- new_journeys %>%
        dplyr::mutate(
            geom = purrr::map(geometry_coordinates, function(coords) {
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
        sf::st_sf() %>%
        dplyr::rename(app_timestamp = timestamp, photo_url = photo) %>%
        dplyr::select(
            -geometry_type,
            -geometry_coordinates,
            -start,
            -end,
            -time,
            -vehicle_year_month_registered,
            -count
        )


    sf::st_write(new_journeys, conn, DBI::Id("op", "all_journeys"), append = TRUE)

    message(sprintf("Added %s journeys.", nrow(new_journeys)))
    return(new_journeys)
}




## region classification using largest join
## calculate midpoint time
## tidy vehicle_class
## work out cm_km_offset & log_cm_km_offset

## simplify geometry to 100m

## elevation
## elevation raster using elevatr::get_elev_raster(). Data source: https://registry.opendata.aws/terrain-tiles/
## extract mean elevation using exactextractr::exact_extract(z = 7)

## habitats
## raster from https://data-gis.unep-wcmc.org/server/rest/services/NatureMap/NatureMap_HabitatTypes/ImageServer
## use a habitat lookup to reclassify, then make columns for each habitat type

## temperature
## source raster from https://knmi-ecad-assets-prd.s3.amazonaws.com/ensembles/data/months/ens/tg_0.1deg_day_2025_grid_ensmean.nc
## get the mean temp for the day of each journey location
