#' get_data
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @export

# Generic GraphQL request function
get_journeys <- function(conn, url, project_id, start_id) {
    journeysQuery <- "
        query BugsMatterNightlyJourneysQuery($projectId: Int!, $startId: Int!){
            records(where: {
                projectId: $projectId,
                id: { gte: $startId }
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
            query = query,
            variables = list(
                projectId = project_id,
                startId = start_id
            )
        ),
        encode = "json",
        httr::add_headers(Authorization = paste("JWT", token))
    )

    content_text <- httr::content(response, as = "text")

    new_journeys <-  jsonlite::fromJSON(content_text, flatten = TRUE) %>%
        as.data.frame() %>%
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

    message(sprintf("Added %s journeys.", new_journeys))
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
