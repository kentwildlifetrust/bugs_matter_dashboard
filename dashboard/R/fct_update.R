#' update
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @export
update_database <- function() {
    #set things up
    conn <- pool::dbPool(
        drv = RPostgres::Postgres(),
        host = "kwt-postgresql-azdb-1.postgres.database.azure.com",
        port = 5432, dbname = "bugs_matter",
        user = Sys.getenv("USER"),
        password = Sys.getenv("PASSWORD"),
        sslmode = "prefer"
    )
    url <- "https://api.coreo.io/graphql"
    token <- Sys.getenv("BUGS_MATTER_API_TOKEN")
    project_id <- 343

    repeat {
        max_sign_up_date <- DBI::dbGetQuery(
            conn = conn,
            statement = "SELECT COALESCE(MAX(date), '2019-12-31') AS max FROM op.users_queried_dates;"
        ) %>%
            dplyr::pull("max") %>%
            as.Date()
        if (max_sign_up_date == Sys.Date()) {
            break
        }
        new_users <- bugsMatterDashboard::get_users(conn, url, project_id, max_sign_up_date + 1)
        DBI::dbExecute(conn, "INSERT INTO op.users_queried_dates (date) VALUES ($1)", max_sign_up_date + 1)
        Sys.sleep(1)
    }



    repeat {
        max_journey_id <- DBI::dbGetQuery(
            conn = conn,
            statement = "SELECT COALESCE(MAX(id), 0) AS max FROM op.journeys;"
        ) %>%
            dplyr::pull("max")
        new_journeys <- bugsMatterDashboard::get_journeys(conn, url, project_id, max_journey_id + 1)
        if (nrow(new_journeys) == 0) {
            break
        }
        Sys.sleep(1)
    }

    #check new journeys
    readLines(system.file("SQL/clean_journeys.sql", package="bugsMatterDashboard")) %>%
        paste(collapse = "\n") %>%
        DBI::dbExecute(conn, statement = .)



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


}
