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
        new_users <- bugsMatter::get_users(conn, url, project_id, max_sign_up_date + 1)
        DBI::dbExecute(conn, "INSERT INTO op.users_queried_dates (date) VALUES ($1)", max_sign_up_date + 1)
        Sys.sleep(1)
    }


    
    repeat {
        max_journey_date <- DBI::dbGetQuery(
            conn = conn,
            statement = "SELECT COALESCE(MAX(date), '2019-12-31') AS max FROM op.journeys_queried_dates;"
        ) %>%
            dplyr::pull("max") %>%
            as.Date()
        if (max_journey_date == Sys.Date()) {
            break
        }
        new_users <- bugsMatter::get_journeys(conn, url, project_id, max_journey_date + 1)
        DBI::dbExecute(conn, "INSERT INTO op.journeys_queried_dates (date) VALUES ($1)", max_journey_date + 1)
        Sys.sleep(1)
    }

}
