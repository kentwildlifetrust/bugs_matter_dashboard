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
        max_user_id <- DBI::dbGetQuery(
            conn = conn,
            statement = "SELECT COALESCE(MAX(id), 0) AS max FROM op.users;"
        ) %>%
            dplyr::pull("max")

        new_users <- bugsMatter::get_users(conn, url, project_id, max_user_id + 1)
        if (nrow(new_users) == 0) {
            break
        }
   }


    # #find the current maximum journey_id
    # max_journey_id <- DBI::dbGetQuery(
    #     conn = conn,
    #     statement = "SELECT COALESCE(MAX(id), 0) AS max FROM op.all_journeys;"
    # ) %>%
    #     dplyr::pull("max")

    # new_journeys <- bugsMatter::get_journeys(conn, url, project_id, max_journey_id + 1)
}
