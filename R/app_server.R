#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  tryCatch({
    conn <- DBI::dbConnect(
      drv = RPostgres::Postgres(),
      host = "kwt-postgresql-azdb-1.postgres.database.azure.com",
      port = 5432,
      dbname = "shared",
      user = Sys.getenv("USER"),
      password = Sys.getenv("PASSWORD"),
      sslmode = "prefer"
    )
  }, error = function(e) {
    handle_error(e, "app_server.R", alert_title = "Database error")
  })

  mod_journeys_map_server("journeys_map_1", conn)
  mod_welcome_server("welcome_1", conn)


}
##