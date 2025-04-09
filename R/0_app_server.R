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
      user = Sys.getenv("BUGS_MATTER_USER"),
      password = Sys.getenv("BUGS_MATTER_PASSWORD"),
      sslmode = "prefer"
    )
  }, error = function(e) {
    handle_error(e, "app_server.R", alert_title = "Database error")
  })

  next_page <- shiny::reactiveVal(0)

  shiny::observeEvent(next_page(), {
    shiny::req(next_page() > 0)
    bslib::nav_select("page_navbar", as.character(as.numeric(input$page_navbar) + 1))
  })

  mod_welcome_server("welcome_1", conn, next_page)
  mod_explore_journeys_server("explore_journeys_1", conn, next_page)
  mod_analyse_server("analyse_1", conn, next_page)
  mod_get_involved_server("get_involved_1", conn)
}
##