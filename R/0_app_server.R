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
      dbname = "bugs_matter",
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

  organisation_choices <- c(
    "Kent Wildlife Trust" = "@kentwildlife",
    "Openreach" = "@openreach",
    "Amazon" = "@amazon",
    "Buglife" = "@buglife"
  )

  observeEvent(input$log_in, {
    if (is.null(email_filter())) {
      showModal(modalDialog(
        title = "View data for your organisation",
        p("Journeys and participation tabs will show data for just your organisation."),
        selectInput("email_filter", "Organisation", choices = organisation_choices, selected = NULL),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_organisation", "Log in", class = "btn-primay")
        ),
        size = "m",
        easyClose = TRUE
      ))
    } else {
      showModal(modalDialog(
        title = sprintf("Log out of %s", names(organisation_choices[organisation_choices == email_filter()])),
        p("Journeys and Participation will no longer be restricted to your organisation."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_log_out", "Log out", class = "btn-primary")
        ),
        size = "m",
        easyClose = TRUE
      ))
    }
  })

  observeEvent(input$confirm_log_out, {
    email_filter(NULL)
    updateActionButton(
      inputId = "log_in",
      label = "Log in"
    )
    removeModal()
  })

  email_filter <- reactiveVal(NULL)

  observeEvent(input$confirm_organisation, {
    updateActionButton(
      inputId = "log_in",
      label = "Log out"
    )
    email_filter(input$email_filter)
    removeModal()
  })

  mod_overview_server("overview_1", conn, next_page)
  mod_participation_server("participation_1", conn, next_page, email_filter, organisation_choices)
  mod_explore_journeys_server("explore_journeys_1", conn, next_page, email_filter, organisation_choices)
  mod_trends_server("trends_1", conn, next_page)
}
