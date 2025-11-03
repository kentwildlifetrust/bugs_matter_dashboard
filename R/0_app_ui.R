#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      id = "page_navbar",
      title = shiny::tagList(
        tags$img(src = "www/bm_bug_icon1.png", height = "30px"),
        "Bugs",
        shiny::span("Matter")
      ),
      bslib::nav_panel(
        title = "Overview",
        value = 1,
        mod_overview_ui("overview_1")
      ),
      bslib::nav_panel(
        title = "Journeys",
        value = 2,
        mod_explore_journeys_ui("explore_journeys_1")
      ),
      bslib::nav_panel(
        title = "Trends",
        value = 3,
        mod_trends_ui("trends_1")
      ),
      bslib::nav_panel(
        title = "Participation",
        value = 4,
        mod_participation_ui("participation_1")
      ),
      bslib::nav_item(
        actionButton("log_in", "Log in")
      ),
      theme = theme()
    )

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  shiny::tags$head(
    golem::favicon(),
    shiny::HTML('<script crossorigin="anonymous" src="https://kit.fontawesome.com/82bb623444.js"></script>'),
    shiny::HTML('<script src="https://unpkg.com/leaflet.vectorgrid@latest/dist/Leaflet.VectorGrid.bundled.js"></script>'),
    shinyjs::useShinyjs(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "Bugs Matter Dashboard"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
