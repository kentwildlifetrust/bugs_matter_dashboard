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
      title = shiny::tagList(
        tags$img(src = "www/bm_bug_icon1.png", height = "36px"),
        "Bugs",
        shiny::span("Matter")
      ),
      bslib::nav_panel(
        title = "Welcome",
        mod_welcome_ui("welcome_1")
      ),
      bslib::nav_panel(
        title = "Explore Journeys",
        mod_journeys_map_ui("journeys_map_1")
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
