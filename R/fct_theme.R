#' theme
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
theme <- function() {
  bslib::bs_theme(
    bg = "#F1F6F4",
    fg = "#0F0F0F",
    primary = "#58A732",
    secondary = "#02606F",
    success = "#00A36A",
    warning = "#FFAB00",
    danger = "#FF5630",
    info = "#00272D",
    base_font = "Rubik",
    font_scale = 0.9,
    "btn-padding-x" = ".5rem",
    "btn-padding-y" = ".25rem",
    "border-radius" = "4px",
    "bslib-spacer" = "12px",
    "card-bg" = "#FFFFFF",
    "table-bg" = "#FFFFFF",
    "dropdown-padding-x" = ".5rem",
    "dropdown-bg" = "#FFFFFF",
    "navbar-bg" = "#C6E5E8",
    "card-border-width" = "2px",
    "card-border-color" = "#58A732",
    "navbar-light-active-color" = "#142D18",
    "navbar-dark-active-color" = "#142D18",
    "nav-link-active-font-weight" = 600,
    "font-size-lg" = "1.2rem"
  )
}
