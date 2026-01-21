#' theme
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
theme <- function() {
  bslib::bs_theme(
    bg = "#F7F9FE",
    fg = "#0F0F0F",
    primary = "#147331",
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
    "navbar-bg" = "#c4e8ff",
    "card-border-width" = "2px",
    "card-border-color" = "#147331",
    "nav-link-active-font-weight" = 600,
    "font-size-lg" = "1.2rem"
  )
}
