#' theme
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
theme <- function() {
  bslib::bs_theme(
    bg = "#C6E5E8",
    fg = "#0F0F0F",
    primary = "#2374FF",
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
    "dropdown-bg" = "#FFFFFF"
  ) |>
    bslib::bs_add_rules(
        "
            .navbar-brand {
            font-family: 'Adelle', sans-serif !important;
            font-size: 1.4rem !important;
            font-weight: bold !important;
            color: #0F0F0F !important;
            }
        "
    )
}
