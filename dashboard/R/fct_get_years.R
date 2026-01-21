#' get_years
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
get_years <- function(x) {
  if (x == "2021 to 2024") {
    as.integer(2021:2024)
  } else {
    as.integer(x)
  }
}
