#' get_region_ids
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
get_region_ids <- function(x) {
  if (x == "uk") {
    as.integer(c(11, 10, 12, 4, 6, 7, 1, 2, 8, 9, 5, 3))
  } else if (x == "england") {
    as.integer(c(4, 6, 1, 2, 8, 9, 5, 3, 7))
  } else {
    as.integer(x)
  }
}