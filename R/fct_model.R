#' model
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
model <- function(journeys) {
  journeys$vehicle_class <- as.factor(journeys$vehicle_class)
  journeys$year <- as.factor(journeys$year)
  journeys$time_of_day <- as.numeric(lubridate::hms(paste0(journeys$time_of_day, ":00")))

  journeys <- journeys %>%
    dplyr::rename(
      "Year" = year,
      "Distance" = distance,
      "Average.speed" = avg_speed_kmh,
      "Vehicle.type" = vehicle_class,
      "Time.of.day" = time_of_day,
      "Day.of.year" = day_of_year,
      "Elevation" = elevation,
      "Temperature" = temperature,
      "Proportion.forest" = forest,
      "Proportion.grassland" = grassland,
      "Proportion.wetland" = wetland,
      "Proportion.arable" = arable,
      "Proportion.urban" = urban
    )

  # Run the model
  NB <- MASS::glm.nb(
    splat_count ~ Year +
    Distance +
    Average.speed +
    Vehicle.type +
    Time.of.day +
    Day.of.year +
    Elevation +
    Temperature +
    Proportion.forest +
    Proportion.grassland +
    Proportion.wetland +
    Proportion.arable +
    Proportion.urban +
    offset(log_cm_km_offset),
    data = journeys
  )
  return(NB)
}
