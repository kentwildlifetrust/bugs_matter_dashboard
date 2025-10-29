#' model
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
model <- function(journeys) {
  journeys$vehicle_class <- as.factor(journeys$vehicle_class)
  journeys$year <- as.integer(journeys$year)
  journeys$time_of_day <- format(journeys$midpoint_time, "%H:%M:%S")
  journeys$time_of_day <- as.numeric(lubridate::hms(journeys$time_of_day)) / 3600
  journeys$temperature[is.na(journeys$temperature)] <- mean(journeys$temperature, na.rm = TRUE)

  journeys <- journeys %>%
    dplyr::rename(
      "Year" = year,
      "Distance" = distance,
      "Average.speed" = avg_speed_kmh,
      "Vehicle.type" = vehicle_class,
      "Vehicle.height" = vehicle_height,
      "Time.of.day" = time_of_day,
      "Day.of.year" = day_of_year,
      "Elevation" = elevation,
      "Temperature" = temperature,
      "Longitude" = center_lat,
      "Latitude" = center_lon,
      "Proportion.forest" = forest,
      "Proportion.shrubland" = shrubland,
      "Proportion.grassland" = grassland,
      "Proportion.wetland" = wetland,
      "Proportion.marine" = marine,
      "Proportion.arable" = arable,
      "Proportion.urban" = urban,
      "Proportion.pasture" = pasture
    )

  # Run the model
  mod <- glmmTMB::glmmTMB(
    splat_count ~ Year +
    Distance +
    Average.speed +
    Vehicle.type +
    Time.of.day +
    Day.of.year +
    Elevation +
    Temperature +
    Longitude +
    Latitude +
    Proportion.forest +
    Proportion.shrubland +
    Proportion.grassland +
    Proportion.wetland +
    Proportion.marine +
    Proportion.arable +
    # Proportion.pasture +
    Proportion.urban +
    offset(log_cm_km_offset),
    family = glmmTMB::nbinom2,
    data = journeys,
    ziformula = ~0        # no zero inflation
  )
  mod
}
