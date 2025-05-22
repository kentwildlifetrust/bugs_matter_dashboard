#' clean_journeys 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @export
clean_journeys <- function(conn) {
}
## region classification using largest join
## calculate midpoint time
## tidy vehicle_class
## work out cm_km_offset & log_cm_km_offset

## simplify geometry to 100m

## elevation
## elevation raster using elevatr::get_elev_raster(). Data source: https://registry.opendata.aws/terrain-tiles/
## extract mean elevation using exactextractr::exact_extract(z = 7)

## habitats
## raster from https://data-gis.unep-wcmc.org/server/rest/services/NatureMap/NatureMap_HabitatTypes/ImageServer
## use a habitat lookup to reclassify, then make columns for each habitat type

## temperature
## source raster from https://knmi-ecad-assets-prd.s3.amazonaws.com/ensembles/data/months/ens/tg_0.1deg_day_2025_grid_ensmean.nc
## get the mean temp for the day of each journey location