library(magrittr)

conn <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  host = "kwt-postgresql-azdb-1.postgres.database.azure.com",
  port = 5432,
  dbname = "bugs_matter",
  user = Sys.getenv("USER"),
  password = Sys.getenv("PASSWORD"),
  sslmode = "prefer"
)


#countries from whole world download (separate levels geopackage)
#https://gadm.org/download_world.html

path <- "dev/.data/gadm_410-levels.gpkg"
sf::st_layers(path)
sf::st_read(path, layer = "ADM_0")  %>%
    dplyr::select(
        code = GID_0,
        name = COUNTRY,
        geom
    ) %>%
    sf::st_transform(crs = 27700) %>%
    sf::st_simplify(dTolerance = 100) %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_write(dsn = conn, DBI::Id("ref", "countries"), append = TRUE)

#regions from shared database

shared_conn <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  host = "kwt-postgresql-azdb-1.postgres.database.azure.com",
  port = 5432,
  dbname = "shared",
  user = Sys.getenv("USER"),
  password = Sys.getenv("PASSWORD"),
  sslmode = "prefer"
)

regions <- sf::st_read(shared_conn, DBI::Id("bugs_matter", "regionboundaries1"))
regions <- regions %>%
  dplyr::mutate(
    id = kwtid,
    name = region,
    country_code = dplyr::case_when(
      country == "Wales" ~ "GBR",
      country == "Scotland" ~ "GBR",
      country == "Northern Ireland" ~ "GBR",
      country == "Ireland" ~ "IRL",
      country == "England" ~ "GBR",
      country == "France" ~ "FRA",
      country == "Spain" ~ "ESP",
      country == "Portugal" ~ "PRT"
    )
  ) %>%
  dplyr::select(
    id,
    name,
    country_code,
    geom = geometry
  )
sf::st_write(regions, conn, DBI::Id("ref", "regions"), append = TRUE)

DBI::dbExecute(conn, "DELETE FROM ref.regions WHERE country_code = 'GBR' AND NAME NOT IN ('Wales', 'Scotland', 'Northern Ireland');")

max_id <- DBI::dbGetQuery(conn, "SELECT MAX(id) FROM ref.regions;") %>% dplyr::pull()
path <- "dev/.data/gadm41_GBR_1.json"
json <- readr::read_file(path) #needed to cope with file encoding (region names with unusual characters)
regions <- sf::st_read(json, quiet = TRUE) %>%
  dplyr::mutate(country_code = "GBR", name = "England", id = max_id + 1)
regions %>%
  dplyr::filter(GID_1 == "GBR.1_1") %>%
  dplyr::select(id, country_code, name, geom = geometry) %>%
  sf::st_transform(crs = 27700) %>%
  sf::st_simplify(dTolerance = 100) %>%
  sf::st_transform(crs = 4326) %>%
  sf::st_write(dsn = conn, DBI::Id("ref", "regions"), append = TRUE)



#get English regions
#https://geoportal.statistics.gov.uk/datasets/f181d27b5bac477388ce81ad935a1227_0/explore?location=52.723884%2C-2.489483%2C6.92
path <- "dev/.data/Regions_December_2024_Boundaries_EN_BFC_1195854647342073399.geojson"
region_id <- DBI::dbGetQuery(conn, "SELECT id FROM ref.regions WHERE name = 'England';") %>% dplyr::pull()
regions <- sf::st_read(path, quiet = TRUE) %>%
    dplyr::mutate(id = 1:nrow(.), region_id = region_id, name = RGN24NM) %>%
    dplyr::select(id, region_id, name, geom = geometry) %>%
        sf::st_transform(crs = 27700) %>%
        sf::st_simplify(dTolerance = 100) %>%
        sf::st_transform(crs = 4326)
DBI::dbExecute(conn, "DELETE FROM ref.sub_regions")
sf::st_write(regions, dsn = conn, DBI::Id("ref", "sub_regions"), append = TRUE)


###-------------------------------ferry routes----------------------------------###
#use overpass api for bulk downloads
url <- "https://overpass-api.de/api/interpreter"

#grab ferry routes for the area
#chunk the area so as to not overwhelm the API
# Create a grid of bounding boxes (2x2 degrees each)
create_bbox_grid <- function(xmin, xmax, ymin, ymax, size = 5) {
  x_seq <- seq(xmin, xmax, by = size)
  y_seq <- seq(ymin, ymax, by = size)

  bboxes <- list()
  for (i in 1:(length(x_seq)-1)) {
    for (j in 1:(length(y_seq)-1)) {
      bboxes[[length(bboxes) + 1]] <- c(
        x_seq[i], y_seq[j],
        x_seq[i+1], y_seq[j+1]
      )
    }
  }
  return(bboxes)
}

# Create grid of bounding boxes
bboxes <- create_bbox_grid(-30, 30, 25, 90)

# Process each bbox and combine results
ferry_routes_list <- list()
for (i in seq_along(bboxes)) {
  message(sprintf("Processing bbox %d of %d", i, length(bboxes)))
  tryCatch({
    routes <- osmdata::opq(bbox = bboxes[[i]]) %>%
      osmdata::add_osm_feature(key = 'route', value = 'ferry') %>%
      osmdata::osmdata_sf()

    if (!is.null(routes$osm_lines)) {
      ferry_routes_list[[i]] <- routes$osm_lines
    }
  }, error = function(e) {
    message(sprintf("Error processing bbox %d: %s", i, e$message))
  })

  # Add a small delay to avoid overwhelming the API
  Sys.sleep(1)
}

# Combine all results
# 12573
ferry_routes <- do.call(dplyr::bind_rows, ferry_routes_list[!sapply(ferry_routes_list, is.null)])

# Remove any duplicates that might occur at bbox boundaries
ferry_routes <- ferry_routes[!duplicated(ferry_routes$osm_id), ]
# 10756

ferry_routes <- ferry_routes %>%
  dplyr::select("osm_id", "name", "motor_vehicle", geom = "geometry")
DBI::dbExecute(conn, "DELETE FROM ref.ferry_routes;")
sf::st_write(ferry_routes, conn, DBI::Id("ref", "ferry_routes"))
DBI::dbExecute(conn, "CREATE MATERIALIZED VIEW ref.vehicle_ferry_routes AS (
    SELECT osm_id, st_simplify(st_transform(geom, 27700), 100) AS geom
    FROM ref.ferry_routes
    WHERE motor_vehicle = 'yes'
) WITH DATA;")
###-------------------------------elevation----------------------------------###
elevation <- elevatr::get_elev_raster(
  locations = data.frame(
    x = c(-30, 30),
    y = c(25, 90)
  ),
  z = 7,
  prj = 4326
)
raster::writeRaster(elevation, "dev/.data/elevation.tif")
# raster2pgsql -s 4326 -C -I -F -t 2000x2000 dev/.data/elevation.tif ref.elevation | psql -h kwt-postgresql-azdb-1.postgres.database.azure.com -p 5432 -U euanmckenzie -d bugs_matter


###-------------------------------habitat-----------------------------------###
url <- "https://data-gis.unep-wcmc.org/server/rest/services/NatureMap/NatureMap_HabitatTypes/ImageServer"
imgsrv <- arcgislayers::arc_open(url)
imgsrv 
shared_conn <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    host = "kwt-postgresql-azdb-1.postgres.database.azure.com",
    port = 5432,
    dbname = "shared",
    user = Sys.getenv("USER"),
    password = Sys.getenv("PASSWORD"),
    sslmode = "prefer"
)
DBI::dbGetQuery(shared_conn, "SELECT ST_extent(geometry) FROM bugs_matter.journeys6;")
#work out resolution from Lawrence's analysis
bbox <- list(xmin = -8.6709931, ymin = 41.14485, xmax = 1.817078176, ymax = 59.13162818)
dims <- list(x = 1000, y = 1000)
resolution <- list(
    x = (bbox$xmax - bbox$xmin) / dims$x,
    y = (bbox$ymax - bbox$ymin) / dims$y
)
full_bbox <- list(xmin = -30, ymin = 25, xmax = 30, ymax = 90)
full_dims <- list(
    x = (full_bbox$xmax - full_bbox$xmin) / resolution$x,
    y = (full_bbox$ymax - full_bbox$ymin) / resolution$y
)
habitats <- arcgislayers::arc_raster(
    imgsrv,
    bbox_crs = 4326,
    xmin = full_bbox$xmin,
    xmax = full_bbox$xmax,
    ymin = full_bbox$ymin,
    ymax = full_bbox$ymax,
    width = round(full_dims$x),
    height = round(full_dims$y)
)
# Import reclass table
habitat_lookup <- read.csv("dev/.data/UNEP_HabitatMap_Lookup.csv")
# Create a reclassification matrix
reclass_matrix <- habitat_lookup %>%
    dplyr::select(value, new) %>%
    as.matrix()
# Apply reclassification
habitats_reclass <- terra::classify(habitats, reclass_matrix)
raster::plot(habitats_reclass, xlim = c(-1,1), ylim = c(51,52))

raster::writeRaster(habitats_reclass, "dev/.data/habitats.tif")

#############################
# raster2pgsql -s 4326 -C -I -F -t 3614x5721 dev/.data/habitats.tif ref.habitats | psql -h kwt-postgresql-azdb-1.postgres.database.azure.com -p 5432 -U euanmckenzie -d bugs_matter

##--------------------------temp------------------------------##
#"https://knmi-ecad-assets-prd.s3.amazonaws.com/ensembles/data/months/ens/tg_0.1deg_day_2025_grid_ensmean.nc"
#"https://knmi-ecad-assets-prd.s3.amazonaws.com/ensembles/data/months/ens/tg_0.1deg_day_2024_grid_ensmean.nc"
#"https://knmi-ecad-assets-prd.s3.amazonaws.com/ensembles/data/months/ens/tg_0.1deg_day_2023_grid_ensmean.nc"
#"https://knmi-ecad-assets-prd.s3.amazonaws.com/ensembles/data/months/ens/tg_0.1deg_day_2022_grid_ensmean.nc"
#"https://knmi-ecad-assets-prd.s3.amazonaws.com/ensembles/data/months/ens/tg_0.1deg_day_2021_grid_ensmean.nc"

#fill in the missing days of 2025 with means from previous years
temp_25 <- terra::rast("dev/.data/tg_0.1deg_day_2025_grid_ensmean.nc")
means <- terra::global(temp_25, "mean", na.rm = TRUE)

#index of days without temp data
no_data <- which(is.na(means$mean))

temp_21_24 <- c(2021:2024) %>%
    sprintf("dev/.data/tg_0.1deg_day_%s_grid_ensmean.nc", .) %>%
    lapply(terra::rast)

# Function to calculate mean of a specific layer across multiple years
calculate_layer_mean <- function(raster_list, layer_index) {
    # Extract the specified layer from each raster
    layers <- lapply(raster_list, function(x) x[[layer_index]])

    # Stack the layers
    layer_stack <- terra::rast(layers)

    # Calculate mean across the stack
    mean_layer <- terra::mean(layer_stack, na.rm = TRUE)

    return(mean_layer)
}

# Fill in missing data in temp_25 using means from previous years
for (layer_idx in no_data) {
    message("Calculating layer", layer_idx)
    # Calculate mean for this layer from previous years. The extents are essentially identical
    mean_layer <- calculate_layer_mean(temp_21_24, layer_idx)

    # Replace NA values in temp_25 with the mean values
    temp_25[[layer_idx]] <- mean_layer
}

# Verify that all layers now have data
means_after <- terra::global(temp_25, "mean", na.rm = TRUE)
any(is.na(means_after$mean))  # Should be FALSE

terra::writeRaster(temp_21_24[[1]], "dev/.data/temp_2021.tif")
terra::writeRaster(temp_21_24[[2]], "dev/.data/temp_2022.tif")
terra::writeRaster(temp_21_24[[3]], "dev/.data/temp_2023.tif")
terra::writeRaster(temp_21_24[[4]], "dev/.data/temp_2024.tif")
terra::writeRaster(temp_25, "temp_2025.tif")

# Verify the data type of the raster files
# Using gdalinfo
# gdalinfo dev/.data/temp_2021.tif | findstr "Type"
# The output should show "Type=Float32" for 32BF

# Using terra in R
# Check data type of written files
temp_2021 <- terra::rast("dev/.data/temp_2021.tif")
print(terra::datatype(temp_2021))  # Should show "FLT4S" for 32-bit float

# raster2pgsql -d -s 4326 -C -I -N NULL -t 100x100 -x -r dev/.data/temp_2021.tif ref.temp_2021 | psql -h kwt-postgresql-azdb-1.postgres.database.azure.com -p 5432 -U euanmckenzie -d bugs_matter
# raster2pgsql -d -s 4326 -C -I -N NULL -t 100x100 -x -r dev/.data/temp_2022.tif ref.temp_2022 | psql -h kwt-postgresql-azdb-1.postgres.database.azure.com -p 5432 -U euanmckenzie -d bugs_matter
# raster2pgsql -d -s 4326 -C -I -N NULL -x -r dev/.data/temp_2023.tif ref.temp_2023 | psql -h kwt-postgresql-azdb-1.postgres.database.azure.com -p 5432 -U euanmckenzie -d bugs_matter
# raster2pgsql -d -s 4326 -C -I -N NULL -x -r dev/.data/temp_2024.tif ref.temp_2024 | psql -h kwt-postgresql-azdb-1.postgres.database.azure.com -p 5432 -U euanmckenzie -d bugs_matter
# raster2pgsql -d -s 4326 -C -I -N NULL -x -r dev/.data/temp_2025.tif ref.temp_2025 | psql -h kwt-postgresql-azdb-1.postgres.database.azure.com -p 5432 -U euanmckenzie -d bugs_matter


# journeys15_simplify_test <- journeys15_simplify[1:100, ]
# plot(journeys15_simplify_test)

# A SpatVector `lines` with 'year' and 'dayofyear' attributes
# And a list of raster stacks named by year (2021:2025) where each stack has 365 layers

# Function to extract mean raster values in batch
extract_means <- function(lines, raster_list) {
    results <- list()

    # Unique combinations of year and dayofyear
    unique_combos <- unique(as.data.frame(lines)[, c("year", "dayofyear")])

    for (i in seq_len(nrow(unique_combos))) {
        year <- unique_combos$year[i]
        doy <- unique_combos$dayofyear[i]

        # Get subset of lines for this year and doy
        subset_lines <- lines[lines$year == year & lines$dayofyear == doy, ]

        # Extract the correct raster layer
        if (as.character(year) %in% names(raster_list)) {
            raster <- raster_list[[as.character(year)]][[doy]]

            # Perform extraction
            # extracted_values <- terra::extract(raster, subset_lines, fun = mean, na.rm = TRUE)
            extracted_values <- exact_extract(raster, subset_lines, append_cols = c("id", "midpoint_time"), "mean")
            print(extracted_values)

            # Merge results with subset_lines
            subset_lines$temp <- extracted_values[, 3] # third column contains extracted values
            results[[i]] <- subset_lines
        }
    }

    # Combine all results into a single dataset
    do.call(rbind, results)
}

# Example usage
start.time <- Sys.time()
journeys15_simplify_temp <- extract_means(journeys15_simplify, eobs_list)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

temp_data <- as.data.frame(journeys15_simplify_temp) %>% dplyr::select(id, temp)

journeys16 <- left_join(journeys15, temp_data, by = "id")

#-----------------------historical journeys----------------------------#
# fill journeys with 2021-2024 data
sf::st_layers("dev/.data/journeys_for_data_cleaning_map_raw.gpkg")
journeys <- sf::st_read("dev/.data/journeys_for_data_cleaning_map_raw.gpkg", "journeys_for_data_cleaning_map_raw") %>%
    dplyr::rename_all(snakecase::to_snake_case)
# journeys$start <- as.POSIXct(journeys$start)
# min(journeys$start)
# max(journeys$start)

journeys <- journeys %>%
    dplyr::mutate(
        duration = as.numeric(time) / 60 / 60, # seconds to hours
        avg_speed = distance / duration,
        start_timestamp = as.POSIXct(start, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
        end_timestamp = as.POSIXct(end, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
        year = format(start_timestamp, format = "%Y"),
        photo_url = NA,
        app_timestamp = NA,
        database_timestamp = Sys.time()
    ) %>%
    dplyr::select(
        id,
        user_id,
        start_timestamp,
        end_timestamp,
        year,
        rain,
        distance,
        duration,
        avg_speed,
        splat_count,
        photo_url,
        vehicle_reg = vehicle_re,
        vehicle_class = vehicle_cl,
        vehicle_bhp = vehicle_bh,
        vehicle_year_of_manufacture = vehicle_ye,
        vehicle_date_first_registered = vehicle_da,
        vehicle_date_first_registered_uk = vehicle_da_1,
        vehicle_make = vehicle_ma,
        vehicle_make_code = vehicle_ma_1,
        vehicle_range = vehicle_ra,
        vehicle_model = vehicle_mo,
        vehicle_model_code = vehicle_mo_1,
        vehicle_trim = vehicle_tr,
        vehicle_width = vehicle_wi,
        vehicle_height = vehicle_he,
        vehicle_car_length = vehicle_ca,
        vehicle_colour = vehicle_co,
        vehicle_num_axles = vehicle_nu_2,
        vehicle_num_doors = vehicle_nu,
        vehicle_num_seats = vehicle_nu_1,
        vehicle_body_shape = vehicle_bo,
        vehicle_wheel_base = vehicle_wh,
        vehicle_wheel_plan = vehicle_wh_1,
        vehicle_kerb_height = vehicle_ke,
        vehicle_load_length = vehicle_lo,
        vehicle_rigid_artic = vehicle_ri,
        vehicle_driving_axle = vehicle_dr,
        vehicle_door_plan_literal = vehicle_do,
        vehicle_unladen_weight = vehicle_un,
        app_timestamp,
        database_timestamp,
        geom = geom
    )
any(duplicated(journeys$id))
DBI::dbExecute(conn, "DELETE FROM op.journey_check;")
DBI::dbExecute(conn, "DELETE FROM op.journeys;")
sf::st_write(journeys, conn, DBI::Id("op", "journeys"), append = TRUE)

#
sample <- sf::st_read(conn, query = "SELECT * FROM op.journeys_simplified WHERE id IN (23881476, 23881744, 23881747, 23883050, 23887748, 23887777, 23887778, 23887909, 23887952, 23888131);")
elevation <- raster::raster("dev/.data/elevation.tif")
extracted <- exactextractr::exact_extract(elevation, sample, append_cols = c("id"), "mean") %>%
    dplyr::arrange(id)

sample2 <- sf::st_read(shared_conn, query = "select id, st_transform(st_simplify(st_transform(geometry, 27700), 100), 4326) AS geom from bugs_matter.journeys9  WHERE id IN (23881476, 23881744, 23881747, 23883050, 23887748, 23887777, 23887778, 23887909, 23887952, 23888131);")



#countries to include: UK, Ireland, France, Spain, Portugal
#download from https://gadm.org/download_country.html
#license: free for non-commercial use: https://gadm.org/license.html#google_vignette

# codes <- c("GBR")

# append_country <- function(code) {
#     url <- sprintf("https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_%s_0.json", code)
#     json <- readLines(url)

#     sf::st_read(json, quiet = TRUE) %>%
#         dplyr::mutate(code = code) %>%
#         dplyr::select(
#             code,
#             name = COUNTRY,
#             geom = geometry
#         ) %>%
#         sf::st_transform(crs = 27700) %>%
#         sf::st_simplify(dTolerance = 100) %>%
#         sf::st_transform(crs = 4326) %>%
#         sf::st_write(dsn = conn, DBI::Id("ref", "countries"), append = TRUE)
# }

# DBI::dbExecute(conn, "DELETE FROM ref.country_subdivisions_1")
# DBI::dbExecute(conn, "DELETE FROM ref.countries;")
# lapply(
#     c("GBR", "FRA", "ESP", "PRT", "IRL"),
#     append_country
# )

# #regions
# #manually download and unzip level-1 for France & Spain and level-2 for UK

# append_regions <- function(code) {
#     path <- sprintf("dev/.data/gadm41_%s_1.json", code)
#     json <- readr::read_file(path) #needed to cope with file encoding (region names with unusual characters)

#     regions <- sf::st_read(json, quiet = TRUE) %>%
#         dplyr::mutate(country_code = code) %>%
#         dplyr::filter(GID_1 != "NA")

#     if (code == "GBR") {
#         regions <- regions %>%
#             dplyr::select(
#                 code = ISO_1,
#                 country_code,
#                 name = NAME_1,
#                 geom = geometry
#             ) %>%
#             dplyr::mutate(
#                 name = dplyr::case_when(
#                     name == "NA" ~ "England",
#                     .default = name
#                 ),
#                 code = dplyr::case_when(
#                     code == "NA" ~ "GB-ENG",
#                     .default = code
#                 )
#             )
#     } else {
#         regions <- regions %>%
#             dplyr::select(
#                 code = HASC_1,
#                 country_code,
#                 name = NAME_1,
#                 geom = geometry
#             )
#     }
#     tryCatch({
#         regions %>%
#             sf::st_transform(crs = 27700) %>%
#             sf::st_simplify(dTolerance = 100) %>%
#             sf::st_transform(crs = 4326) %>%
#             sf::st_write(dsn = conn, DBI::Id("ref", "country_subdivisions_1"), append = TRUE)
#     }, error = function(e) {
#         print(regions)
#         warning(sprintf("Error with %s", code))
#         warning(e$message)
#     })
# }

# DBI::dbExecute(conn, "DELETE FROM ref.country_subdivisions_1")
# mapply(
#     append_regions,
#     code = c("FRA", "ESP", "GBR")
# )

