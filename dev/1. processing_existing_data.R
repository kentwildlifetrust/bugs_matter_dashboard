library(sf)
library(dplyr)
library(tidyr)
library(pool)
library(RPostgres)
library(ggplot2)
library(osmdata)
library(stringr)
library(terra)
library(arcgislayers)
library(elevatr)
library(exactextractr)

# set up pool for postgis connections
pool <- dbPool(
  drv = RPostgres::Postgres(), 
  host = "kwt-postgresql-azdb-1.postgres.database.azure.com", 
  port = 5432, dbname = "shared", 
  user = Sys.getenv("kwt_user"), 
  password = Sys.getenv("kwt_password"),
  sslmode = "prefer"
)

# Import shapefiles downloaded from Coreo, join, remove dups, and write to file and PostGIS. ---- ----

# Define the folder path
folder_path <- "C:/Users/lawrenceb/Kent Wildlife Trust/EVD - Bugs Matter - General/10. Reporting/1. Analysis/Analysis 2025"

# List all .shp files in the folder (and its subfolders, if needed)
shp_files <- list.files(path = folder_path, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)

# Print the list of .shp files
print(shp_files)

journeys <- lapply(shp_files, st_read)
journeys <- do.call(rbind, journeys)

journeys1 <- journeys %>% st_transform(27700) %>%
  mutate(distance = as.numeric(distance) * 0.621371,
         duration = as.numeric(time) * 0.000277778,
         avg_speed = distance / duration,
         splatcount = as.numeric(count),
         start = as.POSIXct(start, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
         end = as.POSIXct(end, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
         year = format(start, format="%Y")) %>%
  group_by(year) %>%
  mutate(
    splat_rate = case_when(
      year == "2021" ~ (as.numeric(count)/144) / distance,
      year == "2022" ~ (as.numeric(count)/144) / distance,
      year == "2023" ~ (as.numeric(count)/577.2) / distance,
      year == "2024" ~ (as.numeric(count)/577.2) / distance,
      year == "2025" ~ (as.numeric(count)/577.2) / distance
    )
  ) %>%
  ungroup()

# Remove duplicates
dups <- duplicated(journeys1[,c("splatcount", "splat_rate", "year", "start", "end", "distance")])
summary(as.factor(dups))
journeys2 <- journeys1[!dups,]
journeys_dups <- journeys1[dups,]

st_write(journeys_dups, "C:/Users/lawrenceb/Kent Wildlife Trust/EVD - Bugs Matter - General/10. Reporting/1. Analysis/Analysis2025/outputs/journeys_duplicates.gpkg", append=F)

#Export dataset for data cleaning map
st_write(journeys2, "C:/Users/lawrenceb/Kent Wildlife Trust/EVD - Bugs Matter - General/10. Reporting/1. Analysis/Analysis2025/outputs/journeys_for_data_cleaning_map_raw.gpkg", append=F)

# Summarize journey counts and distances for each year
journey_cleaning_stats <- st_drop_geometry(journeys2) %>%
  group_by(year) %>%
  summarise(
    Step = "Raw Journey Count",
    Count = n(),
    Distance = sum(distance, na.rm = TRUE))
st_write(obj = journey_cleaning_stats, dsn = pool, Id(schema="bugs_matter", table = "journey_cleaning_stats"), drop=TRUE) 

st_write(obj = journeys2, dsn = pool, Id(schema="bugs_matter", table = "journeys"), drop=TRUE) 

# Filter out journeys with only a single vertex point/journeys with GPS errors (straight line section longer than 1 km). ---- ----

query <- "CREATE TABLE bugs_matter.journeys1 AS
WITH filtered_lines AS (
  -- Filter lines that have more than 1 point
  SELECT 
  l.* 
  FROM 
  bugs_matter.journeys l
  WHERE 
  ST_NPoints(l.geometry) > 1
),
segments AS (
  -- Split the lines into segments at their vertices and calculate the length of each segment
  SELECT
  f.id AS line_id,
  ST_Length(ST_MakeLine(p1.geom, p2.geom)) AS segment_length
  FROM
  filtered_lines f,
  LATERAL ST_DumpPoints(f.geometry) p1,
  LATERAL ST_DumpPoints(f.geometry) p2
  WHERE
  p1.path[1] = p2.path[1] - 1
),
long_segments AS (
  -- Filter the segments that are longer than 1000 meters
  SELECT DISTINCT
  line_id
  FROM
  segments
  WHERE
  segment_length > 1000
)
-- Select lines from the original table that don't match any of the long segment IDs
SELECT
    f.*
FROM
    filtered_lines f
WHERE
    f.id NOT IN (SELECT line_id FROM long_segments);"

# # SQL query to drop the table
# drop_query <- "DROP TABLE IF EXISTS bugs_matter.journeys1"
# 
# # Execute the query to delete the table
# dbExecute(pool, drop_query)

dbExecute(pool, query)

journeys3 <- st_read(pool, query = "SELECT * from bugs_matter.journeys1")

# Check for empty or invalid geometries  ---- ----
any_invalid <- st_is_empty(journeys3) | !st_is_valid(journeys3)
if (any(any_invalid)) {
  print("Invalid or empty geometries found!")
  print(journeys3[any_invalid, ])
}
# Remove invalid or empty geometries
journeys4 <- journeys3[!(st_is_empty(journeys3) | !st_is_valid(journeys3)), ]

# Summarize journey counts and distances for each year
journey_cleaning_stats <- st_drop_geometry(journeys4) %>%
  group_by(year) %>%
  summarise(
    Step = "GPS errors",
    Count = n(),
    Distance = sum(distance, na.rm = TRUE))
st_write(obj = journey_cleaning_stats, dsn = pool, Id(schema="bugs_matter", table = "journey_cleaning_stats"), append=TRUE) 
jcs <- st_read(pool, query = "SELECT * from bugs_matter.journey_cleaning_stats")

# Remove journeys on ferry routes ---- ---- 
journeysbbox <- st_bbox(journeys4) %>% st_as_sfc() %>% st_transform(4326) %>% st_bbox() # journeys bounding box in epsg:4326
journeysbbox
# available_features()
# available_tags("route")
ferry_routes <- opq(bbox = journeysbbox) %>%
  add_osm_feature(key = 'route', value = 'ferry') %>%
  osmdata_sf()
ferry_routes$osm_lines$motor_vehicle # identify car ferry attribute
vehicle_ferry_routes <- subset(ferry_routes$osm_lines, ferry_routes$osm_lines$motor_vehicle == "yes") # subset to vehicle ferries
st_crs(vehicle_ferry_routes)
vehicle_ferry_routes_bng <- vehicle_ferry_routes %>% st_transform(27700) # project ferry routes to epsg:27700
land <- ne_download(type="land", scale="large", category = "physical") %>% st_transform(27700)

# ggplot() +
#   # Land outline in green
#   geom_sf(data = land, color = "green", fill = NA) + 
#   # Car ferry routes in red
#   geom_sf(data = vehicle_ferry_routes_bng, color = "red") + 
#   # Journeys subset in black
#   geom_sf(data = journeys4[8000:8200, ], color = "black") + 
#   # Coordinate limits based on journeys4 bounding box
#   coord_sf(
#     xlim = c(st_bbox(journeys4)["xmin"], st_bbox(journeys4)["xmax"]),
#     ylim = c(st_bbox(journeys4)["ymin"], st_bbox(journeys4)["ymax"])
#   ) +
#   theme_minimal()

# Buffer the ferry routes
vehicle_ferry_routes_bng_buffer <- st_buffer(vehicle_ferry_routes_bng, dist = 50)

# Identify journeys intersecting with ferry routes
?st_intersects
intersections <- st_intersects(journeys4, vehicle_ferry_routes_bng_buffer, sparse = FALSE)

# Filter out intersecting journeys
journeys5 <- journeys4[!apply(intersections, 1, any), ]

# Summarize journey counts and distances for each year
journey_cleaning_stats <- st_drop_geometry(journeys5) %>%
  group_by(year) %>%
  summarise(
    Step = "Ferry routes",
    Count = n(),
    Distance = sum(distance, na.rm = TRUE))
st_write(obj = journey_cleaning_stats, dsn = pool, Id(schema="bugs_matter", table = "journey_cleaning_stats"), append=TRUE) 
jcs <- st_read(pool, query = "SELECT * from bugs_matter.journey_cleaning_stats")

#Remove very short journeys, with length/distance < 1 mile.  ---- ---- 
journeys5$linelength <- st_length(journeys5)
journeys5$linelength <- as.numeric(journeys5$linelength)
summary(journeys5$linelength)
journeys6 <- subset(journeys5, linelength > 1609.34 & distance > 1 & duration > 0.050)

# Summarize journey counts and distances for each year
journey_cleaning_stats <- st_drop_geometry(journeys6) %>%
  group_by(year) %>%
  summarise(
    Step = "Short journeys (<1 mile or 3 mins)",
    Count = n(),
    Distance = sum(distance, na.rm = TRUE))
st_write(obj = journey_cleaning_stats, dsn = pool, Id(schema="bugs_matter", table = "journey_cleaning_stats"), append=TRUE) 
jcs <- st_read(pool, query = "SELECT * from bugs_matter.journey_cleaning_stats")

# Remove journeys with average speed > 60 mph  ---- ---- 
summary(journeys6$avg_speed)
journeys7 <- subset(journeys6, avg_speed < 60 & avg_speed > 3)

# Summarize journey counts and distances for each year
journey_cleaning_stats <- st_drop_geometry(journeys7) %>%
  group_by(year) %>%
  summarise(
    Step = "Very slow or fast (<60 mph and >3 mph)",
    Count = n(),
    Distance = sum(distance, na.rm = TRUE))
st_write(obj = journey_cleaning_stats, dsn = pool, Id(schema="bugs_matter", table = "journey_cleaning_stats"), append=TRUE) 
jcs <- st_read(pool, query = "SELECT * from bugs_matter.journey_cleaning_stats")

# Remove journeys with splat count > 500 as it becomes very unlikely someone would sample more than 50 bugs per splatometer window. ---- ----  
summary(journeys7$splatcount)
journeys8 <- subset(journeys7, splatcount < 500)

# Summarize journey counts and distances for each year
journey_cleaning_stats <- st_drop_geometry(journeys8) %>%
  group_by(year) %>%
  summarise(
    Step = "Splat count <500",
    Count = n(),
    Distance = sum(distance, na.rm = TRUE))
st_write(obj = journey_cleaning_stats, dsn = pool, Id(schema="bugs_matter", table = "journey_cleaning_stats"), append=TRUE) 
jcs <- st_read(pool, query = "SELECT * from bugs_matter.journey_cleaning_stats")

# Filter out journeys with rain  ---- ---- 
summary(as.factor(journeys8$rain))
journeys9 <- subset(journeys8, rain == "false")

# Summarize journey counts and distances for each year
journey_cleaning_stats <- st_drop_geometry(journeys9) %>%
  group_by(year) %>%
  summarise(
    Step = "Rain",
    Count = n(),
    Distance = sum(distance, na.rm = TRUE))
st_write(obj = journey_cleaning_stats, dsn = pool, Id(schema="bugs_matter", table = "journey_cleaning_stats"), append=TRUE) 
jcs <- st_read(pool, query = "SELECT * from bugs_matter.journey_cleaning_stats")

journeys9$linelength <- NULL

st_write(obj = journeys9, dsn = pool, Id(schema="bugs_matter", table = "journeys2"), drop=TRUE) 

journeys10 <- st_read(pool, query = "SELECT * from bugs_matter.journeys2")

# Join admin boundary data  ---- ---- 

regionboundaries <- st_read(pool, query = "
    SELECT 
        nuts118nm, country, 
        ST_MakeValid(ST_Simplify(geometry, 500)) AS geom
    FROM 
        bugs_matter.regionboundaries")
plot(regionboundaries)

journeys11 <- journeys10 %>%
  st_join(regionboundaries, largest = TRUE)

journeys11 <- journeys11 %>%
  rename(
    region = nuts118nm
  )

journeys11$country <- as.factor(journeys11$country)
levels(journeys11$country) <- c("England", "Ireland", "Northern Ireland", "Scotland", "Wales")
summary(journeys11$country)

# Calculate the journey midpoint time  ---- ---- 
journeys11$midpoint_time <- as.POSIXct((as.numeric(journeys11$start) + as.numeric(journeys11$end)) / 2, origin = "1970-01-01", tz = "UTC")
journeys11$dayofyear <- as.numeric(format(as.Date(journeys11$midpoint_time), "%j"))
journeys11$timeofday <- format(journeys11$midpoint_time, "%H:%M:%S")

st_write(obj = journeys11, dsn = pool, Id(schema="bugs_matter", table = "journeys4"), append=FALSE) 

journeys11 <- st_read(pool, query = "SELECT * from bugs_matter.journeys4")

# vehicle type  ---- ---- 
summary(as.factor(journeys5$vehicle_do))
summary(as.factor(journeys5$vehicle_cl))

journeys11$vehicle_cl <- as.factor(journeys11$vehicle_cl)
summary(journeys11$vehicle_cl)
journeys11$vehicle_cl[is.na(journeys11$vehicle_cl)] <- "Other"
journeys11$vehicle_cl[journeys11$vehicle_cl == "PSV"] <- "Other"
summary(journeys11$vehicle_cl)
journeys11$vehicle_cl <- droplevels(journeys11$vehicle_cl)
summary(journeys11$vehicle_cl)

journeys12 <- cbind(journeys11, st_coordinates(st_centroid(journeys11)))

journeys13 <- journeys12 %>%
  group_by(year) %>%
  mutate(
    cm_miles_offset = case_when(
      year == "2021" ~ 144 * distance,
      year == "2022" ~ 144 * distance,
      year == "2023" ~ 577.2 * distance,
      year == "2024" ~ 577.2 * distance,
      year == "2025" ~ 577.2 * distance)) %>%
  ungroup()

journeys13$log_cm_miles_offset <- log(journeys13$cm_miles_offset)

st_write(obj = journeys13, dsn = pool, Id(schema="bugs_matter", table = "journeys5"), append=FALSE) 

journeys13 <- st_read(pool, query = "SELECT * from bugs_matter.journeys5")

# Extract elevation data ---- ----

# Import journeys and simplify
journeys13_simplify <- st_read(pool, query = "SELECT *, ST_Transform(ST_Simplify(geometry, 100), 4326) AS geometry_simplify FROM bugs_matter.journeys5")
st_geometry(journeys13_simplify) <- "geometry_simplify"
journeys13_simplify$geometry <- NULL

# Download elevation raster
elevation <- get_elev_raster(journeys13_simplify, z = 7)
elevation
plot(elevation)
plot(elevation, xlim = c(-0.5,0.5), ylim = c(51.3,51.7))

?exact_extract
extracted <- exact_extract(elevation, journeys13_simplify, append_cols = c("id"), 'mean')

journeys13a <- left_join(journeys13, extracted, by = "id")
journeys13a$elevation <- journeys13a$mean
journeys13a$mean <- NULL

st_write(obj = journeys13a, dsn = pool, Id(schema="bugs_matter", table = "journeys6"), append=FALSE) 

#Extract habitat ---- ----

# Import buffered simplified journey routes
journeys13_simplify_buffer <- st_read(pool, query = "SELECT *, ST_Transform(ST_Buffer(ST_Simplify(geometry, 100), 250), 4326) AS geometry_simplify FROM bugs_matter.journeys6")
st_geometry(journeys13_simplify_buffer) <- "geometry_simplify"
journeys13_simplify_buffer$geometry <- NULL

# Download habitat raster
url <- "https://data-gis.unep-wcmc.org/server/rest/services/NatureMap/NatureMap_HabitatTypes/ImageServer"
imgsrv <- arc_open(url)
imgsrv
bbox <- ext(journeys13_simplify_buffer)
bbox
habitats <- arc_raster(
  imgsrv,
  bbox_crs = 4326,
  width = 10000,
  height = 10000,
  xmin = bbox[1],
  ymin = bbox[3],
  xmax = bbox[2],
  ymax = bbox[4]
)

# Convert raster to factor
habitats <- as.factor(habitats)
habitats
plot(habitats)
plot(habitats, xlim = c(-0.5,0.5), ylim = c(51.3,51.7))
plot(journeys13_simplify_buffer[1], add=T)
#plot(journeys13_simplify_buffer, add=T)

# Import reclass table
habitat_lookup <- read.csv("C:/Users/lawrenceb/Kent Wildlife Trust/EVD - Bugs Matter - General/10. Reporting/1. Analysis/Analysis 2025/Habitats/UNEP_HabitatMap_Lookup.csv")
# Create a reclassification matrix
reclass_matrix <- habitat_lookup %>% dplyr::select(value, new) %>% as.matrix()
reclass_matrix
?classify
# Apply reclassification
habitats_reclass <- classify(habitats, reclass_matrix)
habitats_reclass <- as.factor(habitats_reclass)
habitats_reclass <- droplevels(habitats_reclass)
plot(habitats_reclass, xlim = c(-1,1), ylim = c(51,52))

extracted <- exact_extract(habitats_reclass, journeys13_simplify_buffer, append_cols = c("id"), 'frac')

journeys14 <- st_read(pool, query = "SELECT * from bugs_matter.journeys6")

journeys14a <- left_join(journeys14, extracted, by = "id")

# Create a named vector of new column names
rename_map <- c(
  "frac_1" = "forest",
  "frac_2" = "savanna",
  "frac_3" = "shrubland",
  "frac_4" = "grassland",
  "frac_5" = "wetland",
  "frac_6" = "rocky",
  "frac_7" = "desert",
  "frac_8" = "marine",
  "frac_9" = "arable",
  "frac_10" = "pastureland",
  "frac_11" = "plantation",
  "frac_12" = "rural_garden",
  "frac_13" = "urban"
)

# Rename only the columns that exist in `journeys14`
journeys15 <- journeys14a %>%
  rename_with(~ rename_map[.x], .cols = names(rename_map)[names(rename_map) %in% names(journeys14a)])

st_write(obj = journeys15, dsn = pool, Id(schema="bugs_matter", table = "journeys7"), append=FALSE) 

journeys15 <- st_read(pool, query = "SELECT * from bugs_matter.journeys7")

# # Extract E-OBS gridded mean temperature data to routes. ---- ----

# Import journeys and simplify
journeys15_simplify <- st_read(pool, query = "SELECT *, ST_Transform(ST_Simplify(geometry, 100), 4326) AS geometry_simplify FROM bugs_matter.journeys7")
st_geometry(journeys15_simplify) <- "geometry_simplify"
journeys15_simplify$geometry <- NULL

eobs25 <- rast("https://knmi-ecad-assets-prd.s3.amazonaws.com/ensembles/data/months/ens/tg_0.1deg_day_2025_grid_ensmean.nc")
eobs24 <- rast("https://knmi-ecad-assets-prd.s3.amazonaws.com/ensembles/data/months/ens/tg_0.1deg_day_2024_grid_ensmean.nc")
eobs23 <- rast("https://knmi-ecad-assets-prd.s3.amazonaws.com/ensembles/data/months/ens/tg_0.1deg_day_2023_grid_ensmean.nc")
eobs22 <- rast("https://knmi-ecad-assets-prd.s3.amazonaws.com/ensembles/data/months/ens/tg_0.1deg_day_2022_grid_ensmean.nc")
eobs21 <- rast("https://knmi-ecad-assets-prd.s3.amazonaws.com/ensembles/data/months/ens/tg_0.1deg_day_2021_grid_ensmean.nc")

eobs_list <- list(
  "2025" = eobs25,
  "2024" = eobs24,
  "2023" = eobs23,
  "2022" = eobs22,
  "2021" = eobs21
)

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
      #extracted_values <- terra::extract(raster, subset_lines, fun = mean, na.rm = TRUE)
      extracted_values <- exact_extract(raster, subset_lines, append_cols = c("id", "midpoint_time"), 'mean')
      print(extracted_values)

      # Merge results with subset_lines
      subset_lines$temp <- extracted_values[, 3]  # third column contains extracted values
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

st_write(obj = journeys16, dsn = pool, Id(schema="bugs_matter", table = "journeys8"), append=FALSE) 

