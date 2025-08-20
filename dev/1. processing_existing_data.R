# Load packages
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
library(rnaturalearth)

#Set up Database connection
pool <- dbPool(drv = RPostgres::Postgres(),
               host = "kwt-postgresql-azdb-1.postgres.database.azure.com",
               port = 5432, dbname = "shared",
               user = Sys.getenv("user"),
               password = Sys.getenv("password"),
               sslmode = "prefer")

# dbExecute(pool, 'GRANT SELECT ON TABLE bugs_matter.journeys5 TO "BugsMatterReadOnly";')

# Import shapefiles downloaded from Coreo, join, remove dups, and write to file and PostGIS. ---- ----
folder_path <- "C:/Users/euanm/Downloads/OneDrive_1_19-08-2025"
shp_files <- list.files(path = folder_path, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
print(shp_files)
journeys <- lapply(shp_files, st_read)
journeys <- do.call(rbind, journeys)

#Add and reformat some variables, and calculate splat rates (splatometer discontinued in 2023). ---- ----
journeys1 <- journeys %>%
  mutate(duration = as.numeric(time) * 0.000277778,
         avg_speed = distance / duration,
         splat_count = as.numeric(count),
         start = as.POSIXct(start, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
         end = as.POSIXct(end, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
         year = format(start, format="%Y")) %>%
  select(-count) %>%
  group_by(year) %>%
  mutate(
    splat_rate = case_when(
      year == "2021" ~ (splat_count/144) / distance,
      year == "2022" ~ (splat_count/144) / distance,
      year == "2023" ~ (splat_count/577.2) / distance,
      year == "2024" ~ (splat_count/577.2) / distance,
      year == "2025" ~ (splat_count/577.2) / distance
    )
  ) %>%
  ungroup()

# Remove duplicate journeys
dups <- duplicated(journeys1[,c("splat_count", "splat_rate", "year", "start", "end", "distance")])
summary(as.factor(dups))
journeys2 <- journeys1[!dups,]
journeys_dups <- journeys1[dups,]

st_write(journeys_dups, "C:/Users/lawrenceb/Kent Wildlife Trust/EVD - Bugs Matter - General/10. Reporting/1. Analysis/Analysis 2025/outputs/journeys_duplicates.gpkg", append=F)

#Export raw dataset for data cleaning map
st_write(journeys2, "C:/Users/lawrenceb/Kent Wildlife Trust/EVD - Bugs Matter - General/10. Reporting/1. Analysis/Analysis 2025/outputs/journeys_for_data_cleaning_map_raw.gpkg", append=F)

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
  ST_DistanceSphere(p1.geom, p2.geom) AS segment_length
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

# SQL query to drop the table
drop_query <- "DROP TABLE IF EXISTS bugs_matter.journeys1"

# Execute the query to delete the table
dbExecute(pool, drop_query)

dbExecute(pool, query)

# Query and fix invalid or empty geometries ---- ----
# Remove duplicate vertices in lines. This maintains LINESTRING geometry type.
journeys3 <- st_read(pool, query = "SELECT *, ST_RemoveRepeatedPoints(geometry) AS geometry FROM bugs_matter.journeys1")

# Delete remaining journeys with invalid geometry.
invalid <- st_is_valid(journeys3, reason=T)
unique(invalid)
empty <- st_is_empty(journeys3)
unique(empty)
journeys4 <- journeys3[st_is_valid(journeys3), ]

# Summarize journey counts and distances for each year
journey_cleaning_stats <- st_drop_geometry(journeys4) %>%
  group_by(year) %>%
  summarise(
    Step = "GPS errors",
    Count = n(),
    Distance = sum(distance, na.rm = TRUE))
st_write(obj = journey_cleaning_stats, dsn = pool, Id(schema="bugs_matter", table = "journey_cleaning_stats"), append=TRUE)
jcs <- st_read(pool, query = "SELECT * from bugs_matter.journey_cleaning_stats")
jcs

# Remove journeys on ferry routes ---- ----
journeysbbox <- st_bbox(journeys4) %>% st_as_sfc() %>% st_bbox() # journeys bounding box in epsg:4326
journeysbbox

# Download ferry routes from OSM.
# available_features()
# available_tags("route")
ferry_routes <- opq(bbox = journeysbbox) %>%
  add_osm_feature(key = 'route', value = 'ferry') %>%
  osmdata_sf()
# Identify car ferry attribute
ferry_routes$osm_lines$motor_vehicle
# Subset to vehicle ferries
vehicle_ferry_routes <- subset(ferry_routes$osm_lines, ferry_routes$osm_lines$motor_vehicle == "yes")
st_crs(vehicle_ferry_routes)

# # Download land and plot ferry routes
# land <- ne_download(type="land", scale="large", category = "physical")
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
vehicle_ferry_routes_buffer <- st_buffer(vehicle_ferry_routes, dist = 50)

# Identify journeys intersecting with ferry routes
intersections <- st_intersects(journeys4, vehicle_ferry_routes_buffer, sparse = FALSE)

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
jcs

#Remove very short journeys, with length/distance < 1 km.  ---- ----
journeys5$linelength <- st_length(journeys5)
journeys5$linelength <- as.numeric(journeys5$linelength)
summary(journeys5$linelength)
journeys6 <- subset(journeys5, linelength > 1000 & distance > 1 & duration > 0.050)

# Summarize journey counts and distances for each year
journey_cleaning_stats <- st_drop_geometry(journeys6) %>%
  group_by(year) %>%
  summarise(
    Step = "Short journeys (<1 km or 3 mins)",
    Count = n(),
    Distance = sum(distance, na.rm = TRUE))
st_write(obj = journey_cleaning_stats, dsn = pool, Id(schema="bugs_matter", table = "journey_cleaning_stats"), append=TRUE)
jcs <- st_read(pool, query = "SELECT * from bugs_matter.journey_cleaning_stats")

# Remove journeys with average speed > 60 mph  ---- ----
summary(journeys6$avg_speed)
journeys7 <- subset(journeys6, avg_speed < 97 & avg_speed > 5)

# Summarize journey counts and distances for each year
journey_cleaning_stats <- st_drop_geometry(journeys7) %>%
  group_by(year) %>%
  summarise(
    Step = "Very slow or fast (<97 kph and >5 kph)",
    Count = n(),
    Distance = sum(distance, na.rm = TRUE))
st_write(obj = journey_cleaning_stats, dsn = pool, Id(schema="bugs_matter", table = "journey_cleaning_stats"), append=TRUE)
jcs <- st_read(pool, query = "SELECT * from bugs_matter.journey_cleaning_stats")

# Remove journeys with splat count > 500 as it becomes very unlikely someone would sample more than 50 bugs per splatometer window. ---- ----
summary(journeys7$splat_count)
journeys8 <- subset(journeys7, splat_count < 500)

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

# Join region and country values from boundaries layer  ---- ----

regionboundaries <- st_read(pool, query = "
    SELECT
        region, country,
        ST_MakeValid(ST_Simplify(geometry, 0.0009)) AS geom
    FROM
        bugs_matter.regionboundaries1")

plot(regionboundaries[1],
     xlim = c(0, 1.4),
     ylim = c(50.7, 51.5))

journeys11 <- journeys10 %>%
  st_join(regionboundaries, largest = TRUE)

summary(as.factor(journeys11$region))
summary(as.factor(journeys11$country))

journeys11$country <- as.factor(journeys11$country)
journeys11$region <- as.factor(journeys11$region)

# Calculate the journey midpoint time  ---- ----
journeys11$midpoint_time <- as.POSIXct((as.numeric(journeys11$start) + as.numeric(journeys11$end)) / 2, origin = "1970-01-01", tz = "UTC")
journeys11$dayofyear <- as.numeric(format(as.Date(journeys11$midpoint_time), "%j"))
journeys11$timeofday <- format(journeys11$midpoint_time, "%H:%M:%S")

st_write(obj = journeys11, dsn = pool, Id(schema="bugs_matter", table = "journeys4"), append=FALSE)

#journeys11 <- st_read(pool, query = "SELECT * from bugs_matter.journeys4")

# vehicle type  ---- ----
summary(as.factor(journeys11$vehicle_do))
summary(as.factor(journeys11$vehicle_cl))

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

#journeys13 <- st_read(pool, query = "SELECT * from bugs_matter.journeys5")

# Extract elevation data ---- ----

# Import journeys and simplify
journeys13_simplify <- st_read(pool, query = "SELECT id, ST_Simplify(geometry, 0.0009) AS geom FROM bugs_matter.journeys5")
summary(st_geometry_type(journeys13_simplify))

# Download elevation raster
elevation <- get_elev_raster(journeys13_simplify, z = 7)
elevation
plot(elevation)
plot(elevation, xlim = c(0, 1.4), ylim = c(50.7, 51.5))

?exact_extract
extracted <- exact_extract(elevation, journeys13_simplify, append_cols = c("id"), 'mean')

journeys14 <- left_join(journeys13, extracted, by = "id")
journeys14$elevation <- journeys14$mean
journeys14$mean <- NULL

st_write(obj = journeys14, dsn = pool, Id(schema="bugs_matter", table = "journeys6"), append=FALSE)

#Extract habitat ---- ----

# Import buffered simplified journey routes
journeys14_simplify_buffer <- st_read(pool, query = "SELECT id,
                                      ST_Transform(ST_Buffer(ST_Simplify(ST_Transform(geometry, 3857), 100), 250), 4326) AS
                                      geometry FROM bugs_matter.journeys6")

# Download habitat raster
url <- "https://data-gis.unep-wcmc.org/server/rest/services/NatureMap/NatureMap_HabitatTypes/ImageServer"
imgsrv <- arc_open(url)
imgsrv
bbox <- ext(journeys14_simplify_buffer)
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
plot(habitats, xlim = c(0, 1.4), ylim = c(50.7, 51.5))
plot(journeys14_simplify_buffer[1], add=T)

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
plot(habitats_reclass, xlim = c(0, 1.4), ylim = c(50.7, 51.5))

extracted <- exact_extract(habitats_reclass, journeys14_simplify_buffer, append_cols = c("id"), 'frac')

journeys14 <- st_read(pool, query = "SELECT * from bugs_matter.journeys6")

journeys15 <- left_join(journeys14, extracted, by = "id")

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
  "frac_10" = "pasture",
  "frac_12" = "rural_garden",
  "frac_13" = "urban"
)

# Rename only the columns that exist in `journeys14`
journeys16 <- journeys15 %>%
  rename_with(~ rename_map[.x], .cols = names(rename_map)[names(rename_map) %in% names(journeys15)])

st_write(obj = journeys16, dsn = pool, Id(schema="bugs_matter", table = "journeys7"), append=FALSE)

journeys16 <- st_read(pool, query = "SELECT * from bugs_matter.journeys7")

# # Extract E-OBS gridded mean temperature data to routes. ---- ----

# Import journeys and simplify
journeys16_simplify <- st_read(pool, query = "SELECT id, year, dayofyear, midpoint_time,
                               ST_Simplify(geometry, 0.0009) AS
                               geometry FROM bugs_matter.journeys7")

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
journeys16_simplify_temp <- extract_means(journeys16_simplify, eobs_list)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

temp_data <- as.data.frame(journeys16_simplify_temp) %>% dplyr::select(id, temp)

journeys17 <- left_join(journeys16, temp_data, by = "id")

summary(journeys17$temp)

#Fill NA temp values with average to avoid ommision from model. #euan
journeys18 <- journeys17 %>%
  group_by(group_date = as.Date(start)) %>%
  mutate(temp = ifelse(is.na(temp), mean(temp, na.rm = TRUE), temp)) %>%
  ungroup() %>%
  select(-group_date)  # remove it

summary(journeys18$temp)

st_write(obj = journeys18, dsn = pool, Id(schema="bugs_matter", table = "journeys9"), append=FALSE)
