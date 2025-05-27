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

