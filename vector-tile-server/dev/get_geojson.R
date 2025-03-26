#postgresql -> geojson

library(magrittr)

conn <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  host = "kwt-postgresql-azdb-1.postgres.database.azure.com",
  port = 5432,
  dbname = "shared",
  user = Sys.getenv("USER"),
  password = Sys.getenv("PASSWORD"),
  sslmode = "prefer"
)

lines <- "SELECT
            j.id,
            public.st_astext(public.st_transform(public.ST_Simplify(j.geometry, 100), 4326)) AS geom
        FROM bugs_matter.journeys5 j;" %>%
        DBI::dbGetQuery(conn, .) %>%
      dplyr::mutate(geom = sf::st_as_sfc(.$geom)) %>%
      sf::st_as_sf(crs = 4326)

sf::st_write(lines, "dev/.data/lines.geojson")


#geojson -> vector tiles

#https://github.com/mapbox/tippecanoe?tab=readme-ov-file

# install brew on WSL

#/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"


# insall tippecanoe

#generate tiles
# tippecanoe -o journeys-2021-to-2024.mbtiles --drop-densest-as-needed -Z0 -z12 dev/.data/lines.geojson

#-z12 sets the maximum zoom level


# check a valid zoom, x and y combo

#sqlite3 mytiles.mbtiles "SELECT name, value FROM metadata;"

