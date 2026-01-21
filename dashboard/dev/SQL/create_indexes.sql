-- Active: 1746635410704@@kwt-postgresql-azdb-1.postgres.database.azure.com@5432@bugs_matter
CREATE INDEX IF NOT EXISTS idx_journeys_id ON op.journeys(id);
CREATE INDEX IF NOT EXISTS idx_journey_check_id ON op.journey_check(id);
CREATE INDEX IF NOT EXISTS idx_journeys_geom ON op.journeys USING GIST(geom);
CREATE INDEX IF NOT EXISTS idx_ferry_routes_geom ON ref.ferry_routes USING GIST(geom);

-- Disable triggers before creating extension
SET session_replication_role = 'replica';

CREATE EXTENSION postgis_raster;

-- Re-enable triggers after extension creation
SET session_replication_role = 'origin';

CREATE INDEX IF NOT EXISTS idx_journey_check_id ON op.journey_check (id);

CREATE INDEX IF NOT EXISTS idx_journeys_geom ON op.journeys USING GIST (geom);

CREATE INDEX IF NOT EXISTS idx_ferry_routes_geom ON ref.vehicle_ferry_routes USING GIST (geom);