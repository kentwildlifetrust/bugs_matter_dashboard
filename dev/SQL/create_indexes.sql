CREATE INDEX IF NOT EXISTS idx_journeys_id ON op.journeys(id);
CREATE INDEX IF NOT EXISTS idx_journey_check_id ON op.journey_check(id);
CREATE INDEX IF NOT EXISTS idx_journeys_geom ON op.journeys USING GIST(geom);
CREATE INDEX IF NOT EXISTS idx_ferry_routes_geom ON ref.ferry_routes USING GIST(geom);