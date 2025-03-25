DROP MATERIALIZED VIEW bugs_matter.journeys_app;

CREATE MATERIALIZED VIEW bugs_matter.journeys_app AS (
    SELECT
        j.id,
        j.start,
        j.end,
        public.st_astext(public.st_transform(public.ST_Simplify(j.geometry, 500), 4326)) AS geom
    FROM bugs_matter.journeys5 j
    JOIN admin_boundaries.uk_boundary b ON public.st_intersects(j.geometry, b.geom)
    WHERE j.end > '2021-05-20'
) WITH DATA;

-- DROP TABLE bugs_matter.trends_app;
-- CREATE TABLE bugs_matter.trends_app (
--   region_name VARCHAR(255),
--   baseline_year INT,
--   comparison_year INT,
--   estimate NUMERIC,
--   low NUMERIC,
--   high NUMERIC,
--   PRIMARY KEY (region_name, baseline_year, comparison_year)
-- )

-- DROP MATERIALIZED VIEW bugs_matter.trends_regions;
-- CREATE MATERIALIZED VIEW bugs_matter.trends_regions AS (
--   SELECT r.nuts118nm AS region_name,
--     t.estimate,
--     t.low,
--     t.high,
--     public.st_astext(public.st_transform(public.st_simplify(geometry, 200), 4326)) AS geom
--   FROM bugs_matter.regionboundaries r
--   LEFT JOIN bugs_matter.trends_app t ON r.nuts118nm = t.region_name
--   WHERE t.baseline_year = 2021 AND t.comparison_year = 2024
-- ) WITH DATA