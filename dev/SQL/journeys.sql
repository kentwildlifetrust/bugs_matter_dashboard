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


DROP MATERIALIZED VIEW bugs_matter.journeys_server;
CREATE MATERIALIZED VIEW bugs_matter.journeys_server AS (
    SELECT
        j.id,
        j.start,
        j.end,
        j.distance,
        j.vehicle_cl,
        EXTRACT(year FROM j.end)::integer AS year,
        r.objectid::integer AS region_id,
        public.st_transform(public.ST_Simplify(j.geometry, 100), 4326) AS geom
    FROM bugs_matter.journeys5 j
    JOIN admin_boundaries.uk_boundary b ON public.st_intersects(j.geometry, b.geom)
    LEFT JOIN bugs_matter.regionboundaries r ON j.region = r.nuts118nm
) WITH DATA;

GRANT SELECT ON bugs_matter.journeys_server TO "BugsMatterReadOnly";

DROP MATERIALIZED VIEW bugs_matter.regions_app;

CREATE MATERIALIZED VIEW bugs_matter.regions_app AS (
    SELECT
        r.objectid AS id,
        r.nuts118nm AS name,
        -- public.st_astext(public.st_boundary(public.st_transform(public.ST_Simplify(r.geometry, 50), 4326))) AS geom
        public.st_astext(public.st_transform(public.ST_Simplify(r.geometry, 100), 4326)) AS geom
    FROM bugs_matter.regionboundaries r
) WITH DATA;

GRANT SELECT ON bugs_matter.regions_server TO "BugsMatterReadOnly";


CREATE TEMP TABLE a (
  id SERIAL PRIMARY KEY,
  name TEXT
);

CREATE TEMP TABLE b (
  id SERIAL PRIMARY KEY,
  a_id INT,
  hobby TEXT
);



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