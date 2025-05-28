-- Active: 1746635410704@@kwt-postgresql-azdb-1.postgres.database.azure.com@5432@bugs_matter
SET statement_timeout = '5min';  -- or any other duration
DROP VIEW IF EXISTS op.elevation_intersection;

CREATE MATERIALIZED VIEW op.journeys_simplified AS (
    WITH journeys AS (
        SELECT j.id, j.geom
        FROM op.journeys j
        INNER JOIN (
            SELECT jc.id FROM op.journey_check jc
            WHERE NOT (
                jc.invalid_geom OR
                jc.not_linestring OR
                jc.gps_error OR
                jc.ferry OR
                jc.short OR
                jc.fast OR
                jc.slow OR
                jc.high_count
            )
        ) c ON j.id = c.id
    )
    SELECT id, public.st_transform(public.st_simplify(public.st_transform(geom, 27700), 100), 4326) AS geom
    FROM journeys
) WITH DATA;

WITH journeys AS (
    SELECT j.id, j.st_transform AS geom from op.journeys_simplified j
    LIMIT 5
),
checked_journeys AS (
    SELECT j.id, j.geom AS geom
    FROM op.journey_check jc
    LEFT JOIN journeys j ON j.id = jc.id
    WHERE
        NOT (
            invalid_geom
            OR not_linestring
            OR gps_error
            OR ferry
            OR short
            OR fast
            OR slow
            OR high_count
        )
),
elevation_values AS (
    SELECT
        checked_journeys.id,
        AVG((pixel).val) AS elevation_value
    FROM checked_journeys
    INNER JOIN ref.elevation
        ON public.st_intersects(elevation.rast, checked_journeys.geom)
    CROSS JOIN LATERAL
        public.st_pixelaspolygons(public.st_setsrid(elevation.rast, 4326)) AS pixel
    WHERE
        public.st_intersects((pixel).geom, checked_journeys.geom)
    GROUP BY checked_journeys.id
),
habitat_values AS (
    SELECT
        checked_journeys.id,
        COUNT(CASE WHEN (pixel).val = 1 THEN 1 END)::float /
        COUNT(*) AS forest,
        COUNT(CASE WHEN (pixel).val = 2 THEN 1 END)::float /
        COUNT(*) AS savanna,
        COUNT(CASE WHEN (pixel).val = 3 THEN 1 END)::float /
        COUNT(*) AS shrubland,
        COUNT(CASE WHEN (pixel).val = 4 THEN 1 END)::float /
        COUNT(*) AS grassland,
        COUNT(CASE WHEN (pixel).val = 5 THEN 1 END)::float /
        COUNT(*) AS wetland,
        COUNT(CASE WHEN (pixel).val = 6 THEN 1 END)::float /
        COUNT(*) AS rocky,
        COUNT(CASE WHEN (pixel).val = 7 THEN 1 END)::float /
        COUNT(*) AS desert,
        COUNT(CASE WHEN (pixel).val = 8 THEN 1 END)::float /
        COUNT(*) AS marine,
        COUNT(CASE WHEN (pixel).val = 9 THEN 1 END)::float /
        COUNT(*) AS arable,
        COUNT(CASE WHEN (pixel).val = 10 THEN 1 END)::float /
        COUNT(*) AS pastureland,
        COUNT(CASE WHEN (pixel).val = 11 THEN 1 END)::float /
        COUNT(*) AS plantation,
        COUNT(CASE WHEN (pixel).val = 12 THEN 1 END)::float /
        COUNT(*) AS rural_garden,
        COUNT(CASE WHEN (pixel).val = 13 THEN 1 END)::float /
        COUNT(*) AS urban
    FROM checked_journeys
    INNER JOIN ref.habitats
        ON public.st_intersects(habitats.rast, checked_journeys.geom)
    CROSS JOIN LATERAL
        public.st_pixelaspolygons(public.st_setsrid(habitats.rast, 4326)) AS pixel
    WHERE
        public.st_intersects((pixel).geom, checked_journeys.geom)
    GROUP BY checked_journeys.id
)
SELECT
    e.id,
    e.elevation_value,
    h.forest,
    h.savanna,
    h.shrubland,
    h.grassland,
    h.wetland,
    h.rocky,
    h.desert,
    h.marine,
    h.arable,
    h.pastureland,
    h.plantation,
    h.rural_garden,
    h.urban
FROM journeys j
LEFT JOIN elevation_values e ON j.id = e.id
LEFT JOIN habitat_values h ON j.id = h.id;


