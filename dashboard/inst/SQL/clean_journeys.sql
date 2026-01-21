select count(*), public.st_geometrytype(geom) from op.journeys  where start_timestamp > '2022-01-01T00:00:00Z'::TIMESTAMP GROUP BY public.st_geometrytype (geom);
select * from op.journeys  where splat_count IS NOT NULL order by start_timestamp DESC limit 100;
select max(id) from op.journeys;
WITH new_journeys AS (
    SELECT aj.*
    FROM op.journeys aj
    WHERE NOT EXISTS (
        SELECT 1 FROM op.journey_check jc WHERE aj.id = jc.id
    )
    LIMIT 10000
),
valid_geometries AS (
    SELECT
        id,
        geom,
        public.ST_Transform(geom, 3857) as geom_3857,
        public.ST_NPoints(geom) as point_count,
        public.ST_IsValid(geom) as is_valid,
        public.ST_IsEmpty(geom) as is_empty,
        public.ST_GeometryType(geom) as geom_type
    FROM new_journeys
),
valid_lines AS (
    SELECT *
    FROM valid_geometries
    WHERE point_count > 1
        AND is_valid
        AND NOT is_empty
        AND geom_type = 'ST_LineString'
),
segments AS (
    SELECT
        f.id AS line_id,
        public.ST_Length(public.ST_MakeLine(p1.geom, p2.geom)) AS segment_length
    FROM
        valid_lines f
        CROSS JOIN LATERAL public.ST_DumpPoints(f.geom_3857) p1
        CROSS JOIN LATERAL public.ST_DumpPoints(f.geom_3857) p2
    WHERE
        p1.path[1] = p2.path[1] - 1
        AND public.ST_IsValid(p1.geom)
        AND public.ST_IsValid(p2.geom)
),
long_segments AS (
    SELECT DISTINCT line_id
    FROM segments
    WHERE segment_length > 1000
),
vehicle_ferries AS (
    SELECT osm_id, public.ST_Transform(geom, 3857) as geom
    FROM ref.ferry_routes
    WHERE motor_vehicle = 'yes'
),
ferry_check AS (
    SELECT DISTINCT v.id
    FROM valid_geometries v
    CROSS JOIN vehicle_ferries f
    WHERE public.ST_DWithin(v.geom_3857, f.geom, 50)
)
INSERT INTO op.journey_check (
    SELECT
        aj.id,
        COALESCE(v.is_empty OR NOT v.is_valid, FALSE) AS invalid_geom,
        COALESCE(v.geom_type <> 'ST_LineString', FALSE) AS not_linestring,
        COALESCE(aj.id IN (SELECT line_id FROM long_segments), FALSE) AS gps_error,
        COALESCE(aj.id IN (SELECT id FROM ferry_check), FALSE) AS ferry,
        COALESCE(aj.distance <= 1.60934 OR aj.duration <= 3 / 60, FALSE) AS short,
        COALESCE(aj.avg_speed >= 60 * 1.60934, FALSE) AS fast,
        COALESCE(aj.avg_speed <= 3 * 1.60934, FALSE) AS slow,
        COALESCE(aj.splat_count >= 500, FALSE) AS high_count
    FROM
        new_journeys aj
    LEFT JOIN valid_geometries v ON v.id = aj.id
);



