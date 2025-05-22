WITH new_journeys AS (
    SELECT aj.*
    FROM op.all_journeys aj
    WHERE NOT EXISTS
        (SELECT jc.id FROM op.journey_cleaning jc WHERE aj.id = jc.id)
),
-- Filter out invalid geometries and ensure minimum points
filtered_lines AS (
  SELECT
    l.*
  FROM
    new_journeys l
  WHERE
    public.ST_NPoints(public.st_transform(l.geom, 27700)) > 1
    AND public.ST_IsValid(l.geom)
    AND NOT public.ST_IsEmpty(l.geom)
    AND public.ST_GeometryType(l.geom) = 'ST_LineString'
),
segments AS (
  -- Split the lines into segments at their vertices and calculate the length of each segment
  SELECT
    f.id AS line_id,
    public.ST_Length(public.ST_MakeLine(p1.geom, p2.geom)) AS segment_length
  FROM
    filtered_lines f,
    LATERAL public.ST_DumpPoints(f.geom) p1,
    LATERAL public.ST_DumpPoints(f.geom) p2
  WHERE
    p1.path[1] = p2.path[1] - 1
    AND public.ST_IsValid(p1.geom)
    AND public.ST_IsValid(p2.geom)
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
INSERT INTO op.journey_cleaning (
    SELECT
        aj.id,
        COALESCE(public.st_isempty(aj.geom) OR NOT COALESCE(public.st_isvalid(aj.geom), TRUE), FALSE) AS invalid_geom,
        COALESCE(public.st_geometrytype(aj.geom) <> 'ST_LineString', FALSE) AS not_linestring,
        COALESCE(aj.id IN (SELECT line_id FROM long_segments), FALSE)  AS gps_error,
        FALSE AS ferry,
        COALESCE(aj.distance <= 1.60934 OR aj.duration <= 3 / 60, FALSE) AS short,
        COALESCE(aj.avg_speed >= 60 * 1.60934, FALSE) AS fast,
        COALESCE(aj.avg_speed <= 3 * 1.60934, FALSE) AS slow,
        COALESCE(aj.splat_count >= 500, FALSE) AS high_count
    FROM
        op.all_journeys aj
    LEFT JOIN filtered_lines f ON f.id = aj.id
);


