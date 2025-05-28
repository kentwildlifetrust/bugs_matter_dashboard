-- Active: 1746635410704@@kwt-postgresql-azdb-1.postgres.database.azure.com@5432@bugs_matter

INSERT INTO op.accepted_journeys (id, mean_elevation);

SELECT
    jc.id,
    AVG(ST_Value(e.rast, ST_Intersection(j.geom, e.rast)))
FROM op.journey_check jc
JOIN op.journeys j ON j.id = jc.id
CROSS JOIN ref.elevation e
WHERE NOT (
    invalid_geom OR
    not_linestring OR
    gps_error OR
    ferry OR
    short OR
    fast OR
    slow OR
    high_count
)
GROUP BY jc.id
LIMIT 1000;
