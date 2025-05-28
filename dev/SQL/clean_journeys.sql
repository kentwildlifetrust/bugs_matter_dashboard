DO $$
DECLARE
    batch_size INTEGER := 100;
    processed_count INTEGER;
    total_processed INTEGER := 0;
    start_time TIMESTAMP;
    end_time TIMESTAMP;
    elapsed_time INTERVAL;
BEGIN
	RAISE NOTICE 'batch size: %', batch_size;
	-- Capture the start time
    start_time := clock_timestamp();
    LOOP
        WITH new_journeys AS (
            SELECT
                aj.id,
                aj.distance,
                aj.avg_speed,
                aj.splat_count,
				aj.duration,
                public.st_transform(aj.geom, 27700) AS geom
            FROM op.journeys aj
            WHERE NOT EXISTS (
                SELECT 1 FROM op.journey_check jc WHERE aj.id = jc.id
            )
            LIMIT batch_size
        ),
        filtered_lines AS (
            SELECT
                l.*
            FROM
                new_journeys l
            WHERE
                public.ST_NPoints(l.geom_27700) > 1
                AND public.ST_IsValid(l.geom)
                AND NOT public.ST_IsEmpty(l.geom)
                AND public.ST_GeometryType(l.geom) = 'ST_LineString'
        ),
        segments AS (
            SELECT
                f.id AS line_id,
                public.ST_Length(public.ST_MakeLine(p1.geom, p2.geom)) AS segment_length
            FROM
                filtered_lines f
                CROSS JOIN LATERAL public.ST_DumpPoints(f.geom) p1
                CROSS JOIN LATERAL public.ST_DumpPoints(f.geom) p2
            WHERE
                p1.path[1] = p2.path[1] - 1
                AND public.ST_IsValid(p1.geom)
                AND public.ST_IsValid(p2.geom)
        ),
        long_segments AS (
            SELECT DISTINCT line_id
            FROM segments
            WHERE segment_length > 1000
        )
        INSERT INTO op.journey_check (
            SELECT
                aj.id,
                COALESCE(public.st_isempty(aj.geom) OR NOT COALESCE(public.st_isvalid(aj.geom), TRUE), FALSE) AS invalid_geom,
                COALESCE(public.st_geometrytype(aj.geom) <> 'ST_LineString', FALSE) AS not_linestring,
                COALESCE(aj.id IN (SELECT line_id FROM long_segments), FALSE) AS gps_error,
                COALESCE(EXISTS (
                    SELECT 1
                    FROM ref.vehicle_ferry_routes vf
                    WHERE public.ST_DWithin(
                        aj.geom_27700,
                        vf.geom,
                        50
                    )
                ), FALSE) AS ferry,
                COALESCE(aj.distance <= 1.60934 OR aj.duration <= 3 / 60, FALSE) AS short,
                COALESCE(aj.avg_speed >= 60 * 1.60934, FALSE) AS fast,
                COALESCE(aj.avg_speed <= 3 * 1.60934, FALSE) AS slow,
                COALESCE(aj.splat_count >= 500, FALSE) AS high_count
            FROM
                new_journeys aj
            LEFT JOIN filtered_lines f ON f.id = aj.id
        );

        GET DIAGNOSTICS processed_count = ROW_COUNT;

        total_processed := total_processed + processed_count;
    	RAISE NOTICE 'Total records processed: %', total_processed;

 		-- Capture the end time and calculate elapsed time
        end_time := clock_timestamp();
        elapsed_time := end_time - start_time;

        -- Output the timing to the console
        RAISE NOTICE 'Time taken %', elapsed_time;

        -- Exit if no more records were processed
        IF processed_count = 0 THEN
            EXIT;
        END IF;

        -- Optional: Add a small delay to reduce database load
        PERFORM pg_sleep(0.1);
    END LOOP;

END;
$$
