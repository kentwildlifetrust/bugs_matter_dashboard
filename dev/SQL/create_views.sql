DROP VIEW op.journey_cleaning_summary;
CREATE OR REPLACE VIEW op.journey_cleaning_summary AS (
    SELECT 
        jc.invalid_geom::INT AS invalid_geom,
        jc.not_linestring::INT AS not_linestring,
        jc.gps_error::INT AS gps_error,
        jc.ferry::INT AS ferry,
        jc.short::INT AS short,
        jc.fast::INT AS fast,
        jc.slow::INT AS slow,
        jc.high_count::INT AS high_count,
        COUNT(*) AS n_journeys
    FROM op.journey_cleaning jc
    GROUP BY jc.invalid_geom,
        jc.not_linestring,
        jc.gps_error,
        jc.ferry,
        jc.short,
        jc.fast,
        jc.slow,
        jc.high_count
);