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

select count(*) from bugs_matter.journeys5 j WHERE j.end < '2021-01-01'

WITH daily_counts AS (
  SELECT
    j.end::DATE AS date,
    COUNT(*) AS daily_count
  FROM bugs_matter.journeys_app j
  GROUP BY j.end::DATE
), date_bounds AS (
  SELECT
    '2021-05-01'::DATE AS min_date,
    '2024-12-31'::DATE AS max_date
),  -- No need to select FROM the table here
all_dates AS (
  SELECT generate_series(min_date, max_date, interval '1 day')::date AS date
  FROM date_bounds
)
SELECT
  all_dates.date,
  COALESCE(daily_counts.daily_count, 0) AS daily_count,
  SUM(COALESCE(daily_counts.daily_count, 0)) OVER (
    PARTITION BY EXTRACT(YEAR FROM all_date.date)
    ORDER BY all_dates.date
    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
  ) AS cumulative_count
FROM all_dates
LEFT JOIN daily_counts ON all_dates.date = daily_counts.date
ORDER BY all_dates.date;