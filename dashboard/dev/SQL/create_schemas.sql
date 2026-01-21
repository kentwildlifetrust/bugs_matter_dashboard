-- Active: 1747166414686@@kwt-postgresql-azdb-1.postgres.database.azure.com@5432@bugs_matter

drop schema journeys cascade;
drop schema op cascade;
create schema journeys;
create schema ref;

ALTER SCHEMA "journeys" OWNER TO "DevGroup";
ALTER SCHEMA "ref" OWNER TO "DevGroup";

