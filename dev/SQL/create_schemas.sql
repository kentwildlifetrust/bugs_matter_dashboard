-- Active: 1746707264593@@kwt-postgresql-azdb-1.postgres.database.azure.com@5432@bugs_matter

drop schema ref cascade;
drop schema op cascade;
create schema ref;
create schema op;

ALTER SCHEMA "ref" OWNER TO "DevGroup";
ALTER SCHEMA "op" OWNER TO "DevGroup";