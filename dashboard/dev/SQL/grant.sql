GRANT usage ON SCHEMA journeys TO "BugsMatterReadOnly";
GRANT SELECT ON journeys.processed TO "BugsMatterReadOnly";

GRANT usage ON SCHEMA ref TO "BugsMatterReadOnly";

GRANT SELECT ON ref.regions TO "BugsMatterReadOnly";

GRANT usage ON SCHEMA analysis TO "BugsMatterReadOnly";
GRANT select ON ALL TABLES IN SCHEMA analysis TO "BugsMatterReadOnly";

grant usage, create on SCHEMA sign_ups TO bugsmatteretl;
ALTER DEFAULT PRIVILEGES FOR ROLE "DevGroup" IN SCHEMA sign_ups
GRANT ALL ON TABLES TO bugsmatteretl;
GRANT ALL ON ALL TABLES in schema sign_ups to bugsmatteretl;
