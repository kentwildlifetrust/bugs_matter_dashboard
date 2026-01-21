GRANT usage ON SCHEMA journeys TO "BugsMatterReadOnly";
GRANT SELECT ON journeys.processed TO "BugsMatterReadOnly";

GRANT usage ON SCHEMA ref TO "BugsMatterReadOnly";

GRANT SELECT ON ref.regions TO "BugsMatterReadOnly";

GRANT usage ON SCHEMA analysis TO "BugsMatterReadOnly";
GRANT select ON ALL TABLES IN SCHEMA analysis TO "BugsMatterReadOnly";
