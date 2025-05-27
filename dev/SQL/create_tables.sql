-- Active: 1746105028947@@kwt-postgresql-azdb-1.postgres.database.azure.com@5432@bugs_matter

CREATE TABLE op.journeys
(
  id                               INTEGER                  NOT NULL UNIQUE,
  user_id                          INTEGER                  NOT NULL,
  vehicle_id                       INTEGER                  NOT NULL,
  start_timestamp                  TIMESTAMP WITH TIME ZONE,
  end_timestamp                    TIMESTAMP WITH TIME ZONE,
  rain                             BOOLEAN                 ,
  distance                         NUMERIC                 ,
  duration                         NUMERIC                 ,
  avg_speed                        NUMERIC                 ,
  splat_count                      INTEGER                 ,
  photo_url                        VARCHAR                 ,
  speed                            NUMERIC                 ,
  bearing                          NUMERIC                 ,
  accuracy                         NUMERIC                 ,
  vehicle_reg                      VARCHAR(12)             ,
  vehicle_class                    VARCHAR                 ,
  vehicle_bhp                      NUMERIC                 ,
  vehicle_year_of_manufacture      INTEGER                 ,
  vehicle_date_first_registered    DATE                    ,
  vehicle_date_first_registered_uk DATE                    ,
  vehicle_make                     VARCHAR                 ,
  vehicle_make_code                VARCHAR                 ,
  vehicle_range                    VARCHAR                 ,
  vehicle_model                    VARCHAR                 ,
  vehicle_model_code               VARCHAR                 ,
  vehicle_trim                     VARCHAR                 ,
  vehicle_width                    NUMERIC                 ,
  vehicle_height                   NUMERIC                 ,
  vehicle_car_length               NUMERIC                 ,
  vehicle_colour                   VARCHAR                 ,
  vehicle_num_axles                INTEGER                 ,
  vehicle_num_doors                INTEGER                 ,
  vehicle_num_seats                INTEGER                 ,
  vehicle_body_shape               VARCHAR                 ,
  vehicle_wheel_base               NUMERIC                 ,
  vehicle_wheel_plan               VARCHAR                 ,
  vehicle_kerb_height              NUMERIC                 ,
  vehicle_load_length              NUMERIC                 ,
  vehicle_rigid_artic              VARCHAR                 ,
  vehicle_driving_axle             VARCHAR                 ,
  vehicle_door_plan_literal        VARCHAR                 ,
  vehicle_payload_volume           NUMERIC                 ,
  vehicle_payload_weight           NUMERIC                 ,
  vehicle_unladen_weight           NUMERIC                 ,
  vehicle_gross_train_weight       NUMERIC                 ,
  vehicle_gross_vehicle_weight     NUMERIC                 ,
  vehicle_gross_combined_weight    NUMERIC                 ,
  app_timestamp                    TIMESTAMP WITH TIME ZONE,
  database_timestamp               TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  geom                             GEOMETRY                 NOT NULL,
  id                               INTEGER                  NOT NULL,
  PRIMARY KEY (id)
);

COMMENT ON TABLE op.journeys IS 'Raw bug splat sampling journeys downloaded from Coreo API';

COMMENT ON COLUMN op.journeys.id IS 'Unique journey record Id';

COMMENT ON COLUMN op.journeys.user_id IS 'User account record Id';

COMMENT ON COLUMN op.journeys.vehicle_id IS 'Unique vehicle Id';

COMMENT ON COLUMN op.journeys.start_timestamp IS 'When the journey started';

COMMENT ON COLUMN op.journeys.end_timestamp IS 'When the journey ended';

COMMENT ON COLUMN op.journeys.rain IS 'Did the user report that there was rain during their journey';

COMMENT ON COLUMN op.journeys.distance IS 'Distance travelled in km';

COMMENT ON COLUMN op.journeys.duration IS 'Journey time in hours';

COMMENT ON COLUMN op.journeys.avg_speed IS 'Journey speed in km/h';

COMMENT ON COLUMN op.journeys.splat_count IS 'The number of bug splats on the number plate at the end of the journey';

COMMENT ON COLUMN op.journeys.photo_url IS 'Number plate photo url';

COMMENT ON COLUMN op.journeys.speed IS 'Average speed travelled?';

COMMENT ON COLUMN op.journeys.bearing IS 'Journey bearing?';

COMMENT ON COLUMN op.journeys.accuracy IS 'GPS accuracy?';

COMMENT ON COLUMN op.journeys.vehicle_reg IS 'Vehicle registration number';

COMMENT ON COLUMN op.journeys.vehicle_class IS 'Type of vehicle (car, HGV etc.)';

COMMENT ON COLUMN op.journeys.vehicle_bhp IS 'Vehicle engine break horsepower';

COMMENT ON COLUMN op.journeys.vehicle_year_of_manufacture IS 'Vehicle year of manufacture';

COMMENT ON COLUMN op.journeys.vehicle_date_first_registered IS 'Date first registered';

COMMENT ON COLUMN op.journeys.vehicle_date_first_registered_uk IS 'Date first registered in the UK';

COMMENT ON COLUMN op.journeys.vehicle_make IS 'Vehicle make';

COMMENT ON COLUMN op.journeys.vehicle_make_code IS 'Vehicle make code';

COMMENT ON COLUMN op.journeys.vehicle_range IS 'Vehicle range';

COMMENT ON COLUMN op.journeys.vehicle_model IS 'Vehicle model';

COMMENT ON COLUMN op.journeys.vehicle_model_code IS 'Vehicle model code';

COMMENT ON COLUMN op.journeys.vehicle_trim IS 'Vehicle trim';

COMMENT ON COLUMN op.journeys.vehicle_width IS 'Width of vehicle';

COMMENT ON COLUMN op.journeys.vehicle_height IS 'Height of vehicle';

COMMENT ON COLUMN op.journeys.vehicle_car_length IS 'Vehicle car length';

COMMENT ON COLUMN op.journeys.vehicle_colour IS 'Vehicle colour';

COMMENT ON COLUMN op.journeys.vehicle_num_axles IS 'Vehicle number of axles';

COMMENT ON COLUMN op.journeys.vehicle_num_doors IS 'Vehicle number of doors';

COMMENT ON COLUMN op.journeys.vehicle_num_seats IS 'Vehicle number of seats';

COMMENT ON COLUMN op.journeys.vehicle_body_shape IS 'Vehicle body shape';

COMMENT ON COLUMN op.journeys.vehicle_wheel_base IS 'Vehicle wheel base';

COMMENT ON COLUMN op.journeys.vehicle_wheel_plan IS 'Vehicle wheel plan';

COMMENT ON COLUMN op.journeys.vehicle_kerb_height IS 'Vehicle kerb height';

COMMENT ON COLUMN op.journeys.vehicle_load_length IS 'Vehicle load length';

COMMENT ON COLUMN op.journeys.vehicle_rigid_artic IS 'Vehicle rigid artic';

COMMENT ON COLUMN op.journeys.vehicle_driving_axle IS 'Vehicle driving axle (FWD etc)';

COMMENT ON COLUMN op.journeys.vehicle_door_plan_literal IS 'Vehicle door plan literal';

COMMENT ON COLUMN op.journeys.vehicle_payload_volume IS 'Vehicle payload volume';

COMMENT ON COLUMN op.journeys.vehicle_payload_weight IS 'Vehicle payload weight';

COMMENT ON COLUMN op.journeys.vehicle_unladen_weight IS 'Vehicle unladed weight';

COMMENT ON COLUMN op.journeys.vehicle_gross_train_weight IS 'Vehicle gross train weight';

COMMENT ON COLUMN op.journeys.vehicle_gross_vehicle_weight IS 'Vehicle gross weight';

COMMENT ON COLUMN op.journeys.vehicle_gross_combined_weight IS 'Vehicle gross combined weight';

COMMENT ON COLUMN op.journeys.app_timestamp IS 'Time journey uploaded from app';

COMMENT ON COLUMN op.journeys.geom IS 'Journey line path';

COMMENT ON COLUMN op.journeys.id IS 'Unique journey record Id';

CREATE TABLE op.accepted_journeys
(
  id                  INTEGER                  NOT NULL,
  cm_km_offset        NUMERIC                  NOT NULL,
  log_cm_miles_offset NUMERIC                  NOT NULL,
  midpoint_time       TIMESTAMP WITH TIME ZONE NOT NULL,
  elevation           NUMERIC                  NOT NULL,
  day_of_year         INTEGER                  NOT NULL,
  temp                NUMERIC                  NOT NULL,
  forest              NUMERIC                  NOT NULL,
  shrubland           NUMERIC                  NOT NULL,
  grassland           NUMERIC                  NOT NULL,
  wetland             NUMERIC                  NOT NULL,
  marine              NUMERIC                  NOT NULL,
  arable              NUMERIC                  NOT NULL,
  pasture             NUMERIC                  NOT NULL,
  urban               NUMERIC                  NOT NULL,
  geom                GEOMETRY                 NOT NULL,
  PRIMARY KEY (id)
);

COMMENT ON TABLE op.accepted_journeys IS 'Cleaned sampling journeys to be used in analysis';

COMMENT ON COLUMN op.accepted_journeys.id IS 'Unique journey record Id';

CREATE TABLE op.journey_check
(
  id             INTEGER NOT NULL,
  invalid_geom   BOOLEAN NOT NULL,
  not_linestring BOOLEAN NOT NULL,
  gps_error      BOOLEAN NOT NULL,
  ferry          BOOLEAN NOT NULL,
  short          BOOLEAN NOT NULL,
  fast           BOOLEAN NOT NULL,
  slow           BOOLEAN NOT NULL,
  high_count     BOOLEAN NOT NULL,
  PRIMARY KEY (id)
);

COMMENT ON TABLE op.journey_check IS 'Journeys are checked against multiple criteria to determine if they should be included in analysis';

COMMENT ON COLUMN op.journey_check.id IS 'Unique journey record Id';

COMMENT ON COLUMN op.journey_check.invalid_geom IS 'Geometry must be non-empty and valid';

COMMENT ON COLUMN op.journey_check.not_linestring IS 'All points are excluded';

COMMENT ON COLUMN op.journey_check.gps_error IS 'Linestrings with segments > 1000m are assumed to be GPS errors';

COMMENT ON COLUMN op.journey_check.ferry IS 'Journeys passing < 50m from a car ferry route are excluded';

COMMENT ON COLUMN op.journey_check.short IS 'Journeys with a distance < 1 mile or duration < 0.05 hr are excluded';

COMMENT ON COLUMN op.journey_check.fast IS 'Journeys with speed >= 60 mph are excluded';

COMMENT ON COLUMN op.journey_check.slow IS 'Journeys with speed <= 3 mph are excluded';

COMMENT ON COLUMN op.journey_check.high_count IS 'Journeys with improbable splat counts (>= 500) are excluded';

CREATE TABLE op.journey_regions
(
  journey_id INTEGER NOT NULL,
  region_id  INTEGER NOT NULL,
  PRIMARY KEY (journey_id, region_id)
);

COMMENT ON COLUMN op.journey_regions.journey_id IS 'Unique journey record Id';

CREATE TABLE op.journey_sub_regions
(
  journey_id    INTEGER NOT NULL,
  sub_region_id INTEGER NOT NULL,
  PRIMARY KEY (journey_id, sub_region_id)
);

COMMENT ON COLUMN op.journey_sub_regions.journey_id IS 'Unique journey record Id';

CREATE TABLE op.users
(
  id           INTEGER NOT NULL UNIQUE,
  username     VARCHAR NOT NULL,
  sign_up_date DATE    NOT NULL,
  PRIMARY KEY (id)
);

COMMENT ON TABLE op.users IS 'User accounts';

COMMENT ON COLUMN op.users.id IS 'Unique user account record Id';

COMMENT ON COLUMN op.users.username IS 'Username';

COMMENT ON COLUMN op.users.sign_up_date IS 'Date registered';

CREATE TABLE op.users_queried_dates
(
  date DATE NOT NULL UNIQUE,
  PRIMARY KEY (date)
);

COMMENT ON TABLE op.users_queried_dates IS 'Keeps track of the days that have been checked for user sign ups';

CREATE TABLE ref.countries
(
  code CHAR(3)  NOT NULL,
  name VARCHAR  NOT NULL,
  geom GEOMETRY NOT NULL,
  PRIMARY KEY (code)
);

COMMENT ON TABLE ref.countries IS 'ISO 3166-1 countries';

CREATE TABLE ref.regions
(
  id           INTEGER  NOT NULL,
  country_code CHAR(3)  NOT NULL,
  name         VARCHAR  NOT NULL,
  geom         GEOMETRY NOT NULL,
  PRIMARY KEY (id)
);

COMMENT ON TABLE ref.regions IS 'ISO 3166-2 country subdivisions (UK nations, regions, provinces etc.).';

CREATE TABLE ref.sub_regions
(
  id        INTEGER  NOT NULL,
  area_name VARCHAR  NOT NULL,
  geom      GEOMETRY NOT NULL,
  PRIMARY KEY (id)
);

COMMENT ON TABLE ref.sub_regions IS 'Larger regions are split to another level. English regions for example';

ALTER TABLE op.journeys
  ADD CONSTRAINT FK_op_users_TO_op_all_journeys
    FOREIGN KEY (user_id)
    REFERENCES op.users (id);

ALTER TABLE ref.regions
  ADD CONSTRAINT FK_ref_countries_TO_ref_regions
    FOREIGN KEY (country_code)
    REFERENCES ref.countries (code);

ALTER TABLE op.accepted_journeys
  ADD CONSTRAINT FK_op_all_journeys_TO_op_cleaned_journeys
    FOREIGN KEY (id)
    REFERENCES op.journeys (id);


ALTER TABLE op.journey_check
  ADD CONSTRAINT FK_op_all_journeys_TO_op_journey_cleaning
    FOREIGN KEY (id)
    REFERENCES op.journeys (id);

ALTER TABLE op.journeys
  ADD CONSTRAINT FK_op_all_journeys_TO_op_all_journeys
    FOREIGN KEY (id)
    REFERENCES op.journeys (id);

ALTER TABLE op.journey_regions
  ADD CONSTRAINT FK_op_all_journeys_TO_op_journey_regions
    FOREIGN KEY (journey_id)
    REFERENCES op.journeys (id);

ALTER TABLE op.journey_regions
  ADD CONSTRAINT FK_ref_regions_TO_op_journey_regions
    FOREIGN KEY (region_id)
    REFERENCES ref.regions (id);

ALTER TABLE op.journey_sub_regions
  ADD CONSTRAINT FK_op_all_journeys_TO_op_journey_sub_regions
    FOREIGN KEY (journey_id)
    REFERENCES op.journeys (id);

ALTER TABLE op.journey_sub_regions
  ADD CONSTRAINT FK_ref_sub_regions_TO_op_journey_sub_regions
    FOREIGN KEY (sub_region_id)
    REFERENCES ref.sub_regions (id);

CREATE TABLE op.journey_countries
(
  journey_id   INTEGER NOT NULL,
  country_code CHAR(3) NOT NULL,
  PRIMARY KEY (journey_id, country_code)
);

COMMENT ON COLUMN op.journey_countries.journey_id IS 'Unique journey record Id';


ALTER TABLE op.journey_countries
  ADD CONSTRAINT FK_op_all_journeys_TO_op_journey_countries
    FOREIGN KEY (journey_id)
    REFERENCES op.journeys (id);

ALTER TABLE op.journey_countries
  ADD CONSTRAINT FK_ref_countries_TO_op_journey_countries
    FOREIGN KEY (country_code)
    REFERENCES ref.countries (code);

ALTER TABLE ref.sub_regions
ADD CONSTRAINT FK_ref_regions_TO_ref_sub_regions1
FOREIGN KEY (region_id)
REFERENCES ref.regions (id);