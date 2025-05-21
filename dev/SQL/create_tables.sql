
CREATE TABLE op.processed_journeys
(
  id                  INTEGER                  NOT NULL,
  region_code         CHAR(6)                  NOT NULL,
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
  PRIMARY KEY (id)
);

COMMENT ON TABLE op.processed_journeys IS 'Cleaned sampling journeys to be used in analysis';

COMMENT ON COLUMN op.processed_journeys.id IS 'Unique journey record Id';

CREATE TABLE op.raw_journeys
(
  id                 INTEGER                  NOT NULL UNIQUE,
  user_id            INTEGER                  NOT NULL,
  vehicle_id         INTEGER                  NOT NULL,
  start_timestamp    TIMESTAMP WITH TIME ZONE NOT NULL,
  end_timestamp      TIMESTAMP WITH TIME ZONE NOT NULL,
  rain               BOOLEAN                 ,
  time               INTEGER                 ,
  count              INTEGER                  NOT NULL,
  photo              VARCHAR                 ,
  distance           NUMERIC                  NOT NULL,
  speed              NUMERIC                 ,
  bearing            NUMERIC                 ,
  accuracy           NUMERIC                 ,
  app_timestamp      TIMESTAMP WITH TIME ZONE,
  database_timestamp TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  exclude            BOOLEAN                  NOT NULL,
  exclude_reason     VARCHAR                 ,
  geom               GEOMETRY                 NOT NULL,
  PRIMARY KEY (id)
);

COMMENT ON TABLE op.raw_journeys IS 'Raw bug splat sampling journeys downloaded from Coreo API';

COMMENT ON COLUMN op.raw_journeys.id IS 'Unique journey record Id';

COMMENT ON COLUMN op.raw_journeys.user_id IS 'User account record Id';

COMMENT ON COLUMN op.raw_journeys.vehicle_id IS 'Unique vehicle Id';

COMMENT ON COLUMN op.raw_journeys.start_timestamp IS 'When the journey started';

COMMENT ON COLUMN op.raw_journeys.end_timestamp IS 'When the journey ended';

COMMENT ON COLUMN op.raw_journeys.rain IS 'Did the user report that there was rain during their journey';

COMMENT ON COLUMN op.raw_journeys.time IS 'Journey duration in seconds';

COMMENT ON COLUMN op.raw_journeys.count IS 'The number of bug splats on the number plate at the end of the journey';

COMMENT ON COLUMN op.raw_journeys.photo IS 'Number plate photo url';

COMMENT ON COLUMN op.raw_journeys.distance IS 'Distance travelled in km';

COMMENT ON COLUMN op.raw_journeys.speed IS 'Average speed travelled?';

COMMENT ON COLUMN op.raw_journeys.bearing IS 'Journey bearing?';

COMMENT ON COLUMN op.raw_journeys.accuracy IS 'GPS accuracy?';

COMMENT ON COLUMN op.raw_journeys.app_timestamp IS 'Time journey uploaded from app';

COMMENT ON COLUMN op.raw_journeys.database_timestamp IS 'Time added to this table';

COMMENT ON COLUMN op.raw_journeys.exclude IS 'Has the journey been excluded from the analysis during data cleaning';

COMMENT ON COLUMN op.raw_journeys.exclude_reason IS 'Why has the journey been excluded from the analysis';

COMMENT ON COLUMN op.raw_journeys.geom IS 'Journey line path';

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

CREATE TABLE op.vehicles
(
id     SERIAL NOT NULL,
  reg                      VARCHAR(12),
  class                    VARCHAR    ,
  bhp                      NUMERIC    ,
  year_of_manufacture      INTEGER    ,
  date_first_registered    DATE       ,
  date_first_registered_uk DATE       ,
  make                     VARCHAR    ,
  make_code                VARCHAR    ,
  range                    VARCHAR    ,
  model                    VARCHAR    ,
  model_code               VARCHAR    ,
  trim                     VARCHAR    ,
  width                    NUMERIC    ,
  height                   NUMERIC    ,
  car_length               NUMERIC    ,
  colour                   VARCHAR    ,
  num_axles                INTEGER    ,
  num_doors                INTEGER    ,
  num_seats                INTEGER    ,
  body_shape               VARCHAR    ,
  wheel_base               NUMERIC    ,
  wheel_plan               VARCHAR    ,
  kerb_height              NUMERIC    ,
  load_length              NUMERIC    ,
  rigid_artic              VARCHAR    ,
  driving_axle             VARCHAR    ,
  door_plan_literal        VARCHAR    ,
  payload_volume           NUMERIC    ,
  payload_weight           NUMERIC    ,
  unladen_weight           NUMERIC    ,
  gross_train_weight       NUMERIC    ,
  gross_vehicle_weight     NUMERIC    ,
  gross_combined_weight    NUMERIC    ,
  PRIMARY KEY (id)
);

COMMENT ON TABLE op.vehicles IS 'Vehicles used for sampling journeys';

COMMENT ON COLUMN op.vehicles.id IS 'Unique vehicle Id';

COMMENT ON COLUMN op.vehicles.reg IS 'Vehicle registration number';

COMMENT ON COLUMN op.vehicles.class IS 'Type of vehicle (car, HGV etc.)';

COMMENT ON COLUMN op.vehicles.bhp IS 'Vehicle engine break horsepower';

COMMENT ON COLUMN op.vehicles.year_of_manufacture IS 'Vehicle year of manufacture';

COMMENT ON COLUMN op.vehicles.date_first_registered IS 'Date first registered';

COMMENT ON COLUMN op.vehicles.date_first_registered_uk IS 'Date first registered in the UK';

COMMENT ON COLUMN op.vehicles.make IS 'Vehicle make';

COMMENT ON COLUMN op.vehicles.make_code IS 'Vehicle make code';

COMMENT ON COLUMN op.vehicles.range IS 'Vehicle range';

COMMENT ON COLUMN op.vehicles.model IS 'Vehicle model';

COMMENT ON COLUMN op.vehicles.model_code IS 'Vehicle model code';

COMMENT ON COLUMN op.vehicles.trim IS 'Vehicle trim';

COMMENT ON COLUMN op.vehicles.width IS 'Width of vehicle';

COMMENT ON COLUMN op.vehicles.height IS 'Height of vehicle';

COMMENT ON COLUMN op.vehicles.car_length IS 'Vehicle car length';

COMMENT ON COLUMN op.vehicles.colour IS 'Vehicle colour';

COMMENT ON COLUMN op.vehicles.num_axles IS 'Vehicle number of axles';

COMMENT ON COLUMN op.vehicles.num_doors IS 'Vehicle number of doors';

COMMENT ON COLUMN op.vehicles.num_seats IS 'Vehicle number of seats';

COMMENT ON COLUMN op.vehicles.body_shape IS 'Vehicle body shape';

COMMENT ON COLUMN op.vehicles.wheel_base IS 'Vehicle wheel base';

COMMENT ON COLUMN op.vehicles.wheel_plan IS 'Vehicle wheel plan';

COMMENT ON COLUMN op.vehicles.kerb_height IS 'Vehicle kerb height';

COMMENT ON COLUMN op.vehicles.load_length IS 'Vehicle load length';

COMMENT ON COLUMN op.vehicles.rigid_artic IS 'Vehicle rigid artic';

COMMENT ON COLUMN op.vehicles.driving_axle IS 'Vehicle driving axle (FWD etc)';

COMMENT ON COLUMN op.vehicles.door_plan_literal IS 'Vehicle door plan literal';

COMMENT ON COLUMN op.vehicles.payload_volume IS 'Vehicle payload volume';

COMMENT ON COLUMN op.vehicles.payload_weight IS 'Vehicle payload weight';

COMMENT ON COLUMN op.vehicles.unladen_weight IS 'Vehicle unladed weight';

COMMENT ON COLUMN op.vehicles.gross_train_weight IS 'Vehicle gross train weight';

COMMENT ON COLUMN op.vehicles.gross_vehicle_weight IS 'Vehicle gross weight';

COMMENT ON COLUMN op.vehicles.gross_combined_weight IS 'Vehicle gross combined weight';

CREATE TABLE ref.countries
(
  code CHAR(3)  NOT NULL,
  name VARCHAR  NOT NULL,
  geom GEOMETRY NOT NULL,
  PRIMARY KEY (code)
);

COMMENT ON TABLE ref.countries IS 'ISO 3166-1 countries';

CREATE TABLE ref.country_subdivisions_1
(
  code         CHAR(6)  NOT NULL,
  country_code CHAR(3)  NOT NULL,
  name         VARCHAR  NOT NULL,
  geom         GEOMETRY NOT NULL,
  PRIMARY KEY (code)
);

COMMENT ON TABLE ref.country_subdivisions_1 IS 'ISO 3166-2 country subdivisions (UK nations, regions, provinces etc.).';

CREATE TABLE ref.country_subdivisions_2
(
  subdivision_code CHAR(6) NOT NULL,
  area_name        VARCHAR NOT NULL,
  PRIMARY KEY (subdivision_code, area_name)
);

COMMENT ON TABLE ref.country_subdivisions_2 IS 'Larger country_subdivisions are further split. English regions for example';

ALTER TABLE op.raw_journeys
  ADD CONSTRAINT FK_op_users_TO_op_raw_journeys
    FOREIGN KEY (user_id)
    REFERENCES op.users (id);

ALTER TABLE op.raw_journeys
  ADD CONSTRAINT FK_op_vehicles_TO_op_raw_journeys
    FOREIGN KEY (vehicle_id)
    REFERENCES op.vehicles (id);

ALTER TABLE ref.country_subdivisions_1
  ADD CONSTRAINT FK_ref_countries_TO_ref_country_subdivisions_1
    FOREIGN KEY (country_code)
    REFERENCES ref.countries (code);

ALTER TABLE op.processed_journeys
  ADD CONSTRAINT FK_ref_country_subdivisions_1_TO_op_processed_journeys
    FOREIGN KEY (region_code)
    REFERENCES ref.country_subdivisions_1 (code);

ALTER TABLE op.processed_journeys
  ADD CONSTRAINT FK_op_raw_journeys_TO_op_processed_journeys
    FOREIGN KEY (id)
    REFERENCES op.raw_journeys (id);

ALTER TABLE ref.country_subdivisions_2
  ADD CONSTRAINT FK_ref_country_subdivisions_1_TO_ref_country_subdivisions_2
    FOREIGN KEY (subdivision_code)
    REFERENCES ref.country_subdivisions_1 (code);

