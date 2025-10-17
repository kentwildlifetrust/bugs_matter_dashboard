const express = require('express');
//const MBTiles = require('@mapbox/mbtiles');
const cors = require('cors');
const { Pool } = require('pg');
if (process.env.NODE_ENV !== 'production') {
  require('dotenv').config({ path: '.env' });
}


const app = express();
const port = process.env.PORT || 5000;

pool = new Pool({
  // Add your database configuration here
  user: process.env.DB_USER,
  host: process.env.DB_HOST,
  database: process.env.DB_DATABASE,
  password: process.env.DB_PASSWORD,
  port: process.env.DB_PORT,
  ssl: {
      rejectUnauthorized: false,
  },
});

app.use(cors({
  origin: process.env.FRONTEND_URL || 'http://127.0.0.1:3838',
  methods: ['GET']       // Specify allowed HTTP methods
}));

// world all years
app.get('/tiles/world/:z/:x/:y.pbf', async (req, res) => {
  const { z, x, y } = req.params;
  // Build the SQL query with parameter substitution (using template literals or parameterized queries)
  const sql = `
    WITH
      -- Compute tile envelope once and transform it to EPSG:4326
      bounds AS (
        SELECT public.ST_Transform(public.ST_TileEnvelope($1, $2, $3), 4326) AS geom
      ),
      mvt_data AS (
        SELECT
          public.ST_AsMVTGeom(
            public.ST_Simplify(
              public.ST_Transform(r.geom, 3857), 
              CASE 
                WHEN $1 <= 3 THEN 4000  
                WHEN $1 <= 4 THEN 2000 
                WHEN $1 <= 8 THEN 500
                WHEN $1 <= 10 THEN 100 
                WHEN $1 <= 12 THEN 20 
                ELSE 5               
              END
            ), 
            public.ST_TileEnvelope($1, $2, $3),
            4096, 64, true
          ) AS geom,
          r.id
        FROM journeys.processed r, bounds
        -- Now use the pre-transformed bounds (in 4326) for intersection test
        WHERE public.ST_Intersects(r.geom, bounds.geom)
      )
    SELECT public.ST_AsMVT(mvt_data.*, 'lines', 4096, 'geom') AS tile
    FROM mvt_data;
  `;

  try {
    const client = await pool.connect();
    const result = await client.query(sql, [z, x, y]);
    client.release();

    if (result.rows.length > 0 && result.rows[0].tile) {
      res.setHeader('Content-Type', 'application/x-protobuf');
      res.send(result.rows[0].tile);
    } else {
      res.status(204).send(); // No content if nothing matches
    }
  } catch (err) {
    console.error(err);
    res.status(500).send("Tile generation error");
  }
});

// country all years
app.get('/tiles/countries/:country/:z/:x/:y.pbf', async (req, res) => {
  const { country, z, x, y } = req.params;
  // Build the SQL query with parameter substitution (using template literals or parameterized queries)
  const sql = `
    WITH
      bounds AS (
        SELECT public.ST_Transform(public.ST_TileEnvelope($1, $2, $3), 4326) AS geom
      ),
      mvt_data AS (
        SELECT
          public.ST_AsMVTGeom(
            public.ST_Simplify(
              public.ST_Transform(r.geom, 3857), 
              CASE 
                WHEN $1 <= 3 THEN 4000  
                WHEN $1 <= 4 THEN 2000 
                WHEN $1 <= 8 THEN 500
                WHEN $1 <= 10 THEN 100 
                WHEN $1 <= 12 THEN 20 
                ELSE 5               
              END
            ), 
            public.ST_TileEnvelope($1, $2, $3),
            4096, 64, true
          ) AS geom,
          r.id
        FROM journeys.processed r, bounds
        WHERE public.ST_Intersects(r.geom, bounds.geom)
        AND country_code=$4
      )
    SELECT public.ST_AsMVT(mvt_data.*, 'lines', 4096, 'geom') AS tile
    FROM mvt_data;
  `;

  try {
    const client = await pool.connect();
    const result = await client.query(sql, [z, x, y, country]);
    client.release();

    if (result.rows.length > 0 && result.rows[0].tile) {
      res.setHeader('Content-Type', 'application/x-protobuf');
      res.send(result.rows[0].tile);
    } else {
      res.status(204).send(); // No content if nothing matches
    }
  } catch (err) {
    console.error(err);
    res.status(500).send("Tile generation error");
  }
});

// world by year
app.get('/tiles/world/years/:year/:z/:x/:y.pbf', async (req, res) => {
  const { year, z, x, y } = req.params;
  // Build the SQL query with parameter substitution (using template literals or parameterized queries)
  const sql = `
    WITH
      -- Compute tile envelope once and transform it to EPSG:4326
      bounds AS (
        SELECT public.ST_Transform(public.ST_TileEnvelope($1, $2, $3), 4326) AS geom
      ),
      mvt_data AS (
        SELECT
          public.ST_AsMVTGeom(
            public.ST_Simplify(
              public.ST_Transform(r.geom, 3857), 
              CASE 
                WHEN $1 <= 3 THEN 4000  
                WHEN $1 <= 4 THEN 2000 
                WHEN $1 <= 8 THEN 500
                WHEN $1 <= 10 THEN 100 
                WHEN $1 <= 12 THEN 20 
                ELSE 5               
              END
            ), 
            public.ST_TileEnvelope($1, $2, $3),
            4096, 64, true
          ) AS geom,
          r.id
        FROM journeys.processed r, bounds
        -- Now use the pre-transformed bounds (in 4326) for intersection test
        WHERE public.ST_Intersects(r.geom, bounds.geom)
        AND r.year = $4
      )
    SELECT public.ST_AsMVT(mvt_data.*, 'lines', 4096, 'geom') AS tile
    FROM mvt_data;
  `;

  try {
    const client = await pool.connect();
    const result = await client.query(sql, [z, x, y, year]);
    client.release();

    if (result.rows.length > 0 && result.rows[0].tile) {
      res.setHeader('Content-Type', 'application/x-protobuf');
      res.send(result.rows[0].tile);
    } else {
      res.status(204).send(); // No content if nothing matches
    }
  } catch (err) {
    console.error(err);
    res.status(500).send("Tile generation error");
  }
});


// UK by year
app.get('/tiles/countries/:country/years/:year/:z/:x/:y.pbf', async (req, res) => {
  const { country, year, z, x, y } = req.params;
  // Build the SQL query with parameter substitution (using template literals or parameterized queries)
  const sql = `
    WITH
      bounds AS (
        SELECT public.ST_Transform(public.ST_TileEnvelope($1, $2, $3), 4326) AS geom
      ),
      mvt_data AS (
        SELECT
          public.ST_AsMVTGeom(
            public.ST_Simplify(
              public.ST_Transform(r.geom, 3857), 
              CASE 
                WHEN $1 <= 3 THEN 4000  
                WHEN $1 <= 4 THEN 2000 
                WHEN $1 <= 8 THEN 500
                WHEN $1 <= 10 THEN 100 
                WHEN $1 <= 12 THEN 20 
                ELSE 5               
              END
            ),
            public.ST_TileEnvelope($1, $2, $3),
            4096, 64, true
          ) AS geom,
          r.id
        FROM journeys.processed r, bounds
        WHERE public.ST_Intersects(r.geom, bounds.geom)
        AND r.year = $4
        AND country_code=$5
      )
    SELECT public.ST_AsMVT(mvt_data.*, 'lines', 4096, 'geom') AS tile
    FROM mvt_data;
  `;

  try {
    const client = await pool.connect();
    const result = await client.query(sql, [z, x, y, year, country]);
    client.release();

    if (result.rows.length > 0 && result.rows[0].tile) {
      res.setHeader('Content-Type', 'application/x-protobuf');
      res.send(result.rows[0].tile);
    } else {
      res.status(204).send(); // No content if nothing matches
    }
  } catch (err) {
    console.error(err);
    res.status(500).send("Tile generation error");
  }
});

// England all years
app.get('/tiles/regions/:region/:z/:x/:y.pbf', async (req, res) => {
  const { region, z, x, y } = req.params;

  // ChatGPT made this optimised query to generate .pbf tiles dynamically
  const query = `
    WITH
      bounds AS (
        SELECT public.ST_Transform(public.ST_TileEnvelope($1, $2, $3), 4326) AS geom
      ),
      mvt_data AS (
        SELECT
          public.ST_AsMVTGeom(
            public.ST_Simplify(
              public.ST_Transform(r.geom, 3857), 
              CASE 
                WHEN $1 <= 3 THEN 4000  
                WHEN $1 <= 4 THEN 2000 
                WHEN $1 <= 8 THEN 500
                WHEN $1 <= 10 THEN 100 
                WHEN $1 <= 12 THEN 20 
                ELSE 5               
              END
            ),
            public.ST_TileEnvelope($1, $2, $3), 
            4096, 64, true
          ) AS geom,
          r.id
        FROM journeys.processed r, bounds
        WHERE public.ST_Intersects(r.geom, bounds.geom) AND
        r.region_code=$4
      )
    SELECT public.ST_AsMVT(mvt_data.*, 'lines', 4096, 'geom') AS tile
    FROM mvt_data;
  `;

  try {
    const client = await pool.connect();
    const result = await client.query(query, [z, x, y, region]);
    client.release();

    if (result.rows.length > 0 && result.rows[0].tile) {
      res.setHeader('Content-Type', 'application/x-protobuf');
      res.send(result.rows[0].tile);
    } else {
      res.status(204).send(); // No content if nothing matches
    }
  } catch (err) {
    console.error(err);
    res.status(500).send("Tile generation error");
  }
});

// England by year
app.get('/tiles/regions/:region/years/:year/:z/:x/:y.pbf', async (req, res) => {
  const { region, year, z, x, y } = req.params;

  // ChatGPT made this optimised query to generate .pbf tiles dynamically
  const query = `
    WITH
      bounds AS (
        SELECT public.ST_Transform(public.ST_TileEnvelope($1, $2, $3), 4326) AS geom
      ),
      mvt_data AS (
        SELECT
          public.ST_AsMVTGeom(
            public.ST_Simplify(
              public.ST_Transform(r.geom, 3857), 
              CASE 
                WHEN $1 <= 3 THEN 4000  
                WHEN $1 <= 4 THEN 2000 
                WHEN $1 <= 8 THEN 500
                WHEN $1 <= 10 THEN 100 
                WHEN $1 <= 12 THEN 20 
                ELSE 5               
              END
            ),
            public.ST_TileEnvelope($1, $2, $3),
            4096, 64, true
          ) AS geom,
          r.id
        FROM journeys.processed r, bounds
        WHERE public.ST_Intersects(r.geom, bounds.geom) AND
        r.region_code=$4 AND
        r.year = $5
      )
    SELECT public.ST_AsMVT(mvt_data.*, 'lines', 4096, 'geom') AS tile
    FROM mvt_data;
  `;

  try {
    const client = await pool.connect();
    const result = await client.query(query, [z, x, y, region, year]);
    client.release();

    if (result.rows.length > 0 && result.rows[0].tile) {
      res.setHeader('Content-Type', 'application/x-protobuf');
      res.send(result.rows[0].tile);
    } else {
      res.status(204).send(); // No content if nothing matches
    }
  } catch (err) {
    console.error(err);
    res.status(500).send("Tile generation error");
  }
});


app.get('/', (req, res) => {
  res.send('Hello, world!');
});

app.listen(port, () => {
  console.log(`Live tile server listening at http://localhost:${port}`);
});

