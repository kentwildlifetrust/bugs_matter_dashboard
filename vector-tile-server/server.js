const express = require('express');
//const MBTiles = require('@mapbox/mbtiles');
const cors = require('cors');
const { Pool } = require('pg');
if (process.env.NODE_ENV !== 'production') {
  require('dotenv').config({ path: '.env' });
}


const app = express();
const port = process.env.PORT || 3000;

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


// UK all years
app.get('/tiles/uk/:z/:x/:y.pbf', async (req, res) => {
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
            public.ST_Transform(r.geom, 3857),  -- transform feature to 3857 for tile encoding
            public.ST_TileEnvelope($1, $2, $3),      -- use the original tile envelope in 3857
            4096, 64, true
          ) AS geom,
          r.id
        FROM bugs_matter.journeys_server r, bounds
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

// UK by year
app.get('/tiles/uk/years/:year/:z/:x/:y.pbf', async (req, res) => {
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
            public.ST_Transform(r.geom, 3857),  -- transform feature to 3857 for tile encoding
            public.ST_TileEnvelope($1, $2, $3),      -- use the original tile envelope in 3857
            4096, 64, true
          ) AS geom,
          r.id
        FROM bugs_matter.journeys_server r, bounds
        -- Now use the pre-transformed bounds (in 4326) for intersection test
        WHERE public.ST_Intersects(r.geom, bounds.geom) AND r.year = $4
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

// England all years
app.get('/tiles/england/:z/:x/:y.pbf', async (req, res) => {
  const { z, x, y } = req.params;

  // ChatGPT made this optimised query to generate .pbf tiles dynamically
  const query = `
    WITH
      -- Compute tile envelope once and transform it to EPSG:4326
      bounds AS (
        SELECT public.ST_Transform(public.ST_TileEnvelope($1, $2, $3), 4326) AS geom
      ),
      mvt_data AS (
        SELECT
          public.ST_AsMVTGeom(
            public.ST_Transform(r.geom, 3857),  -- transform feature to 3857 for tile encoding
            public.ST_TileEnvelope($1, $2, $3),      -- use the original tile envelope in 3857
            4096, 64, true
          ) AS geom,
          r.id
        FROM bugs_matter.journeys_server r, bounds
        -- Now use the pre-transformed bounds (in 4326) for intersection test
        WHERE public.ST_Intersects(r.geom, bounds.geom) AND
        r.region_id IN (4, 6, 1, 2, 8, 9, 5, 3, 7)
      )
    SELECT public.ST_AsMVT(mvt_data.*, 'lines', 4096, 'geom') AS tile
    FROM mvt_data;
  `;

  try {
    const client = await pool.connect();
    const result = await client.query(query, [z, x, y]);
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
app.get('/tiles/england/years/:year/:z/:x/:y.pbf', async (req, res) => {
  const { year, z, x, y } = req.params;

  // ChatGPT made this optimised query to generate .pbf tiles dynamically
  const query = `
    WITH
      -- Compute tile envelope once and transform it to EPSG:4326
      bounds AS (
        SELECT public.ST_Transform(public.ST_TileEnvelope($1, $2, $3), 4326) AS geom
      ),
      mvt_data AS (
        SELECT
          public.ST_AsMVTGeom(
            public.ST_Transform(r.geom, 3857),  -- transform feature to 3857 for tile encoding
            public.ST_TileEnvelope($1, $2, $3),      -- use the original tile envelope in 3857
            4096, 64, true
          ) AS geom,
          r.id
        FROM bugs_matter.journeys_server r, bounds
        -- Now use the pre-transformed bounds (in 4326) for intersection test
        WHERE public.ST_Intersects(r.geom, bounds.geom) AND
        r.region_id IN (4, 6, 1, 2, 8, 9, 5, 3, 7) AND
        r.year = $4
      )
    SELECT public.ST_AsMVT(mvt_data.*, 'lines', 4096, 'geom') AS tile
    FROM mvt_data;
  `;

  try {
    const client = await pool.connect();
    const result = await client.query(query, [z, x, y, year]);
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

// Region all years
app.get('/tiles/regions/:regionId/:z/:x/:y.pbf', async (req, res) => {
  const { regionId, z, x, y } = req.params;

  // ChatGPT made this optimised query to generate .pbf tiles dynamically
  const query = `
    WITH
      -- Compute tile envelope once and transform it to EPSG:4326
      bounds AS (
        SELECT public.ST_Transform(public.ST_TileEnvelope($1, $2, $3), 4326) AS geom
      ),
      mvt_data AS (
        SELECT
          public.ST_AsMVTGeom(
            public.ST_Transform(r.geom, 3857),  -- transform feature to 3857 for tile encoding
            public.ST_TileEnvelope($1, $2, $3),      -- use the original tile envelope in 3857
            4096, 64, true
          ) AS geom,
          r.id
        FROM bugs_matter.journeys_server r, bounds
        -- Now use the pre-transformed bounds (in 4326) for intersection test
        WHERE public.ST_Intersects(r.geom, bounds.geom) AND
        r.region_id = $4
      )
    SELECT public.ST_AsMVT(mvt_data.*, 'lines', 4096, 'geom') AS tile
    FROM mvt_data;
  `;

  try {
    const client = await pool.connect();
    const result = await client.query(query, [z, x, y, regionId]);
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

// Region by year
app.get('/tiles/regions/:regionId/years/:year/:z/:x/:y.pbf', async (req, res) => {
  const { regionId, year, z, x, y } = req.params;

  // ChatGPT made this optimised query to generate .pbf tiles dynamically
  const query = `
    WITH
      -- Compute tile envelope once and transform it to EPSG:4326
      bounds AS (
        SELECT public.ST_Transform(public.ST_TileEnvelope($1, $2, $3), 4326) AS geom
      ),
      mvt_data AS (
        SELECT
          public.ST_AsMVTGeom(
            public.ST_Transform(r.geom, 3857),  -- transform feature to 3857 for tile encoding
            public.ST_TileEnvelope($1, $2, $3),      -- use the original tile envelope in 3857
            4096, 64, true
          ) AS geom,
          r.id
        FROM bugs_matter.journeys_server r, bounds
        -- Now use the pre-transformed bounds (in 4326) for intersection test
        WHERE public.ST_Intersects(r.geom, bounds.geom) AND
        r.region_id = $4 AND
        r.year = $5
      )
    SELECT public.ST_AsMVT(mvt_data.*, 'lines', 4096, 'geom') AS tile
    FROM mvt_data;
  `;

  try {
    const client = await pool.connect();
    const result = await client.query(query, [z, x, y, regionId, year]);
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

app.listen(port, () => {
  console.log(`Live tile server listening at http://localhost:${port}`);
});

