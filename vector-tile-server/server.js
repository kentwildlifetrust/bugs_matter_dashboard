const express = require('express');
const MBTiles = require('@mapbox/mbtiles');
const cors = require('cors');

const app = express();
const PORT = 3000;

app.use(cors({
    origin: process.env.FRONTEND_URL || 'http://127.0.0.1:3838',
    methods: ['GET']       // Specify allowed HTTP methods
}));

// Open your MBTiles file
new MBTiles('./.data/journeys-2021-to-2024.mbtiles', (err, mbtiles) => {
  if (err) {
    console.error('Error opening MBTiles:', err);
    process.exit(1);
  }

  // Define a route to serve the vector tiles
  app.get('/tiles/:z/:x/:y.pbf', (req, res) => {
    const z = parseInt(req.params.z, 10);
    const x = parseInt(req.params.x, 10);
    let y = parseInt(req.params.y, 10);

    // If using the Google/Bing (XYZ) scheme, flip the y coordinate:
    // y = (1 << z) - 1 - y;

    mbtiles.getTile(z, x, y, (err, tile, headers) => {
      if (err) {
        res.status(404).send('Tile not found');
      } else {
        res.set({
          'Content-Type': 'application/x-protobuf',
          'Content-Encoding': 'gzip',
          ...headers,
        });
        res.send(tile);
      }
    });
  });

  app.listen(PORT, () => {
    console.log(`Tile server is running on port ${PORT}`);
  });
});
