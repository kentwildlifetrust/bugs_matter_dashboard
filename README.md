# Bugs Matter Dashboard

An R Shiny dashboard for visualizing and analyzing car number plate insect splat counts as a measure of invertebrate abundance change over time.

## Description

The Bugs Matter Dashboard provides tools for tracking and analyzing insect abundance using data collected from car number plate splat counts. This dashboard helps monitor changes in invertebrate populations over time, providing valuable insights into ecosystem health. Extra considerations have been taken to manage performance and scalability compared to other KWT Shiny apps, owing to the international significance and expected user demand.

## How it works

The dashboard is a golem-based R Shiny app using {bslib}, with journeys and sign up data held in the `bugs_matter` database in PostgreSQL. The database is updated on a schedule by scraping the Coreo API provided by Natural Apptitude, using ETL pipelines found in [Bugs Matter Pipelines](https://github.com/kentwildlifetrust/bugs-matter-pipelines) repository.

The dashboard aims to be as scalable and responsive as possible, by minimising the data processing carried out in the server. Any heavy data processing is either carried out in the database, or in pre-processing data pipelines. For example, the Analysis tab shows figures that have been pre-generated and saved to the database. This avoids users having to wait for the model to run (30s +) while interacting with the drop downs. This approach is also favoured over `bindCache` as it allows more direct control over model configuration and when to run updates, without disruption to the live dashboard.

### Journeys Map

The Explore Journeys tab of the dashboard shows journeys routes on a map, to show the extensiveness of survey coverage. There are too many journeys for standard rendering using `sf::addPolyLines()`. This relies on a lot of processing by the browser and risks slowing the app down or even crashing it. So, a vector tile approach is used instead, which provides tiles pre-processed to show the appropriate amount of detail for the zoom level. A simple Express JS server, deployed to [https://journeys.bugsmatter.org](https://journeys.bugsmatter.org) provides these vector tiles in [Protobuf (PBF) format](https://docs.mapbox.com/data/tilesets/guides/vector-tiles-standards/). These are added to the Leaflet map using a JavaScript snippet applied via `htmlwidgets::onRender`:

```R
vector_grid_js <- sprintf(
    "
    function(el, x) {
        // Leaflet.VectorGrid is loaded in header
        var vectorGrid = L.vectorGrid.protobuf(
        '%s/tiles/%s%s/{z}/{x}/{y}.pbf', {
            vectorTileLayerStyles: {
                lines: function(properties, zoom) {
                    return {
                        weight: 2,
                        color: '#1D763B',
                        opacity: 0.3
                    };
                }
            },
            maxZoom: 12, // Use your max zoom level
            // Use canvas renderer for better performance with many features
            rendererFactory: L.canvas.tile
        });

        // Add the vector grid layer to the map
        vectorGrid.addTo(this);
    }
    ",
    ifelse(golem::app_dev(), "http://127.0.0.1:5000", "https://journeys.bugsmatter.org"),
    region_param,
    year_param
)
map %>%
    htmlwidgets::onRender(vector_grid_js)
```

The opacity of the lines is set to 0.3, so that routes with more sampling appear darker. Because tiles contain a lot of overlapping lines, rendering can still be slow at low zoom levels (zoomed out). These may be better displayed as some sort of heatmap raster.

## Vector Tile Sever (`/vector-tile-serer`)

The Vector Tile Server is an Express JS app using the PostGIS functions `public.ST_AsMVTGeom()` and `public.ST_AsMVT()` to serve pre-processed tiles on the fly. This approach is reasonably performant while avoiding the need to pregenerate a large number of tiles. However, it does risk putting strain on the database should demand be high. It also performs less well for low zoom levels (zoomed out), in which case a pre-generated heat map raster would be more suitable.

### Environment variables

Environment variables should be added to a gitignored file `vector-tile-server/.env`:
- `DB_USER` - `BugsMatterReadOnly`
- `DB_HOST` - KWT PostgreSQL database host address
- `DB_DATABASE` - `bugs_matter`
- `DB_PASSWORD` - Password for `BugsMatterReadOnly` user
- `DB_PORT` - `5432`

### Run locally

```Powershell
cd vector-tile-server
npm run dev
```

### Deployment

Deployment is to AWS Elastic Beanstalk (EB), to make use of free credits provided by AWS up to July 2026. Deployment is achieved with the [EB Command Line Interface](https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/eb-cli3.html). This wouldn't install on Windows 11, so Linux is used instead via Windows Subsystem for Linux (WSL).

#### Installation

WSL installation is a single command:

```Powershell
wsl --install
```

You should choose Ubuntu as the distro. See the [WSL docs](https://learn.microsoft.com/en-us/windows/wsl/install) for more information, including [how to connect via VS Code](https://learn.microsoft.com/en-us/windows/wsl/tutorials/wsl-vscode).

Once you have VS Code connected to WSL, install git, create a git folder and follow the [instructions to install the EB CLI](https://github.com/aws/aws-elastic-beanstalk-cli-setup). Also clone this repository into WSL.

#### Deployment

[Initial deployment](https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/eb-cli3-configuration.html) was carried out using `eb init`, which set up the environment `journeys-env` and the application `bugs-matter-journeys-vector-tile-server`. You may need to configure AWS Single Sign-on first with `aws sso configure`. After that a domain name was purchased via Route 53 and an SSL certificate set up with Certificate Manager. In future it may be better to manage the domain via Ionos, along with all the other domains, however initially it was easier to set up on AWS.

#### Updates

To update the deployment, first commit any changes and then run `eb deploy`. If necessary authorise AWS using `aws sso login`.

Environment variables are manually configured for `journeys-env` in the AWS Console. Go to Environment: journeys-env > Updates, monitoring and logging > Configuration.