FROM rocker/verse:4.4.2

# Install system dependencies
# not sure if all needed (copied from wilder_carbon_registry)
RUN apt-get update -y && apt-get install -y  make \
    pandoc \
    zlib1g-dev \
    libpq-dev \
    libicu-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libx11-dev \
    git \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    libjpeg-dev \
    libpng-dev \
    libtiff-dev \
    libgdal-dev \
    gdal-bin \
    libgeos-dev \
    libproj-dev \
    libsqlite3-dev \
    libudunits2-dev \
    libmagick++-dev \
    gsfonts \
    libprotobuf-dev \
    protobuf-compiler \
    libprotoc-dev && rm -rf /var/lib/apt/lists/*


# install R dependencies
# do this before copying the app-code, to ensure this layer is cached
WORKDIR /build

# Install required R packages
RUN R -q -e "options(warn=2); install.packages(c('shiny', 'leaflet', 'sf', 'dplyr', 'tidyr', 'pool', 'RPostgres', \
                                                 'ggplot2', 'MASS', 'sjPlot', 'scales', 'shinycssloaders', \
                                                 'shinyFeedback', 'shinydashboard', 'shinyjs', 'bslib', 'slickR'))"

# install R code
COPY . /app_pkg

RUN ls -lah /app_pkg

# Set up runtime
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/app_pkg', host='0.0.0.0', port=3838)"]


