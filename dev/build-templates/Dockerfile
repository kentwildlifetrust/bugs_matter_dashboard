FROM rocker/verse:4.4.2

# Install system dependencies
RUN apt-get update -y && \
    apt-get -y install make \
    && rm -rf /var/lib/apt/lists/*

# Create a directory in the image and set wd to it
WORKDIR /build

# Install remotes and renv
RUN R -q -e "options(warn=2); install.packages('remotes')"
RUN R -q -e 'options(warn=2); remotes::install_version("renv", version = "1.1.1")'

#private GitHub dependencies - we need to pass in GITHUB_PAT to be used by renv::restore()
#don't forget to adjust docker-build.yml
#e.g. docker build --progress=plain --build-arg GITHUB_PAT=${{ secrets.SHINY_HELPER_PAT }} -t planning_portal .
#ARG GITHUB_PAT

# Copy renv.lock into image directory and restore packages
COPY renv.lock.prod renv.lock
RUN R -q -e 'options(warn=2); renv::restore()'
RUN rm -rf renv.lock

# add all app files not in .dockerignore to /app_pkg within the image directory
COPY . /app_pkg
# List files in /app_pkg for debugging purposes
RUN ls -lh
# Build R package from /app_pkg. This creates a tarball (tar.gz file) in /build
RUN R CMD build /app_pkg
# Remove the /app_pkg directory
RUN rm -rf /app_pkg

# Install the R package from the tarball
RUN R CMD INSTALL blankWebapp*.tar.gz
# Remove the tarball
RUN rm -rf blankWebapp*.tar.gz

COPY inst inst

EXPOSE 3838
# set options and run app in container
CMD R -e "options('shiny.port'=3838, shiny.host='0.0.0.0', golem.app.prod=TRUE); library(blankWebapp); blankWebapp::run_app()"
