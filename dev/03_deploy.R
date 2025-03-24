# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# use source("dev/03_deploy.R") each time you want to merge into main
#
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod.
devtools::check()

## Deploy to ShinyProxy ---
## generate renv.lock.prod using a different package repository
## p3m provides pre-built binaries so the Docker build doesn't take ages
renv::snapshot(
    lockfile = "renv.lock.prod",
    repos = c(CRAN = "https://p3m.dev/cran/__linux__/noble/latest")
)
## renv changes the repo url for some reason and breaks the lockfile
readLines("renv.lock.prod") |>
    stringr::str_replace("https://p3m.dev/cran/latest", "https://p3m.dev/cran/__linux__/noble/latest") |>
    writeLines("renv.lock.prod")


## get system dependencies for Ubuntu, which is the OS to run inside the Docker container
pak::pkg_install("desc")
sys_deps <- desc::desc_get_deps(file = "DESCRIPTION") |>
    dplyr::pull(package) |>
    unique() |>
    pak::pkg_sysreqs(sysreqs_platform = "ubuntu")

## add Dockerfile based on template
pak::pkg_install("stringr")
if (!file.exists("Dockerfile") || readline("Are you sure you would like to overwrite an existing Dockerfile? y/n ") == "y") {
    readLines("dev/build-templates/Dockerfile") |>
        stringr::str_replace("apt-get -y install make", sys_deps[[5]]) |>
        stringr::str_replace_all("blankWebapp", golem::pkg_name()) |>
        writeLines("Dockerfile")
}
## GITHUB DEPENDENCIES: you'll need to manually pass in GITHUB_PAT to the build function
## e.g. docker build --progress=plain --build-arg GITHUB_PAT=${{ secrets.SHINY_HELPER_PAT }} -t planning_portal .
## uncomment ARG GITHUB_PAT from Dockerfile
## Ask Kia or Euan for help getting GitHub PATs

## add .dockerignore based on template
if (
    !file.exists(".dockerignore") ||
    readline("Are you sure you would like to overwrite an existing .dockerignore? y/n ") == "y"
   ) {
    readLines("dev/build-templates/.dockerignore") |>
        writeLines(".dockerignore")
}

## add docker-build.yml based on template
pak::pkg_install("snakecase")
if (!dir.exists(".github")) dir.create(".github")
if (!dir.exists(".github/workflows")) dir.create(".github/workflows")
if (
    !file.exists(".github/workflows/docker-build.yml") ||
    readline("Are you sure you would like to overwrite an existing docker-build.yml? y/n ") == "y"
   ) {
    readLines("dev/build-templates/docker-build.yml") |>
        stringr::str_replace("blank_webapp", snakecase::to_snake_case(golem::pkg_name())) |>
        writeLines(".github/workflows/docker-build.yml")
}

## now merge into main
cat("Success! You can now merge into main.\n\n")
cat("To run locally:\n\n")
cat("docker build -t", snakecase::to_snake_case(golem::pkg_name()), ".\n")
cat('docker run -p 3838:3838 --env-file "path/to/your/env-file"', snakecase::to_snake_case(golem::pkg_name()), ".\n")