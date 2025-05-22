# Building a Prod-Ready, Robust Shiny Application.
#
# README: these dev scripts help ensure KWT Webapps are consistent, reliable and production-ready
# By following each step, you'll be helping your future self and any other maintainers!
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# use source("dev/03_deploy.R") each time you want to merge into main
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## System checks
## Does your R version match the expected for this template?
if (getRversion() != "4.4.2") {
  stop("Your R Version doesn't match the expected for this project (4.4.2). Please update or get in touch with the webapp team")
}

## Fill the DESCRIPTION ----
## Essential documentation for future maintenance of this package!
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "bugsMatter", #must be camelCase (but capitilise acronyms fully e.g. LNRSMeasuresWebmap)
  pkg_title = "Bugs Matter Dashboard", #please give a title for the app (to be displayed on ShinyProxy)
  pkg_description = "Car number plate insect splat counts as a measure of invertebrate abundance change over time.", #please give a description to help future maintainers understand what this is for!!
  authors = c(person( #add yourself here!
    given = "Lawrence",
    family = "Ball",
    email = "lawrence.ball@kentwildlife.org.uk",
    role = c(
      "aut", #means package author
      "cre" #means package maintainer
    )
  ),
  person( #add yourself here!
    given = "Euan",
    family = "Mckenzie",
    email = "euan.mckenzie@kentwildlife.org.uk",
    role = c(
      "aut" #means package author
    )
  )),
  repo_url = "https://github.com/kentwildlifetrust/bugs_matter_dashboard" #switch for the github url
)

## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
## See ?usethis for more information

## The mit license is a permissive license allowing code to be reused and modified, without any warranty
usethis::use_mit_license("Kent Wildlife Trust")

## instructions on how to install app need to go in the Readme.
## Mention any configuration required outside the code, such as Auth0
usethis::use_readme_rmd(open = FALSE)

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)

## Dependency management
## Use renv to manage dependencies.
renv::install()
#check that renv version matches the Dockerfile template
if (packageVersion("renv") != "1.1.1") {
  "Your renv version (%s) doesn't match the expected for this project (1.1.1).
    You can update renv using renv::upgrade()" |>
    sprintf(packageVersion("renv")) |>
    stop()
}

renv::init(
  ## Local development should use Imperial as the main CRAN mirror for speedier downloads.
  repos = list(CRAN = "https://cran.ma.imperial.ac.uk/"),
  ## Explicit mode is recommneded to help with maintainability
  ## It allows the maintainer to specify package versions that cause problems, while updating other packages
  ## There is less chance of development dependencies entering lockfile and increasing build costs
  settings = list(snapshot.type = "explicit")
)

# You're now set! ----
# Time to restart the R console, e.g. using Ctrl + Shift + F10 in RStudio,
# or by q() in a terminal, followed by radian

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
