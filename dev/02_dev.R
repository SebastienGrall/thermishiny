# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency

usethis::use_package("ggplot2")
usethis::use_package("DBI")
usethis::use_package("shinydashboard")
usethis::use_package("shinydashboardPlus")
usethis::use_package("dplyr")
usethis::use_package("shinybusy")
usethis::use_package("plotly")
usethis::use_package("rle")
usethis::use_package("data.table")
usethis::use_package("spsComps")
usethis::use_package("magrittr")
usethis::use_package("shinyWidgets")
usethis::use_package("rlang")
usethis::use_package("lubridate")
usethis::use_package("shinyjs")
usethis::use_package("stringr")
usethis::use_package("sf")
usethis::use_package("leaflet")
usethis::use_package("dbplyr")
usethis::use_package("RPostgres")


## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "main_page") # Main page for the shiny interface
golem::add_module( name = "header" ) # Header for the shiny interface
golem::add_module( name = "left_side_bar") # left side bar
golem::add_module( name = "main_page_login" ) # login page
golem::add_module( name = "import_stations" ) # module d'import des stations
golem::add_module( name = "import_data_temp" ) # module d'import des données de température des sondes
golem::add_module( name = "import_data_releve" ) # module d'import des données de relève des sondes
golem::add_module( name = "analysis_from_file" ) # module d'analyse des températures depuis un fichier csv
golem::add_module( name = "analysis_from_bdd" ) # module d'analyse des températures depuis la bdd
golem::add_module( name = "export_data_from_bdd" ) # module d'export des données de la base de données

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct( "helpers" ) 
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("Thermie.shiny")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
## 
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action() 
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release() 
usethis::use_github_action_check_standard() 
usethis::use_github_action_check_full() 
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis() 
usethis::use_travis_badge() 

# AppVeyor 
usethis::use_appveyor() 
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")