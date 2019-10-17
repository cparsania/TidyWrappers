## add imports to DESCRIPTION file
usethis::use_package(type = "imports" ,package = "dplyr")
usethis::use_package(type = "imports" ,package = "purrr")
usethis::use_package(type = "imports" ,package = "magrittr")

## build docs
devtools::document()

##

usethis::use_version()
usethis::use_dev_version()

