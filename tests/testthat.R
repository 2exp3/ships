library(testthat)
source(here::here("R/global.R")) # app data and grid
source(here::here("R/dropdowns.R"))# module

test_results = test_dir(here::here("tests","testthat"), reporter = "summary", stop_on_failure = TRUE)
