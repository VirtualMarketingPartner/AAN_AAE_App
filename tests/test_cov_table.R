library(testthat)
library(dplyr)
library(DT)
library(htmltools)
library(htmlwidgets)
library(RColorBrewer)
library(leaflet)

# Environment setup for tests ----

# Source functions to be tested
source(file = file.path("..", "app_functions", "visualization_functions.R"), chdir = TRUE)


# Test ----

test_that("Output is a DataTable", {
  load(file.path("testing_objects", "Asthma_Dashboard_Birmingham_AL.RData"))
  
  actual <- build_cov_table(
    geo_shp_dat, 
    cov = "Particulate.Matter", 
    outcome = "CASTHMA_CrudePrev_BRFSS", 
    geo_unit = "Census Tract", 
    geo_level = "Place (Census-tract level data)",
    highlighted_geography = NULL
  )
  
  # Datatables are made from lists
  expect_type(
    actual,
    "list"
  )
  
  # Make sure that output is a datatable
  expect_identical(
    class(actual),
    c("datatables", "htmlwidget")
  )
})

test_that("Number of rows is correct", {
  load(file.path("testing_objects", "Asthma_Dashboard_Birmingham_AL.RData"))
  
  actual <- build_cov_table(
    geo_shp_dat, 
    cov = "holc_grade_pop", 
    outcome = "CASTHMA_CrudePrev_BRFSS", 
    geo_unit = "Census Tract", 
    geo_level = "Place (Census-tract level data)",
    highlighted_geography = NULL
  )
  
  expect_identical(
    nrow(actual$x$data),
    as.integer(84)  # Function removes empty covariate values
  )
})