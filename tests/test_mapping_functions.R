library(testthat)
library(dplyr)
library(DT)
library(htmltools)
library(htmlwidgets)
library(RColorBrewer)
library(leaflet)
library(sp)
library(ggplot2)

# Environment setup for tests ----

# Source functions to be tested
source(file = file.path("..", "app_functions", "visualization_functions.R"), chdir = TRUE)

# Test ----

test_that("Defaults for leaflet should run without error", {
  load(file.path("testing_objects", "Asthma_Dashboard_Birmingham_AL.RData"))
  
  expect_no_error(
    build_map(
      geo_shp_dat,
      geo_level = "Place (Census-tract level data)",
      cov_map = "Particulate.Matter",
      show_asthma = TRUE,
      show_missing = FALSE,
      highlighted_geography = NULL,
      fqhc_dat = NULL,
      school_dat = NULL,
      chosen_palette = FALSE
    )
  )
  
  # Should run when show_asthma is turned off
  expect_no_error(
    build_map(
      geo_shp_dat,
      geo_level = "Place (Census-tract level data)",
      cov_map = "Particulate.Matter",
      show_asthma = FALSE,
      show_missing = FALSE,
      highlighted_geography = NULL,
      fqhc_dat = NULL,
      school_dat = NULL,
      chosen_palette = FALSE
    )
  )
  
  # Should run when geography is highlighted
  expect_no_error(
    build_map(
      geo_shp_dat,
      geo_level = "Place (Census-tract level data)",
      cov_map = "Particulate.Matter",
      show_asthma = TRUE,
      show_missing = FALSE,
      highlighted_geography = "01073000300",
      fqhc_dat = NULL,
      school_dat = NULL,
      chosen_palette = FALSE
    )
  )
})

test_that("leaflet map type returns a leaflet map", {
  load(file.path("testing_objects", "Asthma_Dashboard_Birmingham_AL.RData"))
  
  map <- build_map(
    geo_shp_dat,
    geo_level = "Place (Census-tract level data)",
    cov_map = "Particulate.Matter",
    show_asthma = TRUE,
    show_missing = FALSE,
    highlighted_geography = NULL,
    fqhc_dat = NULL,
    school_dat = NULL,
    chosen_palette = FALSE
  )
  
  # Leaflet is created from a list
  expect_type(
    map,
    "list"
  )
  
  # Class leaflet
  expect_identical(
    class(map),
    c("leaflet", "htmlwidget")
  )
})

test_that("ggplot map type returns a ggplot map", {
  load(file.path("testing_objects", "Asthma_Dashboard_Birmingham_AL.RData"))
  
  map <- build_map(
    geo_shp_dat,
    geo_level = "Place (Census-tract level data)",
    cov_map = "Particulate.Matter",
    show_asthma = TRUE,
    show_missing = FALSE,
    highlighted_geography = NULL,
    fqhc_dat = NULL,
    school_dat = NULL,
    chosen_palette = FALSE,
    map_type = "ggplot"
  )
  
  # ggplot is created from a list
  expect_type(
    map,
    "list"
  )
  
  # Class ggplot
  expect_identical(
    class(map),
    c("gg", "ggplot")
  )
})

test_that("Expected errors occur", {
  load(file.path("testing_objects", "Asthma_Dashboard_Birmingham_AL.RData"))
  
  # Covariate doesn't exist
  expect_error(
    build_map(
      geo_shp_dat,
      geo_level = "Place (County level data)",
      cov_map = "nonexistent_covariate",
      show_asthma = TRUE,
      show_missing = FALSE,
      highlighted_geography = NULL,
      fqhc_dat = NULL,
      school_dat = NULL,
      chosen_palette = FALSE
    )
  )
  
  # Data must be a list with "SpatialPolygonsDataFrame"
  expect_error(
    build_map(
      geo_shp_dat$dat,
      geo_level = "Place (County level data)",
      cov_map = "Particulate.Matter",
      show_asthma = TRUE,
      show_missing = FALSE,
      highlighted_geography = NULL,
      fqhc_dat = NULL,
      school_dat = NULL,
      chosen_palette = FALSE
    )
  )
})

test_that("All geography levels run leaflet without error", {
  load(file.path("testing_objects", "Asthma_Dashboard_Birmingham_AL.RData"))
  
  expect_no_error(
    build_map(
      geo_shp_dat,
      geo_level = "Place (Census-tract level data)",
      cov_map = "Particulate.Matter",
      show_asthma = TRUE,
      show_missing = FALSE,
      highlighted_geography = NULL,
      fqhc_dat = NULL,
      school_dat = NULL,
      chosen_palette = FALSE
    )
  )
  
  load(file.path("testing_objects", "Asthma_Dashboard_Bullock County_AL.RData"))
  
  expect_no_error(
    build_map(
      geo_shp_dat,
      geo_level = "County (Census-tract level data)",
      cov_map = "Particulate.Matter",
      show_asthma = TRUE,
      show_missing = FALSE,
      highlighted_geography = NULL,
      fqhc_dat = NULL,
      school_dat = NULL,
      chosen_palette = FALSE
    )
  )
  
  load(file.path("testing_objects", "Asthma_Dashboard_AL.RData"))
  
  expect_no_error(
    build_map(
      geo_shp_dat,
      geo_level = "State (County level data)",
      cov_map = "Particulate.Matter",
      show_asthma = TRUE,
      show_missing = FALSE,
      highlighted_geography = NULL,
      fqhc_dat = NULL,
      school_dat = NULL,
      chosen_palette = FALSE
    )
  )
})

test_that("All geography levels run ggplot without error", {
  load(file.path("testing_objects", "Asthma_Dashboard_Birmingham_AL.RData"))
  
  expect_no_error(
    build_map(
      geo_shp_dat,
      geo_level = "Place (Census-tract level data)",
      cov_map = "Particulate.Matter",
      show_asthma = TRUE,
      show_missing = FALSE,
      highlighted_geography = NULL,
      fqhc_dat = NULL,
      school_dat = NULL,
      chosen_palette = FALSE,
      map_type = "ggplot"
    )
  )
  
  load(file.path("testing_objects", "Asthma_Dashboard_Bullock County_AL.RData"))
  
  expect_no_error(
    build_map(
      geo_shp_dat,
      geo_level = "County (Census-tract level data)",
      cov_map = "Particulate.Matter",
      show_asthma = TRUE,
      show_missing = FALSE,
      highlighted_geography = NULL,
      fqhc_dat = NULL,
      school_dat = NULL,
      chosen_palette = FALSE,
      map_type = "ggplot"
    )
  )
  
  load(file.path("testing_objects", "Asthma_Dashboard_AL.RData"))
  
  expect_no_error(
    build_map(
      geo_shp_dat,
      geo_level = "State (County level data)",
      cov_map = "Particulate.Matter",
      show_asthma = TRUE,
      show_missing = FALSE,
      highlighted_geography = NULL,
      fqhc_dat = NULL,
      school_dat = NULL,
      chosen_palette = FALSE,
      map_type = "ggplot"
    )
  )
})
