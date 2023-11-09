library(testthat)
library(dplyr)
library(DT)
library(htmltools)
library(htmlwidgets)
library(RColorBrewer)
library(leaflet)
library(plotly)
library(sp)
library(ggplot2)
library(raster)

# Environment setup for tests ----

# Source functions to be tested
source(file = file.path("..", "app_functions", "visualization_functions.R"), chdir = TRUE)

# Test ----

test_that("Defaults for plotly should run without error", {
  load(file.path("testing_objects", "Asthma_Dashboard_Birmingham_AL.RData"))
  
  # Should run on only defaults
  expect_no_error(
    build_cov_outcome_scatterplot(
      geo_shp_dat,
      "Particulate.Matter",
      "CASTHMA_CrudePrev_BRFSS"
    )
  )
  
  # Should run when geography is selected
  expect_no_error(
    build_cov_outcome_scatterplot(
      geo_shp_dat,
      "Particulate.Matter",
      "CASTHMA_CrudePrev_BRFSS",
      highlighted_geography = "01073000300"
    )
  )
  
  # Should run when rank is turned on
  expect_no_error(
    build_cov_outcome_scatterplot(
      geo_shp_dat,
      "Particulate.Matter",
      "CASTHMA_CrudePrev_BRFSS",
      chosen_palette = TRUE
    )
  )
  
  # Should run when bubble size is changed
  expect_no_error(
    build_cov_outcome_scatterplot(
      geo_shp_dat,
      "Particulate.Matter",
      "CASTHMA_CrudePrev_BRFSS",
      bubble_size = "Population Size"
    )
  )
})

test_that("plotly plot type returns a plotly", {
  load(file.path("testing_objects", "Asthma_Dashboard_Birmingham_AL.RData"))
  
  scatterplot <-
    build_cov_outcome_scatterplot(
      geo_shp_dat,
      "Particulate.Matter",
      "CASTHMA_CrudePrev_BRFSS"
    )
  
  # plotly is created from a list
  expect_type(
    scatterplot,
    "list"
  )
  
  # Class leaflet
  expect_identical(
    class(scatterplot),
    c("plotly", "htmlwidget")
  )
})

test_that("ggplot plot type returns a ggplot", {
  load(file.path("testing_objects", "Asthma_Dashboard_Birmingham_AL.RData"))
  
  scatterplot <-
    build_cov_outcome_scatterplot(
      geo_shp_dat,
      "Particulate.Matter",
      "CASTHMA_CrudePrev_BRFSS",
      scatter_type = "ggplot"
    )
  
  # ggplot is created from a list
  expect_type(
    scatterplot,
    "list"
  )
  
  # Class ggplot
  expect_identical(
    class(scatterplot),
    c("gg", "ggplot")
  )
})

test_that("Expected errors occur", {
  load(file.path("testing_objects", "Asthma_Dashboard_Birmingham_AL.RData"))
  
  # Covariate doesn't exist
  expect_error(
    build_cov_outcome_scatterplot(
      geo_shp_dat,
      "nonexistent_covariate",
      "CASTHMA_CrudePrev_BRFSS"
    )
  )
  
  # Data must be a list with "SpatialPolygonsDataFrame"
  expect_error(
    build_cov_outcome_scatterplot(
      geo_shp_dat$dat,
      "Particulate.Matter",
      "CASTHMA_CrudePrev_BRFSS"
    )
  )
  
  # Incorrect bubble size parameter given
  expect_error(
    build_cov_outcome_scatterplot(
      geo_shp_dat,
      "Particulate.Matter",
      "CASTHMA_CrudePrev_BRFSS",
      bubble_size = "nonexistent bubble size"
    )
  )
  
  # Nonexistent scatter type
  expect_error(
    build_cov_outcome_scatterplot(
      geo_shp_dat,
      "Particulate.Matter",
      "CASTHMA_CrudePrev_BRFSS",
      scatter_type = "nonexistent scatter type"
    )
  )
})

test_that("All geography levels run plotly without error", {
  load(file.path("testing_objects", "Asthma_Dashboard_Birmingham_AL.RData"))
  
  expect_no_error(
    build_cov_outcome_scatterplot(
      geo_shp_dat,
      "Particulate.Matter",
      "CASTHMA_CrudePrev_BRFSS"
    )
  )
  
  load(file.path("testing_objects", "Asthma_Dashboard_Bullock County_AL.RData"))
  
  expect_no_error(
    build_cov_outcome_scatterplot(
      geo_shp_dat,
      "Particulate.Matter",
      "CASTHMA_CrudePrev_BRFSS"
    )
  )
  
  load(file.path("testing_objects", "Asthma_Dashboard_AL.RData"))
  
  expect_no_error(
    build_cov_outcome_scatterplot(
      geo_shp_dat,
      "Particulate.Matter",
      "CASTHMA_CrudePrev_BRFSS"
    )
  )
})

test_that("All geography levels run ggplot without error", {
  load(file.path("testing_objects", "Asthma_Dashboard_Birmingham_AL.RData"))
  
  expect_no_error(
    build_cov_outcome_scatterplot(
      geo_shp_dat,
      "Particulate.Matter",
      "CASTHMA_CrudePrev_BRFSS",
      scatter_type = "ggplot"
    )
  )
  
  load(file.path("testing_objects", "Asthma_Dashboard_Bullock County_AL.RData"))
  
  expect_no_error(
    build_cov_outcome_scatterplot(
      geo_shp_dat,
      "Particulate.Matter",
      "CASTHMA_CrudePrev_BRFSS",
      scatter_type = "ggplot"
    )
  )
  
  load(file.path("testing_objects", "Asthma_Dashboard_AL.RData"))
  
  expect_no_error(
    build_cov_outcome_scatterplot(
      geo_shp_dat,
      "Particulate.Matter",
      "CASTHMA_CrudePrev_BRFSS",
      scatter_type = "ggplot"
    )
  )
})
