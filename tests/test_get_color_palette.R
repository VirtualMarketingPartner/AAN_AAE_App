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

test_that("get_color_palette errors when given nonexistent var_name", {
  # Dataset to test on
  test_dat <- data.frame(
    CASTHMA_CrudePrev_BRFSS = c(9.7, 8.8, 11.0, 9.8, 9.6, 10.8),
    holc_grade_pop = c(3.384065, 3.999415, 3.614645, 4.000000, 4.000000, 3.772933)
  )

  expect_error(
    # Nonexistent variable name
    get_color_palette(
      "nonexistent_variable_name",
      test_dat[["nonexistent_variable_name"]]
    )
  )
})

test_that("get_color_palette errors when given empty data set", {
  empty_df <- data.frame()

  expect_error(
    # Empty data set
    get_color_palette(
      "CASTHMA_CrudePrev_BRFSS",
      empty_df
    )
  )
})

test_that("get_color_palette returns holc color palette for holc_grade_pop", {
  # Dataset to test on
  test_dat <- data.frame(
    CASTHMA_CrudePrev_BRFSS = c(9.7, 8.8, 11.0, 9.8, 9.6, 10.8),
    holc_grade_pop = c(3.384065, 3.999415, 3.614645, 4.000000, 4.000000, 3.772933)
  )

  # Get palette
  p <- get_color_palette(
    "holc_grade_pop",
    test_dat$holc_grade_pop,
    holc_colors = holc_colors
  )

  # Palette colors are correct for given set of holc colors
  expect_identical(
    p(test_dat$holc_grade_pop),
    c(
      "#C5AA7E",
      "#C27E8D",
      "#C59A84",
      "#C27E8D",
      "#C27E8D",
      "#C48E88"
    )
  )
})

test_that("get_color_palette returns a colorNumeric palette", {
  # Dataset to test on
  test_dat <- data.frame(
    CASTHMA_CrudePrev_BRFSS = c(9.7, 8.8, 11.0, 9.8, 9.6, 10.8),
    holc_grade_pop = c(3.384065, 3.999415, 3.614645, 4.000000, 4.000000, 3.772933)
  )

  p <- get_color_palette(
    "CASTHMA_CrudePrev_BRFSS",
    test_dat$CASTHMA_CrudePrev_BRFSS
  )

  # Check class
  expect_identical(attr(p, "colorType"), "numeric")
})

test_that("chosen_palette works correctly", {
  # Dataset to test on
  test_dat <- data.frame(
    CASTHMA_CrudePrev_BRFSS = c(9.7, 8.8, 11.0, 9.8, 9.6, 10.8),
    holc_grade_pop = c(3.384065, 3.999415, 3.614645, 4.000000, 4.000000, 3.772933),
    Rank = c(1, 2, 3, 4, 5, 6)
  )

  # If chosen_palette is TRUE, var_name must be Rank
  expect_error(
    get_color_palette(
      var_name = "CASTHMA_CrudePrev_BRFSS",
      var_data = test_dat$CASTHMA_CrudePrev_BRFSS,
      chosen_palette = TRUE
    )
  )

  # If var_name is Rank, chosen_palette must be TRUE
  expect_error(
    get_color_palette(
      var_name = "Rank",
      var_data = test_dat$Rank,
      chosen_palette = FALSE
    )
  )

  # Should run smoothly if parameters are set up correctly
  expect_no_error(
    get_color_palette(
      var_name = "Rank",
      var_data = test_dat$Rank,
      chosen_palette = TRUE
    )
  )
})
