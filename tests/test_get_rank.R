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

test_that("No errors when outcome and covariate data present", {
  test_dat <- data.frame(
    CASTHMA_CrudePrev_BRFSS = c(9.7, 8.8, 11.0, 9.8, 9.6, 10.8),
    holc_grade_pop = c(3.384065, 3.999415, 3.614645, 4.000000, 4.000000, 3.772933),
    Particulate.Matter = c(9.418296, 10.301028, 10.155254, 10.281074, 10.229297, 9.503013),
    pop_acs = c(2546, 1398, 3161, 3146, 2096, 3124)
  )

  # Should run
  expect_no_error(
    get_rank(test_dat, "Particulate.Matter", "CASTHMA_CrudePrev_BRFSS")
  )
})

test_that("Rank errors when covariate does not exist", {
  test_dat <- data.frame(
    CASTHMA_CrudePrev_BRFSS = c(9.7, 8.8, 11.0, 9.8, 9.6, 10.8),
    holc_grade_pop = c(3.384065, 3.999415, 3.614645, 4.000000, 4.000000, 3.772933),
    pop_acs = c(2546, 1398, 3161, 3146, 2096, 3124)
  )

  # Actual output
  expect_error(get_rank(test_dat, "Particulate.Matter", "CASTHMA_CrudePrev_BRFSS"))
})

test_that("Rank is calculated correctly when outcome and covariate data present", {
  test_dat <- data.frame(
    CASTHMA_CrudePrev_BRFSS = c(9.7, 8.8, 11.0, 9.8, 9.6, 10.8),
    holc_grade_pop = c(3.384065, 3.999415, 3.614645, 4.000000, 4.000000, 3.772933),
    Particulate.Matter = c(9.418296, 10.301028, 10.155254, 10.281074, 10.229297, 9.503013),
    pop_acs = c(2546, 1398, 3161, 3146, 2096, 3124)
  )

  # Actual output
  actual_df <- get_rank(test_dat, "Particulate.Matter", "CASTHMA_CrudePrev_BRFSS") %>%
    arrange(final_rank)

  # Expected output (subset)
  expected_df <- data.frame(
    final_rank = c(1, 2, 3, 4, 5, 6),
    CASTHMA_CrudePrev_BRFSS = c(9.8, 11.0, 8.8, 10.8, 9.6, 9.7),
    Particulate.Matter = c(10.281074, 10.155254, 10.301028, 9.503013, 10.229297, 9.418296)
  )

  # Data frame is built from list
  expect_type(
    actual_df,
    "list"
  )

  # Output should be same length as input (with na's omitted)
  expect_identical(
    nrow(actual_df),
    nrow(na.omit(test_dat))
  )

  # Order of final rank should match in outcome variable
  expect_identical(
    actual_df$CASTHMA_CrudePrev_BRFSS,
    expected_df$CASTHMA_CrudePrev_BRFSS
  )

  # Order of final rank should match in covariate
  expect_identical(
    actual_df$Particulate.Matter,
    expected_df$Particulate.Matter
  )
})

test_that("Rank breaks ties appropriately", {
  test_dat <- data.frame(
    CASTHMA_CrudePrev_BRFSS = c(9.7, 8.8, 11.0, 9.8, 9.6, 10.8),
    holc_grade_pop = c(3.384065, 3.999415, 3.614645, 4.000000, 4.000000, 3.772933),
    Particulate.Matter = c(9.418296, 10.301028, 10.155254, 10.281074, 10.229297, 9.503013),
    pop_acs = c(2546, 1398, 3161, 3146, 2096, 3124)
  )

  # Actual output
  actual_df <- get_rank(test_dat, "holc_grade_pop", "CASTHMA_CrudePrev_BRFSS") %>%
    arrange(final_rank)

  # Expected output (subset)
  expected_df <- data.frame(
    final_rank = c(1, 2, 3, 4, 5, 6),
    CASTHMA_CrudePrev_BRFSS = c(9.8, 10.8, 11.0, 9.6, 8.8, 9.7),
    holc_grade_pop = c(4.000000, 3.772933, 3.614645, 4.000000, 3.999415, 3.384065)
  )

  # Breaks ties appropriately
  expect_identical(
    actual_df$CASTHMA_CrudePrev_BRFSS,
    expected_df$CASTHMA_CrudePrev_BRFSS
  )

  expect_identical(
    actual_df$holc_grade_pop,
    expected_df$holc_grade_pop
  )
})

test_that("Rank doesn't use covariate when population metric", {
  test_dat <- data.frame(
    CASTHMA_CrudePrev_BRFSS = c(9.7, 8.8, 11.0, 9.8, 9.6, 10.8),
    nonhis.black_acs = c(1962, 1060, 2588, 2974, 2060, 3021),
    pop_acs = c(2546, 1398, 3161, 3146, 2096, 3124)
  )

  # Actual output
  actual_df <- get_rank(test_dat, "nonhis.black_acs", "CASTHMA_CrudePrev_BRFSS") %>%
    arrange(final_rank)

  # Expected output (subset)
  expected_df <- data.frame(
    final_rank = c(1, 2, 3, 4, 5, 6),
    CASTHMA_CrudePrev_BRFSS = c(8.8, 9.6, 9.7, 9.8, 10.8, 11.0),
    nonhis.black_acs = c(1060, 2060, 1962, 2974, 3021, 2588)
  )

  # Breaks ties appropriately
  expect_identical(
    actual_df$CASTHMA_CrudePrev_BRFSS,
    expected_df$CASTHMA_CrudePrev_BRFSS
  )

  expect_identical(
    actual_df$nonhis.black_acs,
    expected_df$nonhis.black_acs
  )
})

test_that("Rank defaults to covariate rank when outcome data is empty", {
  test_dat <- data.frame(
    CASTHMA_CrudePrev_BRFSS = NA,
    Particulate.Matter = c(9.418296, 10.301028, 10.155254, 10.281074, 10.229297, 9.503013),
    pop_acs = c(2546, 1398, 3161, 3146, 2096, 3124)
  )

  # Actual output
  actual_df <- get_rank(test_dat, "Particulate.Matter", "CASTHMA_CrudePrev_BRFSS") %>%
    arrange(final_rank)

  # Expected output (subset)
  expected_df <- data.frame(
    final_rank = c(1, 2, 3, 4, 5, 6),
    CASTHMA_CrudePrev_BRFSS = NA,
    Particulate.Matter = c(10.301028, 10.281074, 10.229297, 10.155254, 9.503013, 9.418296)
  )

  # Types can be double and integer, want to match exactly
  expect_identical(
    actual_df$final_rank %>% as.integer(),
    actual_df$covariate_rank %>% as.integer()
  )

  expect_identical(
    actual_df$Particulate.Matter,
    expected_df$Particulate.Matter
  )
})

test_that("Rank defaults to covariate rank when outcome column doesn't exist", {
  test_dat <- data.frame(
    Particulate.Matter = c(9.418296, 10.301028, 10.155254, 10.281074, 10.229297, 9.503013),
    pop_acs = c(2546, 1398, 3161, 3146, 2096, 3124)
  )

  # Actual output
  actual_df <- get_rank(test_dat, "Particulate.Matter", "CASTHMA_CrudePrev_BRFSS") %>%
    arrange(final_rank)

  # Expected output (subset)
  expected_df <- data.frame(
    final_rank = c(1, 2, 3, 4, 5, 6),
    Particulate.Matter = c(10.301028, 10.281074, 10.229297, 10.155254, 9.503013, 9.418296)
  )

  # Types can be double and integer, want to match exactly
  expect_identical(
    actual_df$final_rank %>% as.integer(),
    actual_df$covariate_rank %>% as.integer()
  )

  expect_identical(
    actual_df$Particulate.Matter,
    expected_df$Particulate.Matter
  )
})

test_that("Rank is identical for every row when no outcome data and covariate is a population metric", {
  test_dat1 <- data.frame(
    CASTHMA_CrudePrev_BRFSS = NA,
    nonhis.black_acs = c(1962, 1060, 2588, 2974, 2060, 3021),
    pop_acs = c(2546, 1398, 3161, 3146, 2096, 3124)
  )

  test_dat2 <- data.frame(
    nonhis.black_acs = c(1962, 1060, 2588, 2974, 2060, 3021),
    pop_acs = c(2546, 1398, 3161, 3146, 2096, 3124)
  )

  # Actual output
  actual_df1 <- get_rank(test_dat1, "nonhis.black_acs", "CASTHMA_CrudePrev_BRFSS") %>%
    arrange(final_rank)

  actual_df2 <- get_rank(test_dat2, "nonhis.black_acs", "CASTHMA_CrudePrev_BRFSS") %>%
    arrange(final_rank)

  # Expected output (subset)
  expected_df1 <- data.frame(
    final_rank = c(1, 1, 1, 1, 1, 1),
    CASTHMA_CrudePrev_BRFSS = NA,
    nonhis.black_acs = c(1962, 1060, 2588, 2974, 2060, 3021)
  )

  expected_df2 <- data.frame(
    final_rank = c(1, 1, 1, 1, 1, 1),
    nonhis.black_acs = c(1962, 1060, 2588, 2974, 2060, 3021)
  )

  # Types can be double and integer, want to match exactly
  expect_identical(
    actual_df1$final_rank,
    c(1, 1, 1, 1, 1, 1)
  )
  expect_identical(
    actual_df2$final_rank,
    c(1, 1, 1, 1, 1, 1)
  )

  expect_identical(
    actual_df1$nonhis.black_acs,
    expected_df1$nonhis.black_acs
  )
  expect_identical(
    actual_df2$nonhis.black_acs,
    expected_df2$nonhis.black_acs
  )
})
