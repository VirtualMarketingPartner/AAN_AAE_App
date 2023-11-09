library(testthat)
library(dplyr)
library(DT)
library(htmltools)
library(htmlwidgets)
library(RColorBrewer)

# Environment setup for tests ----

# Source functions to be tested
source(file = file.path("..", "app_functions", "visualization_functions.R"), chdir = TRUE)


# Test ----

test_that("Explanation has expected components", {
  test_dat <- data.frame(
    CASTHMA_CrudePrev_BRFSS = c(9.7, 8.8, 11.0, 9.8, 9.6, 10.8),
    holc_grade_pop = c(3.384065, 3.999415, 3.614645, 4.000000, 4.000000, 3.772933),
    Particulate.Matter = c(9.418296, 10.301028, 10.155254, 10.281074, 10.229297, 9.503013),
    pop_acs = c(2546, 1398, 3161, 3146, 2096, 3124)
  )
  
  actual <- write_explanation(
    test_dat, 
    cov = "Particulate.Matter", 
    outcome = "CASTHMA_CrudePrev_BRFSS", 
    geo_name = "Birmingham, AL",
    geo_unit = "census tracts", 
    highlighted_geography = NULL
  )
  
  # Extract just the text of the interpretation, all lowercase
  actual_explanation_noHTML <- stringr::str_squish(gsub("<.*?>", "", actual[2])) %>%
    tolower()
  

  # Covariate should be stated somewhere
  expect_true(
    grepl("particulate matter", actual_explanation_noHTML, fixed = TRUE)
  )
  
  # Outcome should be stated somewhere
  expect_true(
    grepl("asthma", actual_explanation_noHTML, fixed = TRUE)
  )
  
  # Geography should be stated somewhere
  expect_true(
    grepl("birmingham", actual_explanation_noHTML, fixed = TRUE)
  )
  expect_true(
    grepl("alabama", actual_explanation_noHTML, fixed = TRUE) |
      grepl("al", actual_explanation_noHTML, fixed = TRUE)
  )
})

test_that("Small sample size results in correct explanation", {
  test_dat <- data.frame(
    CASTHMA_CrudePrev_BRFSS = c(9.7, 8.8),
    Particulate.Matter = c(9.418296, 10.301028),
    pop_acs = c(2546, 1398)
  )
  
  actual <- write_explanation(
    test_dat, 
    cov = "Particulate.Matter", 
    outcome = "CASTHMA_CrudePrev_BRFSS", 
    geo_name = "Birmingham, AL",
    geo_unit = "census tracts", 
    highlighted_geography = NULL
  )
  
  # Extract just the text of the interpretation, all lowercase
  actual_explanation_noHTML <- stringr::str_squish(gsub("<.*?>", "", actual[2])) %>%
    tolower()
  
  # There should be no significance boolean
  expect_true(
    is.na(actual[1])
  )
  
  expect_identical(
    actual_explanation_noHTML,
    "due to the small sample size, we cannot determine a relationship between particulate matter and asthma prevalence."
  )
})

test_that("No outcome data results in correct explanation", {
  test_dat <- data.frame(
    holc_grade_pop = c(3.384065, 3.999415, 3.614645, 4.000000, 4.000000, 3.772933),
    Particulate.Matter = c(9.418296, 10.301028, 10.155254, 10.281074, 10.229297, 9.503013),
    pop_acs = c(2546, 1398, 3161, 3146, 2096, 3124)
  )
  
  actual <- write_explanation(
    test_dat, 
    cov = "Particulate.Matter", 
    outcome = "CASTHMA_CrudePrev_BRFSS", 
    geo_name = "Birmingham, AL",
    geo_unit = "census tracts", 
    highlighted_geography = NULL
  )
  
  # Extract just the text of the interpretation, all lowercase
  actual_explanation_noHTML <- stringr::str_squish(gsub("<.*?>", "", actual[2])) %>%
    tolower()
  
  # There should be no significance boolean
  expect_true(
    is.na(actual[1])
  )
  
  expect_identical(
    actual_explanation_noHTML,
    "due to the absence of asthma prevalence data in birmingham, al, we cannot determine a relationship between particulate matter and asthma prevalence."
  )
})

test_that("Significant association is correct", {
  load(file.path("testing_objects", "Asthma_Dashboard_Birmingham_AL.RData"))
  
  test_dat <- geo_shp_dat$dat
  
  actual <- write_explanation(
    test_dat, 
    cov = "holc_grade_pop", 
    outcome = "CASTHMA_CrudePrev_BRFSS", 
    geo_name = "Birmingham, AL",
    geo_unit = "census tracts", 
    highlighted_geography = NULL
  )
  
  # Extract just the text of the interpretation, all lowercase
  actual_explanation_noHTML <- stringr::str_squish(gsub("<.*?>", "", actual[2])) %>%
    tolower()
  
  # Significance should be FALSE
  expect_identical(
    actual["holc_grade_pop"],
    c("holc_grade_pop" = "TRUE")
  )
  
  # Explanation should say 'not significant'
  expect_true(
    !grepl("not significantly", actual_explanation_noHTML, fixed = TRUE) |
      !grepl("not significant", actual_explanation_noHTML, fixed = TRUE)
  )
  
  expect_true(
    grepl("84 census tracts", actual_explanation_noHTML, fixed = TRUE)
  )
  
  expect_true(
    grepl("holc grade", actual_explanation_noHTML, fixed = TRUE) |
      grepl("redlining", actual_explanation_noHTML, fixed = TRUE)
  )
})

test_that("Non-significant association is correct", {
  test_dat <- data.frame(
    CASTHMA_CrudePrev_BRFSS = c(9.7, 8.8, 11.0, 9.8, 9.6, 10.8),
    holc_grade_pop = c(3.384065, 3.999415, 3.614645, 4.000000, 4.000000, 3.772933),
    Particulate.Matter = c(9.418296, 10.301028, 10.155254, 10.281074, 10.229297, 9.503013),
    pop_acs = c(2546, 1398, 3161, 3146, 2096, 3124)
  )
  
  actual <- write_explanation(
    test_dat, 
    cov = "Particulate.Matter", 
    outcome = "CASTHMA_CrudePrev_BRFSS", 
    geo_name = "Birmingham, AL",
    geo_unit = "census tracts", 
    highlighted_geography = NULL
  )
  
  # Extract just the text of the interpretation, all lowercase
  actual_explanation_noHTML <- stringr::str_squish(gsub("<.*?>", "", actual[2])) %>%
    tolower()
  
  # Significance should be FALSE
  expect_identical(
    actual["Particulate.Matter"],
    c("Particulate.Matter" = "FALSE")
  )
  
  # Explanation should say 'not significant'
  expect_true(
    grepl("not significantly", actual_explanation_noHTML, fixed = TRUE) |
      grepl("not significant", actual_explanation_noHTML, fixed = TRUE)
  )
})

test_that("Highlighted geography explanation is correct", {
  load(file.path("testing_objects", "Asthma_Dashboard_Birmingham_AL.RData"))
  
  test_dat <- geo_shp_dat$dat
  
  actual <- write_explanation(
    test_dat, 
    cov = "holc_grade_pop", 
    outcome = "CASTHMA_CrudePrev_BRFSS", 
    geo_name = "Birmingham, AL",
    geo_unit = "census tracts", 
    highlighted_geography = "01073000300"
  )
  
  # Extract just the text of the interpretation, all lowercase
  actual_explanation_noHTML <- stringr::str_squish(gsub("<.*?>", "", actual[3])) %>%
    tolower()
  
  expect_identical(
    actual_explanation_noHTML,
    "for census tract 3, jefferson county, alabama, asthma prevalence is 10.8 percent and redlining (holc grade) is 3.522 grade with a rank of 63."
  )
})