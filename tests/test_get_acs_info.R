library(dplyr)
library(rgeos)
library(sf)
library(testthat)
library(tidycensus)
library(tigris)

# Environment setup for tests ----

acs_year <- 2021

# supporting data -- data folder
data_folder <- file.path(
  # gsub("\\\\", "/", gsub("OneDrive - ", "", Sys.getenv("OneDrive"))),
  getwd(),
  "H",
  "Data",
  "H"
)

# supporting category data
dat_info <-
  read.csv(
    file.path(
      data_folder,
      "Supporting_Data",
      "column_name_map.csv"
    )
  )
rownames(dat_info) <- dat_info$Colname

places_tracts <- readRDS(file.path(data_folder, 
                                   "Supporting_Files", 
                                   "Geography", 
                                   "final_places_tracts.RDS"))

expected_colnames <- sort(c(
  "GEOID", 
  "NAME", 
  "unw_centroid_long", 
  "unw_centroid_lat", 
  "pop_acs", 
  "nonhis.pop_acs", 
  "nonhis.white_acs", 
  "nonhis.black_acs", 
  "nonhis.american.indian_acs", 
  "nonhis.asian_acs", 
  "nonhis.pacific.islander_acs", 
  "nonhis.other_acs", 
  "nonhis.two_acs", 
  "hispanic_acs", 
  "pop.education_acs", 
  "median.income_acs", 
  "pop.poverty.level_acs", 
  "pop.income.to.poverty.level_acs", 
  "under.1.income.to.poverty.level_acs", 
  "one.to.1.99.income.to.poverty.level_acs", 
  "unemp.rate.16.over", 
  "under.2.income.to.poverty.level_acs", 
  "high.school.or.less_acs", 
  "some.college.or.greater_acs", 
  "labor.force.part.rate", 
  "nonwhite_acs"
))


# Source functions to be tested
source(file.path("..", "data_scripts", "02_helper_functions.R"), local = TRUE)

#trycatch due to api pulls? tigris/tidycensus. test passed due to connection issue message
#warnings for improper input
#if something is subject to change, send message for things that changed


test_that("get_acs_info returns list with SpatialPolygonsDataFrame shp and data.frame dat", {
  # state
  expect_is(
    get_acs_info(level = "state",
                 acs_year = 2020),
    "list"
  )
  expect_length(
    get_acs_info(level = "state",
                 acs_year = 2020),
    2
  )
  expect_identical(
    names(get_acs_info(level = "state",
                 acs_year = 2020)),
    c("shp", "dat")
  )
  expect_is(
    get_acs_info(level = "state",
                 acs_year = 2020)$shp,
    "SpatialPolygonsDataFrame"
  )
  expect_is(
    get_acs_info(level = "state",
                 acs_year = 2020)$dat,
    "data.frame"
  )
  expect_identical(
    sort(colnames(get_acs_info(level = "state",
                               acs_year = 2020)$dat)),
    expected_colnames
  )
  
  # county
  expect_is(
    get_acs_info(level = "county",
                 acs_year = 2020,
                 st = "RI"),
    "list"
  )
  expect_length(
    get_acs_info(level = "county",
                 acs_year = 2020,
                 st = "RI"),
    2
  )
  expect_identical(
    names(get_acs_info(level = "county",
                       acs_year = 2020,
                       st = "RI")),
    c("shp", "dat")
  )
  expect_is(
    get_acs_info(level = "county",
                 acs_year = 2020,
                 st = "RI")$shp,
    "SpatialPolygonsDataFrame"
  )
  expect_is(
    get_acs_info(level = "county",
                 acs_year = 2020,
                 st = "RI")$dat,
    "data.frame"
  )
  expect_identical(
    sort(colnames(get_acs_info(level = "county",
                               acs_year = 2020,
                               st = "RI")$dat)),
    expected_colnames
  )
  
  # tract
  expect_is(
    get_acs_info(level = "tract",
                 acs_year = 2020,
                 st = "RI",
                 county = "Providence County"),
    "list"
  )
  expect_length(
    get_acs_info(level = "tract",
                 acs_year = 2020,
                 st = "RI",
                 county = "Providence County"),
    2
  )
  expect_identical(
    names(get_acs_info(level = "tract",
                       acs_year = 2020,
                       st = "RI",
                       county = "Providence County")),
    c("shp", "dat")
  )
  expect_is(
    get_acs_info(level = "tract",
                 acs_year = 2020,
                 st = "RI",
                 county = "Providence County")$shp,
    "SpatialPolygonsDataFrame"
  )
  expect_is(
    get_acs_info(level = "tract",
                 acs_year = 2020,
                 st = "RI",
                 county = "Providence County")$dat,
    "data.frame"
  )
  expect_identical(
    sort(colnames(get_acs_info(level = "tract",
                               acs_year = 2020,
                               st = "RI",
                               county = "Providence County")$dat)),
    expected_colnames
  )
  
  # place
  expect_is(
    get_acs_info(level = "place",
                 acs_year = 2020,
                 st = "RI",
                 place = "Cranston",
                 stateFP = "44",
                 places_tracts = places_tracts),
    "list"
  )
  expect_length(
    get_acs_info(level = "place",
                 acs_year = 2020,
                 st = "RI",
                 place = "Cranston",
                 stateFP = "44",
                 places_tracts = places_tracts),
    2
  )
  expect_identical(
    names(get_acs_info(level = "place",
                       acs_year = 2020,
                       st = "RI",
                       place = "Cranston",
                       stateFP = "44",
                       places_tracts = places_tracts)),
    c("shp", "dat")
  )
  expect_is(
    get_acs_info(level = "place",
                 acs_year = 2020,
                 st = "RI",
                 place = "Cranston",
                 stateFP = "44",
                 places_tracts = places_tracts)$shp,
    "SpatialPolygonsDataFrame"
  )
  expect_is(
    get_acs_info(level = "place",
                 acs_year = 2020,
                 st = "RI",
                 place = "Cranston",
                 stateFP = "44",
                 places_tracts = places_tracts)$dat,
    "data.frame"
  )
  expect_identical(
    sort(colnames(get_acs_info(level = "place",
                               acs_year = 2020,
                               st = "RI",
                               place = "Cranston",
                               stateFP = "44",
                               places_tracts = places_tracts)$dat)),
    expected_colnames
  )
})

test_that("get_acs_info errors when given improper level", {
  expect_error(
    get_acs_info(level = "test",
                 acs_year = 2020)
  )
})

test_that("get_acs_info errors when given improper year", {
  expect_error(
    get_acs_info(level = "state",
                 acs_year = 0)
  )
})

test_that("get_acs_info errors when no places_tracts file supplied", {
  expect_error(
    get_acs_info(level = "place",
                 acs_year = 2020,
                 st = "RI",
                 place = "Cranston",
                 stateFP = "44",
                 places_tracts = NULL)
  )
})

test_that("get_acs_info errors when improper stateFP supplied", {
  expect_error(
    get_acs_info(level = "place",
                 acs_year = 2020,
                 st = "RI",
                 place = "Cranston",
                 stateFP = "11",
                 places_tracts = places_tracts)
  )
})