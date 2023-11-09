library(dplyr)
library(rgeos)
library(sf)
library(testthat)
library(tidycensus)
library(tigris)

# Environment setup for tests ----

acs_year <- 2021

# File name of data downloaded from CDC's PLACES - County Data (GIS Friendly Format)
places_county_fn <- "PLACES__County_Data__GIS_Friendly_Format___2022_release.csv"

# File name of data downloaded from CDC's PLACES - Census Tract Data (GIS Friendly Format)
places_censustract_fn <- "PLACES__Census_Tract_Data__GIS_Friendly_Format___2021_release.csv"

# File name of data downloaded from CDC's BRFSS - Age-Adjusted Prevalence data
brfss_state_fn <- "Behavioral_Risk_Factor_Surveillance_System__BRFSS__Age-Adjusted_Prevalence_Data__2011_to_2021.csv"

# supporting data -- data folder
data_folder <- file.path(
  # gsub("\\\\", "/", gsub("OneDrive - ", "", Sys.getenv("OneDrive"))),
  getwd(),
  "H",
  "Data",
  "H"
)

# Load 2010 to 2020 Census Tract Relationship files
tract_xw <-
  read.delim(
    file.path(
      data_folder,
      "Supporting_Files",
      "Geography",
      "tab20_tract20_tract10_natl.txt"
    ),
    sep = "|",
    colClasses = "character"
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

# State pull brfss data
brfss_state <-
  read.csv(file.path(data_folder,
                     "Supporting_Files",
                     "BRFSS",
                     brfss_state_fn))


# County pull PLACES brfss data
brfss_county <-
  read.csv(
    file.path(
      data_folder,
      "Supporting_Files",
      "PLACES",
      "County",
      places_county_fn
    ),
    colClasses = c("CountyFIPS" = "character")
  )


# Tract and place pull PLACES brfss data
brfss_ct <-
  read.csv(
    file.path(
      data_folder,
      "Supporting_Files",
      "PLACES",
      "CT",
      places_censustract_fn
    ),
    colClasses = c("CountyFIPS" = "character",
                   "TractFIPS" = "character")
  ) %>%
  right_join(tract_xw, by = c("TractFIPS" = "GEOID_TRACT_10")) %>%
  mutate(AREALAND_TRACT_20 = as.numeric(AREALAND_TRACT_20)) %>%
  group_by(StateAbbr, StateDesc, CountyName, CountyFIPS, GEOID_TRACT_20) %>%
  summarize(
    CASTHMA_CrudePrev = weighted.mean(CASTHMA_CrudePrev, AREALAND_TRACT_20, na.rm = TRUE),
    CSMOKING_CrudePrev = weighted.mean(CSMOKING_CrudePrev, AREALAND_TRACT_20, na.rm = TRUE),
    OBESITY_CrudePrev = weighted.mean(OBESITY_CrudePrev, AREALAND_TRACT_20, na.rm = TRUE),
    COPD_CrudePrev = weighted.mean(COPD_CrudePrev, AREALAND_TRACT_20, na.rm = TRUE),
    ACCESS2_CrudePrev = weighted.mean(ACCESS2_CrudePrev, AREALAND_TRACT_20, na.rm = TRUE),
    BPHIGH_CrudePrev = weighted.mean(BPHIGH_CrudePrev, AREALAND_TRACT_20, na.rm = TRUE)
  ) %>%
  rename("TractFIPS" = "GEOID_TRACT_20") %>%
  na.omit()

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
  "nonwhite_acs",
  "CASTHMA_CrudePrev_BRFSS",
  "CSMOKING_CrudePrev_BRFSS",
  "OBESITY_CrudePrev_BRFSS",
  "COPD_CrudePrev_BRFSS",
  "ACCESS2_CrudePrev_BRFSS",
  "BPHIGH_CrudePrev_BRFSS"
))

places_tracts <- readRDS(file.path(data_folder, 
                                   "Supporting_Files", 
                                   "Geography", 
                                   "final_places_tracts.RDS"))


# Source functions to be tested
source(file.path("..", "data_scripts", "02_helper_functions.R"), local = TRUE)


test_state_df <- 
  get_acs_info(level = "state",
               acs_year = 2020)$dat

test_county_df <- 
  get_acs_info(level = "county",
               acs_year = 2020,
               st = "RI")$dat

test_tract_df <- 
  get_acs_info(level = "tract",
               acs_year = 2020,
               st = "RI",
               county = "Providence County")$dat

test_place_df <- 
  get_acs_info(level = "place",
               acs_year = 2020,
               st = "RI",
               place = "Cranston",
               stateFP = "44",
               places_tracts = places_tracts)$dat


test_that("get_brfss_data returns dataframe dat with correct fields", {
  # state
  expect_is(
    get_brfss_data(data = test_state_df,
                   level = "state"),
    "data.frame"
  )
  expect_identical(
    sort(colnames(get_brfss_data(data = test_state_df,
                                 level = "state"))),
    expected_colnames
  )
  
  # county
  expect_is(
    get_brfss_data(data = test_county_df,
                   level = "county",
                   st = "RI"),
    "data.frame"
  )
  expect_identical(
    sort(colnames(get_brfss_data(data = test_county_df,
                                 level = "county",
                                 st = "RI"))),
    expected_colnames
  )
  
  # tract
  expect_is(
    get_brfss_data(data = test_tract_df,
                   level = "tract",
                   st = "RI",
                   county = "Providence County"),
    "data.frame"
  )
  expect_identical(
    sort(colnames(get_brfss_data(data = test_tract_df,
                                 level = "tract",
                                 st = "RI",
                                 county = "Providence County"))),
    expected_colnames
  )
  
  # place
  expect_is(
    get_brfss_data(data = test_place_df,
                   level = "place",
                   st = "RI"),
    "data.frame"
  )
  expect_identical(
    sort(colnames(get_brfss_data(data = test_place_df,
                                 level = "place",
                                 st = "RI"))),
    expected_colnames
  )
})

test_that("get_brfss_data errors when given improper level", {
  expect_error(
    get_brfss_data(data = test_state_df,
                   level = "test")
  )
})