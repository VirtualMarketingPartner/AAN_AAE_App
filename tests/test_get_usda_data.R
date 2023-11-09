library(ACAGPM)
library(dplyr)
library(rgeos)
library(sf)
library(stringr)
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

# File name of data downloaded from HUD Comprehensive Housing Affordability Survey
hud_fn <- "Table1 15-19.csv"

# File name of data downloaded from NEPHTN
nephtn_fn <- "Half_Mile_Park_Data.csv"

# File name of data downloaded from USDA Economic Research Services
usda_fn <- "FoodAccessDataProcessed2019.csv"

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

# All tracts PM data
pm_sub_all <-
  read.csv(
    file.path(
      data_folder,
      "Supporting_Files",
      "PM",
      "Census_Tract-County-State-Level_PM2.5_Concentrations_merged_all__2016.csv"
    ),
    colClasses = c(
      "stateFIPS" = "character",
      "ctFIPS" = "character",
      "countyFIPS" = "character",
      "GEOID" = "character"
    )
  ) %>%
  filter(StateAbbr %in% c("AK", "HI")) %>%
  group_by(stateFIPS, State, StateAbbr, Year) %>%
  summarise(State.Value.3Year.Mean_EPA =
              ifelse(
                is.nan(State.Value.3Year.Mean_EPA),
                NA_real_,
                State.Value.3Year.Mean_EPA
              )) %>% ungroup() %>% distinct() %>%
  dplyr::select(stateFIPS, State, State.Value.3Year.Mean_EPA) %>%
  rename(
    c(
      "GEOID" = "stateFIPS",
      "NAME" = "State",
      "Particulate.Matter" = "State.Value.3Year.Mean_EPA"
    )
  ) %>%
  mutate(NAME = str_to_title(NAME))


# AK county PM data
pm_sub_AK <-
  read.csv(
    file.path(
      data_folder,
      "Supporting_Files",
      "PM",
      "Census_Tract-County-State-Level_PM2.5_Concentrations_merged_AK__2016.csv"
    ),
    colClasses = c(
      "stateFIPS" = "character",
      "ctFIPS" = "character",
      "countyFIPS" = "character",
      "stateCountyFIPS" = "character",
      "GEOID" = "character"
    )
  ) %>%
  filter(StateAbbr == "AK") %>%
  group_by(stateFIPS,
           countyFIPS,
           State,
           County,
           stateCountyFIPS,
           StateAbbr,
           Year) %>%
  summarise(County.Value.3Year.Mean_EPA =
              ifelse(
                is.nan(County.Value.3Year.Mean_EPA),
                NA_real_,
                County.Value.3Year.Mean_EPA
              )) %>% ungroup() %>%
  left_join(
    fips_codes %>% dplyr::select(state_code, county_code, county),
    by = c("stateFIPS" = "state_code", "countyFIPS" = "county_code")
  ) %>%
  dplyr::select(StateAbbr,
                stateCountyFIPS,
                county,
                County.Value.3Year.Mean_EPA) %>%
  rename(
    c(
      "state" = "StateAbbr",
      "GEOID" = "stateCountyFIPS",
      "NAMELSAD" = "county",
      "Particulate.Matter" = "County.Value.3Year.Mean_EPA"
    )
  )


# HI county PM data
pm_sub_HI <-
  read.csv(
    file.path(
      data_folder,
      "Supporting_Files",
      "PM",
      "Census_Tract-County-State-Level_PM2.5_Concentrations_merged_HI__2016.csv"
    ),
    colClasses = c(
      "stateFIPS" = "character",
      "ctFIPS" = "character",
      "countyFIPS" = "character",
      "stateCountyFIPS" = "character",
      "GEOID" = "character"
    )
  ) %>%
  filter(StateAbbr == "HI") %>%
  group_by(stateFIPS,
           countyFIPS,
           State,
           County,
           stateCountyFIPS,
           StateAbbr,
           Year) %>%
  summarise(County.Value.3Year.Mean_EPA =
              ifelse(
                is.nan(County.Value.3Year.Mean_EPA),
                NA_real_,
                County.Value.3Year.Mean_EPA
              )) %>% ungroup() %>%
  left_join(
    fips_codes %>% dplyr::select(state_code, county_code, county),
    by = c("stateFIPS" = "state_code", "countyFIPS" = "county_code")
  ) %>%
  dplyr::select(StateAbbr,
                stateCountyFIPS,
                county,
                County.Value.3Year.Mean_EPA) %>%
  rename(
    c(
      "state" = "StateAbbr",
      "GEOID" = "stateCountyFIPS",
      "NAMELSAD" = "county",
      "Particulate.Matter" = "County.Value.3Year.Mean_EPA"
    )
  )


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
  "BPHIGH_CrudePrev_BRFSS",
  "ICE_nonwhite_income",
  "ICE_households",
  "Particulate.Matter",
  "housing_stress",
  "Pop_Half_Mile_Park",
  "low_access_healthy_food"
))

places_tracts <- readRDS(file.path(data_folder, 
                                   "Supporting_Files", 
                                   "Geography", 
                                   "final_places_tracts.RDS"))


# Source functions to be tested
source(file.path("..", "data_scripts", "02_helper_functions.R"), local = TRUE)


# HUD housing stress data
HUD <-
  read.csv(file.path(data_folder,
                     "Supporting_Files",
                     "HUD",
                     hud_fn)) %>%
  mutate(
    housing_stress = (T1_est3 + T1_est76) / (T1_est2 + T1_est75) * 100,
    # Updated in 2015-2019 data
    housing_stress = ifelse(housing_stress > 100, 100, housing_stress)
  ) %>% #cap all percentages max at 100%
  dplyr::select(geoid, housing_stress) %>%
  mutate(GEOID = stringr::str_sub(geoid, -11, -1)) %>%
  mutate_all( ~ ifelse(is.nan(.), NA, .)) %>%
  mutate_all( ~ ifelse(is.infinite(.), NA, .)) %>%
  convert_to_2020_ct(geoid = `GEOID`,
                     variable = `housing_stress`)


# Access to park data
park_data <-
  read.csv(
    file.path(data_folder,
              "Supporting_Files",
              "NEPHT",
              nephtn_fn),
    colClasses = c("GEOID" = "character")
  ) %>%
  mutate(Pop_Half_Mile_Park = as.numeric(str_remove(Pop_Half_Mile_Park, "%"))) %>%
  convert_to_2020_ct(geoid = `GEOID`,
                     variable = `Pop_Half_Mile_Park`)

# Limited access to healthy food data
limited_access <-
  read.csv(
    file.path(data_folder,
              "Supporting_Files",
              "USDA",
              usda_fn),
    colClasses = c("GEOID" = "character")
  ) %>%
  mutate(low_access_healthy_food = LAPOP1_10 / Pop2010 * 100) %>%
  dplyr::select(GEOID, low_access_healthy_food) %>%
  convert_to_2020_ct(geoid = `GEOID`,
                     variable = `low_access_healthy_food`)


test_state_df <- 
  get_nephtn_data(
    data = get_hud_data(
      data = get_pm_data(
        data = get_ice_metric(
          data = get_brfss_data(
            data = get_acs_info(level = "state",
                                acs_year = 2020)$dat,
            level = "state"),
          level = "state"),
        level = "state"),
      level = "state"),
    level = "state")

test_county_df <- 
  get_nephtn_data(
    data = get_hud_data(
      data = get_pm_data(
        data = get_ice_metric(
          data = get_brfss_data(
            data = get_acs_info(level = "county",
                                acs_year = 2020,
                                st = "RI")$dat,
            level = "county",
            st = "RI"),
          level = "county",
          st = "RI"),
        level = "county",
        st = "RI"),
      level = "county"),
    level = "county")


test_tract_df <- 
  get_nephtn_data(
    data = get_hud_data(
      data = get_pm_data(
        data = get_ice_metric(
          data = get_brfss_data(
            data = get_acs_info(level = "tract",
                                acs_year = 2020,
                                st = "RI",
                                county = "Providence County")$dat,
            level = "tract",
            st = "RI",
            county = "Providence County"),
          level = "tract",
          st = "RI",
          county = "Providence County"),
        level = "tract",
        st = "RI",
        county = "Providence County"),
      level = "tract"),
    level = "tract")

test_place_df <- 
  get_nephtn_data(
    data = get_hud_data(
      data = get_pm_data(
        data = get_ice_metric(
          data = get_brfss_data(
            data = get_acs_info(level = "place",
                                acs_year = 2020,
                                st = "RI",
                                place = "Cranston",
                                stateFP = "44",
                                places_tracts = places_tracts)$dat,
            level = "place",
            st = "RI"),
          level = "place",
          st = "RI"),
        level = "place",
        st = "RI"),
      level = "place"),
    level = "place")

test_that("get_usda_data returns dataframe dat with correct fields", {
  # state
  expect_is(
    get_usda_data(data = test_state_df,
                    level = "state"),
    "data.frame"
  )
  expect_identical(
    sort(colnames(get_usda_data(data = test_state_df,
                                  level = "state"))),
    expected_colnames
  )
  
  # county
  expect_is(
    get_usda_data(data = test_county_df,
                    level = "county"),
    "data.frame"
  )
  expect_identical(
    sort(colnames(get_usda_data(data = test_county_df,
                                  level = "county"))),
    expected_colnames
  )
  
  # tract
  expect_is(
    get_usda_data(data = test_tract_df,
                    level = "tract"),
    "data.frame"
  )
  expect_identical(
    sort(colnames(get_usda_data(data = test_tract_df,
                                  level = "tract"))),
    expected_colnames
  )
  
  # place
  expect_is(
    get_usda_data(data = test_place_df,
                    level = "place"),
    "data.frame"
  )
  expect_identical(
    sort(colnames(get_usda_data(data = test_place_df,
                                  level = "place"))),
    expected_colnames
  )
})

test_that("get_usda_data errors when given improper level", {
  expect_error(
    get_usda_data(data = test_state_df,
                    level = "test")
  )
})