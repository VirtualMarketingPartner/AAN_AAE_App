# Load Asthma Dashboard Data
# By Hannah De los Santos, Karen Jiang, Julianna Bernardi, AJ Liberatore
# Originated on: 3/15/21

# LOADING DATA ---------------

# FIPS code data
fips_codes <- tidycensus::fips_codes

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

# Load places according to places mapping.
places_tracts <-
  readRDS(
    file.path(
      data_folder,
      "Supporting_Files",
      "Geography",
      "final_places_tracts.RDS"
    )
  )

# Redlining data
holc_dat <-
  read.csv(
    file.path(
      data_folder,
      "Supporting_Files",
      "Redlining",
      "All_Population_Weighted_HOLC_Grades_pt2_Threshold.csv"
    ),
    colClasses = c("GEOID" = "character",
                   "holc_grade_pop" = "numeric")
  ) %>%
  left_join(tract_xw, by = c("GEOID" = "GEOID_TRACT_10")) %>%
  mutate(AREALAND_TRACT_20 = as.numeric(AREALAND_TRACT_20)) %>%
  group_by(GEOID_TRACT_20, city, state)  %>%
  summarize(updated_var = weighted.mean(holc_grade_pop, AREALAND_TRACT_20, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(GEOID_TRACT_20, updated_var, city, state) %>%
  rename(GEOID = GEOID_TRACT_20,
         holc_grade_pop = updated_var)

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

save(
  fips_codes,
  tract_xw,
  brfss_county,
  brfss_ct,
  brfss_state,
  places_tracts,
  holc_dat,
  HUD,
  limited_access,
  park_data,
  pm_sub_AK,
  pm_sub_all,
  pm_sub_HI,
  file = file.path(data_folder, "Supporting_Data", "all_data.RData")
)


# SAVE OVERLAY DATA ---------

# HRSA FQHC Data for map overlays
fqhc_dat <- read.csv(
  file.path(
    data_folder,
    "Supporting_Files",
    "HRSA",
    "Health_Center_Service_Delivery_and_LookAlike_Sites.csv"
  ),
  check.names = FALSE
)

fqhc_dat <- fqhc_dat[, -which(colnames(fqhc_dat) == "")]

save(fqhc_dat,
     file = file.path(data_folder,
                      "Supporting_Data",
                      "fqhc_dat.Rdata"))

# Public School Data for map overlays
public_school_dat <- readxl::read_xlsx(
  file.path(
    data_folder,
    "Supporting_Files",
    "NCES",
    "EDGE_GEOCODE_PUBLICSCH_2122.xlsx"
  )
)

save(
  public_school_dat,
  file = file.path(data_folder,
                   "Supporting_Data",
                   "public_school_dat.Rdata")
)
