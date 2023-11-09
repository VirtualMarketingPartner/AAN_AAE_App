# Process and Save Asthma Dashboard Data
# By Hannah De los Santos, Karen Jiang, Julianna Bernardi, AJ Liberatore
# Originated on: 3/15/21

## LOAD DATA AND INITIALIZE DIRECTORIES ----------------------------------------

tic()

load(file.path(data_folder, "Supporting_Data", "all_data.RData"))

# Create directory to store these files into
# dir.create(file.path(rstudioapi::getActiveProject(), "data"))
# dir.create(file.path(rstudioapi::getActiveProject(), "data", "county"))
# dir.create(file.path(rstudioapi::getActiveProject(), "data", "tract"))
# dir.create(file.path(rstudioapi::getActiveProject(), "data", "place"))

dir.create(file.path(data_folder, "county"))
dir.create(file.path(data_folder, "tract"))
dir.create(file.path(data_folder, "place"))

## MERGE DATA ------------------------------------------------------------------


# Get county data
print(paste0("Working on county, tract, and place data...:", Sys.time()))


# Create list of states to pull in abbreviated format
states <- unique(fips_codes %>%
                   filter(!(
                     state %in% c("AS", "GU", "MP", "PR", "UM", "VI")
                   )) %>%
                   pull(state))

# set up parallel backend to use many processors
cores <- detectCores()
clust <- makeCluster(cores)
clusterExport(clust, 
              varlist = c("data_folder", "acs_year"), 
              envir = .GlobalEnv)

parallel::clusterEvalQ(clust, {
  library(ACAGPM)
  library(broom)
  library(dplyr)
  library(raster)
  library(rgdal)
  library(rgeos)
  library(sf)
  library(stringr)
  library(tidycensus)
  
  source(here::here("data_scripts/02_helper_functions.R"))
  
  load(file.path(data_folder, "Supporting_Data", "all_data.RData"))
})
toc()

start <- Sys.time()

# Iterate through states
for (state in states) {
  # state = 'CT'
  print(paste0(state, "..."))
  tic()
  
  # Save file as Asthma_Dashboard_[state abbreviation].RData and name it geo_shp_dat
  county_path <- file.path(data_folder,
                           "county",
                           paste0("Asthma_Dashboard_", state, ".RData"))
  
  if (!file.exists(county_path)) {
    # Get the county level data
    this_state <-
      get_merged_shp(geo_level = "county",
                     st = state,
                     acs_year = 2020)
    geo_shp_dat <- list(dat = this_state$dat, shp = this_state$shp)
    
    
    # Save to specified directory
    save(geo_shp_dat,
         file = county_path)
    print(paste0(state, " data saved to ", county_path, "/...", Sys.time()))
    
    toc()
  }
  
  
  # Get county level data (all tracts in county)
  counties <- fips_codes %>%
    filter(state == !!state)

  print(paste0("Working on ", state, " tract data...:", Sys.time()))
  
  # Iterate through counties
  # for (county in counties$county)
  
  save_county_rds <- function(county, state) {
    print(paste0(county, ", ", state, "..."))

        # Skip over specific counties in FIPS code list, that do not exist in ACS
    if (state == "VA" && county %in% c("Bedford city",
                                       "Clifton Forge city",
                                       "South Boston city")) {
      return()
    } else if (state == "SD" && county == "Shannon County") {
      return()
    } else if (state == "AK" &&
               county %in% c(
                 "Wade Hampton Census Area",
                 "Prince of Wales-Outer Ketchikan Census Area",
                 "Skagway-Yakutat-Angoon Census Area",
                 "Skagway-Hoonah-Angoon Census Area",
                 "Valdez-Cordova Census Area",
                 "Wrangell-Petersburg Census Area"
               )) {
      return()
    } else if (state == "DE" && county == "Middletown") {
      return()
    } else if (state == "FL" && county == "Dade County") {
      return()
    } else if (state == "MT" &&
               county == "Yellowstone National Park") {
      return()
    } else if (state == "NM" && county == "Dona Ana County") {
      return()
    }
####
    else if (state == "CT" && county %in%  c("Capitol",
                                             "Greater Bridgeport",
                                             "Lower Connecticut River Valley",
                                             "Naugatuck Valley",
                                             "Northeastern Connecticut"
                                             )){
      return()
    }
    else if (state == "CT"){
      return()
    }
####    
    
    
    # Save file as Asthma_Dashboard_[County_name]_[State abbr] and name it geo_shp_dat
    tract_path <- file.path(data_folder,
                            "tract")
    
    # Skip over files if they already exist
    if (!file.exists(file.path(
      tract_path,
      paste0("Asthma_Dashboard_", county, "_", state, ".RData")
    ))) {
      # Get the tract level data
      this_county <-
        get_merged_shp(
          geo_level = "tract",
          acs_year = acs_year,
          st = state,
          county = county
        )
      geo_shp_dat <-
        list(dat = this_county$dat, shp = this_county$shp)
      
      # Save to specified directory
      save(geo_shp_dat,
           file = file.path(
             tract_path,
             paste0("Asthma_Dashboard_", county, "_", state, ".RData")
           ))
      
      
      
    }
  }
  
  tic()
  clusterExport(clust, varlist = c("state"))
  parLapply(counties$county,
            fun = save_county_rds,
            state = state,
            cl = clust)
  # lapply(counties$county,save_county_rds,state = state)
  toc()
  
  print(paste0("All ", state, " tract data saved", Sys.time()))
  
  # Get place data
  stateFP <- fips_codes %>%
    dplyr::select(state, state_code) %>%
    distinct() %>%
    filter(state == .env$state) %>%
    pull(state_code)
  
  # Pulls places by name
  places <- places_tracts %>%
    filter(STATEFP == stateFP) %>%
    pull(NAME) %>%
    unique()
  
  # Make sure there's at least one place in this state
  if (length(places) != 0) {
    print(paste0("Working on ", state, " place data...:", Sys.time()))
    
    # Iterate throught places
    
    save_place_rds <- function(place, stateFP, places_tracts) {
      print(paste0(place, ", ", state, "..."))
      
      
      if (state == "DE" && place == "Middletown") {
        return()
      } else if (state == "ID" && place == "Meridian") {
        return()
      } else if (state == "KY" &&
                 place == "Louisville/Jefferson County metro government (balance)") {
        return()
      }
      
      place_path <- file.path(data_folder,
                              "place")
      
      # # Rename place due to issues with saving the file
      # if (place == "Louisville/Jefferson County metro government (balance)"){
      #   place <- "Louisville-Jefferson County metro government (balance)"
      # }
      #
      if (!file.exists(file.path(
        place_path,
        paste0("Asthma_Dashboard_", place, "_", state, ".RData")
      ))) {
        # Get the tract level data
        this_place <-
          get_merged_shp(
            geo_level = "place",
            st = state,
            place = place,
            acs_year = acs_year,
            stateFP = stateFP,
            places_tracts = places_tracts
          )
        geo_shp_dat <-
          list(dat = this_place$dat, shp = this_place$shp)
        
        # Save file as Asthma_Dashboard_[place name]_[state abbreviation].RData and name it geo_shp_dat
        
        
        # Save to specified directory
        save(geo_shp_dat,
             file = file.path(
               place_path,
               paste0("Asthma_Dashboard_", place, "_", state, ".RData")
             ))
      }
      
    }
    
    
    tic()
    clusterExport(clust, varlist = c("stateFP", "places_tracts"))
    parLapply(
      places,
      fun = save_place_rds,
      stateFP = stateFP,
      places_tracts = places_tracts,
      cl = clust
    )
    toc()
    
    print(paste0("All ", state, " place data saved", Sys.time()))
    
    print(Sys.time() - start)
    
  }
}

stopCluster(clust)
end <- Sys.time()
print("Finished!")
print(end - start)

