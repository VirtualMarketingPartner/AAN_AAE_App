# Helper Functions 
# By Hannah De los Santos, Karen Jiang, Julianna Bernardi, AJ Liberatore
# Originated on: 3/15/21

## SUPPORTING FUNCTIONS --------------------------------------------------------

#' Helper function to get the acs data for variables of interest and geographies
#'
#' @param level, string representing level of pull
#' @param st, string representing state if level is county, tract, or place
#' @param county, string representing county if level is tract
#' @param place, string representing place if level is place
#'
#' @return List containing data.frame of acs data and SpatialPolygonsDataFrame 
#' of geography
get_acs_info <- function(level = c("state", "county", "tract", "place"),
                         acs_year,
                         st = NULL, county = NULL, place = NULL, stateFP = NULL, places_tracts = NULL){
  
  # Specify arguments
  level <- match.arg(level)
  
  # Specify variables of interest
  var_interest <- c(
    
    # Population Composition
    "B03002_001E" = "pop_acs", # Estimate!!Total
    "B03002_002E" = "nonhis.pop_acs", # Estimate!!Total!!Not Hispanic or Latino
    "B03002_003E" = "nonhis.white_acs", # Estimate!!Total!!Not Hispanic or Latino!!White alone
    "B03002_004E" = "nonhis.black_acs", # 	Estimate!!Total!!Not Hispanic or Latino!!Black or African American alone
    "B03002_005E" = "nonhis.american.indian_acs", # Estimate!!Total!!Not Hispanic or Latino!!American Indian and Alaska Native alone
    "B03002_006E" = "nonhis.asian_acs", # Estimate!!Total!!Not Hispanic or Latino!!Asian alone
    "B03002_007E" = "nonhis.pacific.islander_acs", # Estimate!!Total!!Not Hispanic or Latino!!Native Hawaiian and Other Pacific Islander alone
    "B03002_008E" = "nonhis.other_acs", # 	Estimate!!Total!!Not Hispanic or Latino!!Some Other Race alone
    "B03002_009E" = "nonhis.two_acs", # 	Estimate!!Total!!Not Hispanic or Latino!!Two or more races
    "B03002_012E" = "hispanic_acs", # Estimate!!Total!!Hispanic or Latino
    
    # Labor and Employment
    "S2301_C04_001E" = "unemp.rate.16.over", # Estimate!!Unemployment rate!!Population 16 years and over
    "C18120_002E" = "labor.force", # Labor force
    "C18120_001E" = "total.labor.force", # total for labor force calculation
    
    # Education Measures
    "B06009_001E" = "pop.education_acs", # Estimate!!Total
    "B06009_002E" = "less.than.high.school_acs",	# Estimate!!Total!!Less than high school graduate
    "B06009_003E" = "high.school_acs",	# Estimate!!Total!!High school graduate (includes equivalency)
    "B06009_004E" = "some.college_acs",	# Estimate!!Total!!Some college or associate's degree
    "B06009_005E" = "bachelors_acs",	# Estimate!!Total!!Bachelor's degree
    "B06009_006E" = "graduate.degree_acs",	# Estimate!!Total!!Graduate or professional degree
    
    # Median household Income
    "B06011_001E" = "median.income_acs",	# Estimate!!Median income in the past 12 months!!Total
    
    # Federal Poverty Level Measures
    "B06012_001E" = "pop.poverty.level_acs",
    
    # Ratio of Income to Poverty
    "B05010_001E" = "pop.income.to.poverty.level_acs", # Estimate!!Total
    "B05010_002E" = "under.1.income.to.poverty.level_acs", # Estimate!!Total:!!Under 1.00: 
    "B05010_010E" = "one.to.1.99.income.to.poverty.level_acs" # Estimate!!Total:!!1.00 to 1.99: 
  )
  
  # Pull data via tidycensus
  acsinfo <-
    if (level == "state"){
      get_acs(
        geography = "state",  
        year = acs_year, geometry = T, variables = names(var_interest), output = "wide"
      )
    } else if (level == "county"){
      get_acs(
        geography = "county",  state = st,  
        year = acs_year, geometry = T, variables = names(var_interest), output = "wide"
      )
    } else if (level == "tract"){
      get_acs(
        geography = "tract",  state = st, county = county, 
        year = acs_year, geometry = T, variables = names(var_interest), output = "wide"
      )
    } else if (level == "place"){
      get_acs(
        geography = "tract",  state = st, 
        year = acs_year, geometry = T, variables = names(var_interest), output = "wide"
      )
    } else{
      stop("Improper level. Level must be state, county, tract, or place.")
    }
  
  # Filter to tracts in specified place
  acsinfo <-
    if (level == "place"){
      acsinfo %>%
        filter(GEOID %in% (places_tracts %>% 
                             filter(STATEFP == stateFP & NAME == place) 
                           %>% pull(GEOID)
                           ))
    } else{
      acsinfo
    }
  
  # Separate geography from data
  init_ct <- acsinfo %>%
    dplyr::select(GEOID, NAME, geometry)
  
  # Set data type to SpatialPolygonsDataFrame
  geo_shp <- as(
    init_ct[!st_is_empty(init_ct),],
    "Spatial"
  )
  
  # calculate centroids
  unw_centr_pt <- gCentroid(geo_shp[,], byid = T)
  
  # add information to the overall merge dataframe
  # note: geo id is specific to the level
  merged_dat <- data.frame(
    "GEOID" = geo_shp@data$GEOID,
    "NAME" = geo_shp@data$NAME,
    "unw_centroid_long" = unw_centr_pt@coords[,1],
    "unw_centroid_lat" = unw_centr_pt@coords[,2]
  )
  rownames(merged_dat) <- merged_dat$GEOID
  
  # Drop geometry and convert to dataframe object
  acsinfo <- as.data.frame(acsinfo %>% st_drop_geometry())
  
  # remove all the modifier columns (not interested for now)
  acsinfo <- acsinfo[, !grepl("M", colnames(acsinfo))] %>%
    rename(setNames(names(var_interest), var_interest))
  rownames(acsinfo) <- acsinfo$GEOID
  
  # Create below 200% the poverty level
  acsinfo <- acsinfo %>%
    mutate(
      under.2.income.to.poverty.level_acs = 
        under.1.income.to.poverty.level_acs + 
        `one.to.1.99.income.to.poverty.level_acs`
    )
  
  # Convert to percents
  acsinfo <- acsinfo %>%
    mutate(
      # Calculate education variables and convert to percents
      high.school.or.less_acs = (less.than.high.school_acs + high.school_acs) /
        pop.education_acs * 100,
      some.college.or.greater_acs = (some.college_acs + bachelors_acs + 
                                       graduate.degree_acs) / 
        pop.education_acs * 100,
      
      # Convert income to poverty variables to percents
      under.1.income.to.poverty.level_acs = under.1.income.to.poverty.level_acs / 
        pop.income.to.poverty.level_acs * 100,
      one.to.1.99.income.to.poverty.level_acs = one.to.1.99.income.to.poverty.level_acs / 
        pop.income.to.poverty.level_acs * 100,
      under.2.income.to.poverty.level_acs = under.2.income.to.poverty.level_acs /
        pop.income.to.poverty.level_acs * 100,
      
      # Convert labor force to a percent
      labor.force.part.rate = labor.force/total.labor.force*100,
      
      # Nonwhite calculation
      nonwhite_acs = pop_acs - nonhis.white_acs
    ) %>%
    dplyr::select(
      # Drop unnecessary variables
      -labor.force, 
      -total.labor.force, 
      -less.than.high.school_acs, 
      -high.school_acs, 
      -some.college_acs, 
      -bachelors_acs, 
      -graduate.degree_acs
    )
  
  
  # add to full dataframe
  merged_dat <- cbind(merged_dat, acsinfo[as.character(merged_dat$GEOID),-1])
  
  return(list("shp" = geo_shp, "dat" = merged_dat))
}


#' Helper function to pull brfss/places data for health indicators
#' 
#' @param data, data.frame object containing data for geography
#' @param level, string representing level of pull
#' @param st, string representing state if level is county, tract, or place
#' @param county, string representing county if level is tract
#'
#' @return Input data.frame joined with brfss data
get_brfss_data <- function (data, level = c("state", "county", "tract", "place"),
                            st = NULL, county = NULL){
  
  # Specify arguments
  level <- match.arg(level)
  
  # Utilize different data depending on geographic level
  if (level == "state"){
    
    # map topic to future column name
    # note -- must look at response
    var_interest <- c(
      "Asthma" = "CASTHMA", # asthma
      "Current Smoker Status" = "CSMOKING", # current smoking
      "BMI Categories" = "OBESITY", # obesity
      "COPD" = "COPD", # COPD
      "Health Care Coverage" = "ACCESS2", # health care access
      "High Blood Pressure" = "BPHIGH" # high blood pressure
    )
    
    # subset to the topics we care about
    b_sub <- brfss_state[brfss_state$Topic %in% names(var_interest),]
    b_sub$Response <- tolower(b_sub$Response)
    
    # subset to responses we are concerned with
    correct_ans <- c(
      "Asthma" = "yes", # asthma prevalence
      "Current Smoker Status" = "yes", # current smoking
      "BMI Categories" = "obese (bmi 30.0 - 99.8)", # obesity
      "COPD" = "yes", # COPD
      "Health Care Coverage" = "no", # health care access
      "High Blood Pressure" = "yes" # high blood pressure
    )
    
    # latest year available
    year_spec <- 2020
    
    # preallocate final result
    b_int <- data.frame(matrix(NA, nrow(data), ncol = length(var_interest)))
    colnames(b_int) <- c(
      paste0(var_interest, "_CrudePrev_BRFSS")
    )
    rownames(b_int) <- data$NAME
    
    # go through each topic and add it
    for (v in 1:length(correct_ans)){
      bi_sub <- b_sub[
        b_sub$Topic == names(correct_ans)[v] & 
          b_sub$Response == correct_ans[v] &
          b_sub$Year == year_spec,]
      
      b_int[bi_sub$Locationdesc, 
            paste0(var_interest[names(correct_ans)[v]], "_CrudePrev_BRFSS")] <-
        bi_sub$Data_value
    }
    
    # add to the final data frame
    data <- cbind(data, b_int[data$NAME,])
    
  } else if (level == "county")
    {
    
    # Subset to specific state
    cnty_sub <- brfss_county[brfss_county$StateAbbr==st,]
    
    # Assures that specified state has county level data
    if (nrow(cnty_sub) > 0){
      # we're only going to pull the variables of interest
      var_interest <- c(
        "CASTHMA", # asthma
        "CSMOKING", # current smoking
        "OBESITY", # obesity
        "COPD", # COPD
        "ACCESS2", # insurance
        "BPHIGH" # high blood pressure
      )
      
      # subset to columns of interest
      cnty_int <- cnty_sub[,grepl(paste(var_interest, collapse = "|"), 
                                  colnames(cnty_sub))] %>% as.data.frame()
      colnames(cnty_int) <- paste0(colnames(cnty_int), "_BRFSS")
      rownames(cnty_int) <- cnty_sub$CountyFIPS
      
      # do not include the 95CI or AdjPrev
      cnty_int <- cnty_int[, !grepl("95CI", colnames(cnty_int))]
      cnty_int <- cnty_int[, !grepl("AdjPrev", colnames(cnty_int))]
      
      # add to the final data frame
      data <- cbind(data, cnty_int[data$GEOID,])
      
    }
    
  } else if (level == "tract")
    {
    
    # Creates county GEOID field
    countyfip <- fips_codes %>%
      filter(state == !!st, county == !!county) %>%
      tidyr::unite(col = "CountyFIPS", c(state_code, county_code), sep = "") %>%
      pull(CountyFIPS)
    
    # subset to our specific county
    ct_sub <- brfss_ct[brfss_ct$StateAbbr == st & 
                           brfss_ct$CountyFIPS == countyfip,]
    
    # Assure that specified county has tract level data
    if (nrow(ct_sub) > 0){
      # we're only going to pull the variables of interest
      var_interest <- c(
        "CASTHMA", # asthma
        "CSMOKING", # current smoking
        "OBESITY", # obesity
        "COPD", # COPD
        "ACCESS2", # insurance
        "BPHIGH" # high blood pressure
      )
      
      # subset to columns of interest
      cntyCT_int <- ct_sub[,grepl(paste(var_interest, collapse = "|"), 
                                  colnames(ct_sub))] %>% as.data.frame()
      colnames(cntyCT_int) <- paste0(colnames(cntyCT_int), "_BRFSS")
      rownames(cntyCT_int) <- ct_sub$TractFIPS
      
      # do not include the 95CI
      cntyCT_int <- cntyCT_int[, !grepl("95CI", colnames(cntyCT_int))]
      
      # add to the final data frame
      data <- cbind(data, cntyCT_int[data$GEOID,])
      
    }
    
  } else if (level == "place")
    {
    
    # subset to our specific place
    brfss_ct <- brfss_ct[brfss_ct$StateAbbr == st & brfss_ct$TractFIPS %in% data$GEOID,]
    
    # summarize all values by simple mean for census tracts that span across multiple places
    brfss_ct <- brfss_ct %>% 
      group_by(StateAbbr, StateDesc, TractFIPS) %>%
      summarise_if(is.numeric, mean)
    
    # Assure that specified place has tract level data
    if (nrow(brfss_ct) > 0){
      # we're only going to pull the variables of interest
      var_interest <- c(
        "CASTHMA", # asthma
        "CSMOKING", # current smoking
        "OBESITY", # obesity
        "COPD", # COPD
        "ACCESS2", # insurance
        "BPHIGH" # high blood pressure
      )
      
      # subset to columns of interest
      b_int <- brfss_ct[,grepl(paste(var_interest, collapse = "|"), 
                               colnames(brfss_ct))] %>% as.data.frame()
      colnames(b_int) <- paste0(colnames(b_int), "_BRFSS")
      rownames(b_int) <- brfss_ct$TractFIPS
      
      # do not include the 95CI
      b_int <- b_int[, !grepl("95CI", colnames(b_int))]
      
      # add to the final data frame
      data <- cbind(data, b_int[data$GEOID,])
      
    }
    
  } else{
    stop("Improper level. Level must be state, county, tract, or place.")
  }
  
  return(data)
}


#' Helper function to get ICE metrics for nonwhite v income, using ACS estimates
#' 
#' @param data, data.frame object containing data for geography
#' @param level, string representing level of pull
#' @param st, string representing state if level is county, tract, or place
#' @param county, string representing county if level is tract
#'
#' @return Input data.frame joined with ice data
get_ice_metric <- function(data, level = c("state", "county", "tract", "place"),
                           st = NULL, county = NULL){
  
  # Specify arguments
  level <- match.arg(level)
  
  # Specify ICE variables
  var_interest <- c(
    # All, Total + Less than $25k
    "B19001_001",
    "B19001_002",
    "B19001_003",
    "B19001_004",
    "B19001_005", 
    
    # White, Less than $25k
    "B19001H_002",
    "B19001H_003",
    "B19001H_004",
    "B19001H_005",
    
    # White, Greater than $125k
    "B19001H_015",
    "B19001H_016",
    "B19001H_017",
    
    # Rent/Own ICE,
    "B25003_001E", #= "occupied.housing.units_acs",
    "B25003_002E", #= "own.occupied.housing.units_acs", 
    "B25003_003E" #= "rent.occupied.housing.units_acs"
    
  )
  
  # Pull data via tidycensus
  geo_info <- 
    if (level == "state"){
      get_acs(
        geography = "state",  
        year = acs_year, geometry = F, variables = var_interest, output = "wide"
      )
      
    } else if (level == "county"){
      get_acs(
        geography = "county",  state = st,  
        year = acs_year, geometry = F, variables = var_interest, output = "wide"
      )
      
    } else if (level == "tract"){
      get_acs(
        geography = "tract",  state = st, county = county, 
        year = acs_year, geometry = F, variables = var_interest, output = "wide"
      )
      
    } else if (level == "place"){
      get_acs(
        geography = "tract",  state = st,
        year = acs_year, geometry = F, variables = var_interest, output = "wide"
      )
      
    } else{
      stop("Improper level. Level must be state, county, tract, or place.")
    }
  
  # keep only the estimates for now
  geo_info <- as.data.frame(geo_info)
  geo_info <- cbind(
    geo_info[,c(1:2)],
    geo_info[,-c(1:2)][, !grepl("M", colnames(geo_info)[-c(1:2)])]
  )
  
  # remove the "E" at the end, for ease
  colnames(geo_info)[-c(1:2)] <- gsub("E", "", colnames(geo_info)[-c(1:2)])
  
  # Calculate desired fields from pulled data
  geo_info$high_inc_white <- with(geo_info, B19001H_015 + B19001H_016 + 
                                    B19001H_017)
  geo_info$low_inc_nonwhite <- 
    with(geo_info,
         (B19001_002 + B19001_003 + B19001_004 + B19001_005) 
         - (B19001H_002 + B19001H_003 + B19001H_004 + B19001H_005)
    )
  geo_info$income_ICE <-
    with(geo_info, (high_inc_white - low_inc_nonwhite) / B19001_001)
  geo_info$home_ICE <- 
    with(geo_info, (B25003_002 - B25003_003) / B25003_001)
  
  # Add new fields to dataframe
  geo_info <- as.data.frame(geo_info)
  rownames(geo_info) <- geo_info$GEOID
  data <- cbind(data, "ICE_nonwhite_income" = geo_info[as.character(data$GEOID),
                                                       "income_ICE"],
                      "ICE_households" = geo_info[as.character(data$GEOID),
                                                  "home_ICE"])
  
  return(data)
}


#' Helper function that rescales ICE values to be between 0 & 100
#'
#' @param data, data.frame object containing data for geography
#'
#' @return Input data.frame with rescaled ice data
rescale_ice <- function(data){
  # Rescale ICE metric to be bounded by 0 and 100
  new_data <- data %>% 
    mutate_at(vars(contains("ICE")), ~(100*(. + 1))/2)
  
  return(new_data)
}


#' Helper function to pull pm2.5 data
#' 
#' @param data, data.frame object containing data for geography
#' @param level, string representing level of pull
#' @param st, string representing state if level is county, tract, or place
#' @param county, string representing county if level is tract
#'
#' @return Input data.frame joined with pm2.5 data
get_pm_data <- function(data, level = c("state", "county", "tract", "place"),
                          st = NULL, county = NULL){
  
  # Specify arguments
  level <- match.arg(level)
  
  # Rename counties to be compatible with ACAGPM
  if (!is.null(st) && !is.null(county)){
    if (st == "NM" && county == "Dona Ana County") {
      county <- 'Do?a Ana County'
    } else if (st == "LA" && county == "La Salle Parish"){
      county <- "LaSalle Parish"
    }
  }
  
  # Pull data via ACAGPM; EPA data for AK and HI
  geo_info <-
    if (level == "state") {
      pull_state_ACAG(pull_type = "Internal", year = 2018, level = "National") %>%
        bind_rows(pm_sub_all)
      
    } else if (level == "county") {
      if (st == "AK") {
        pm_sub_AK
      } else if (st == "HI"){
        pm_sub_HI
      } else{
        pull_county_ACAG(pull_type = "Internal", year = 2018, level = "State", 
                         state = lookup_GEOID(state = st))
      }
      
    } else if (level == "tract") {
      if (st %in% c("AK", "HI")) {
        data.frame(county_state = character(),
                   GEOID = character(),
                   NAMELSAD = character(),
                   Particulate.Matter = double())
      } else{
        pull_tract_ACAG(pull_type = "Internal", year = 2018, level = "County", 
                        county_state = lookup_GEOID(county = paste0(county, ", ", st))) %>%
          convert_to_2020_ct(geoid = `GEOID`, variable = `Particulate.Matter`) %>%
          filter(!is.nan(Particulate.Matter))
      }
        
    } else if (level == "place") {
      if (st %in% c("AK", "HI")) {
        data.frame(county_state = character(),
                   GEOID = character(),
                   NAMELSAD = character(),
                   Particulate.Matter = double())
      } else{
        pull_tract_ACAG(pull_type = "Internal", year = 2018, level = "State", 
                        lookup_GEOID(state = st)) %>%
          convert_to_2020_ct(geoid = GEOID, variable = Particulate.Matter) %>%
          filter(!is.nan(Particulate.Matter))
      }
      
    } else {
      stop("Improper level. Level must be state, county, tract, or place.")
    }
  
  # Add new field to dataframe
  geo_info <- geo_info[geo_info$GEOID %in% data$GEOID,]
  rownames(data) <- data$GEOID
  data[geo_info$GEOID, "Particulate.Matter"] <- geo_info$Particulate.Matter
  
  return(data) 
}


#' Helper function to pull hud housing stress data
#' 
#' @param data, data.frame object containing data for geography
#' @param level, string representing level of pull
#'
#' @return Input data.frame joined with hud data
get_hud_data <- function(data, level = c("state", "county", "tract", "place")){
  
  # Specify arguments
  level <- match.arg(level)
  
  # Pull data via HUD table
  geo_info <-
    if (level == "state"){
      # Filter and summarise by selected states
      HUD %>% group_by(GEOID = substr(GEOID, 1, 2)) %>%
        summarise(housing_stress = mean(housing_stress, na.rm = T))
      
    } else if (level == "county"){
      # Filter and summarise by selected counties
      HUD %>% group_by(GEOID = substr(GEOID, 1, 5)) %>%
        summarise(housing_stress = mean(housing_stress, na.rm = T))
      
    } else if (level == "tract" || level == "place") {
      # Filter by selected tracts
      HUD
      
    } else {
      stop("Improper level. Level must be state, county, tract, or place.")
    }
  
  # Add new field to dataframe
  geo_info <- geo_info[geo_info$GEOID %in% data$GEOID,]
  rownames(data) <- data$GEOID
  data[geo_info$GEOID, "housing_stress"] <- geo_info$housing_stress
  
  return(data)
}


#' Helper function to pull nephtn park access data
#' 
#' @param data, data.frame object containing data for geography
#' @param level, string representing level of pull
#'
#' @return Input data.frame joined with nephtn data
get_nephtn_data <- function(data, level = c("state", "county", "tract", "place")) {
  
  # Specify arguments
  level <- match.arg(level)
  
  # Pull data via NEPHTN table
  geo_info <-
    if (level == "state"){
      # Aggregate to state level
      park_data %>% group_by(GEOID = substr(GEOID, 1, 2)) %>%
        summarise(Pop_Half_Mile_Park = mean(Pop_Half_Mile_Park, na.rm = T))
      
    } else if (level == "county"){
      # Aggregate to county level
      park_data %>% group_by(GEOID = substr(GEOID, 1, 5)) %>%
        summarise(Pop_Half_Mile_Park = mean(Pop_Half_Mile_Park, na.rm = T))
      
    } else if (level == "tract" || level == "place") {
      # Filter by selected tracts
      park_data
      
    } else {
      stop("Improper level. Level must be state, county, tract, or place.")
    }
  
  # Add new field to dataframe
  geo_info <- geo_info[geo_info$GEOID %in% data$GEOID,]
  rownames(data) <- data$GEOID
  data[geo_info$GEOID, "Pop_Half_Mile_Park"] <- geo_info$Pop_Half_Mile_Park
  
  return(data)
}


#' Helper function to pull usda healthy food access data
#' 
#' @param data, data.frame object containing data for geography
#' @param level, string representing level of pull
#'
#' @return Input data.frame joined with usda data
get_usda_data <- function(data, level = c("state", "county", "tract", "place")){
  
  # Specify arguments
  level <- match.arg(level)
  
  # Pull data via USDA table
  geo_info <-
    if (level == "state"){
      # Aggregate to state level
      limited_access %>% group_by(GEOID = substr(GEOID, 1, 2)) %>%
        summarise(low_access_healthy_food = mean(low_access_healthy_food, 
                                                 na.rm = T))
      
    } else if (level == "county"){
      # Aggregate to county level
      limited_access %>% group_by(GEOID = substr(GEOID, 1, 5)) %>%
        summarise(low_access_healthy_food = mean(low_access_healthy_food, 
                                                 na.rm = T))
      
    } else if (level == "tract" || level == "place") {
      # Filter by selected tracts
      limited_access
      
    } else {
      stop("Improper level. Level must be state, county, tract, or place.")
    }
  
  # Add new field to dataframe
  geo_info <- geo_info[geo_info$GEOID %in% data$GEOID,]
  rownames(data) <- data$GEOID
  data[geo_info$GEOID, "low_access_healthy_food"] <- geo_info$low_access_healthy_food
  
  return(data)
}


#' Helper function to convert 2010 census tract based data to 2020 census tracts. 
#' This function uses a land-area weighted average to align any 2020 census tracts that are constructed of two or more 2010 census tracts. 2010 census tracts that are split into multiple tracts in 2020 inherit 2010 values.
#' 
#' @param data, data.frame object containing data 2010 census tract GEOIDs and variable information
#' @param geoid, column name with 11 digit census tract geoid (2 digit state + 3 digit county + 6 digit census tract components)
#' @param variable, column name with numeric value of data column
convert_to_2020_ct <- function(data, geoid, variable){
  geoid <- enquo(geoid)
  variable <- enquo(variable)
  
  updated_data <- data %>% 
    mutate(GEOID = as.character(!!geoid),
           var = as.numeric(!!variable)) %>%
    right_join(tract_xw, by = c("GEOID" = "GEOID_TRACT_10")) %>%
    mutate(AREALAND_TRACT_20 = as.numeric(AREALAND_TRACT_20)) %>%
    group_by(GEOID_TRACT_20) %>%
    summarize(updated_var = weighted.mean(var, AREALAND_TRACT_20, na.rm = TRUE)) %>%
    dplyr::select(GEOID_TRACT_20, updated_var)
  
  colnames(updated_data) <- c(quo_name(geoid), quo_name(variable))
  
  return(updated_data)
}

## MAIN FUNCTION ---------------------------------------------------------------

#' Main function to merge all data
#'
#' @param geo_level, string representing level of pull
#' @param st, string representing state if level is county, tract, or place
#' @param county, string representing county if level is tract
#' @param place, string representing place if level is place
#'
#' @return List containing data.frame of all data and SpatialPolygonsDataFrame 
#' of geography
get_merged_shp <- function(geo_level = c("state", "county", "tract", "place"),
                           acs_year,
                           st = NULL, county = NULL, place = NULL, stateFP = NULL, places_tracts = NULL){

  # Specify arguments
  geo_level <- match.arg(geo_level)  
  
  
  # merge ACS data and geography ----
  dat_shp <- get_acs_info(geo_level, st = st, county = county, place = place, acs_year = acs_year, places_tracts = places_tracts, stateFP = stateFP)
  
  
  # Separate into data and geography
  geo_shp <- dat_shp$shp
  merged_dat <- dat_shp$dat
  
  
  # merge redlining data ----
  if (geo_level == "tract" || geo_level == "place"){
    new_holc_dat <- holc_dat %>%
      filter(GEOID %in% merged_dat$GEOID)
    
    merged_dat[as.character(new_holc_dat$GEOID),"holc_grade_pop"] <-
      new_holc_dat$holc_grade_pop
  }
  
  
  # merge BRFSS/PLACES data ----
  merged_dat <- get_brfss_data(data = merged_dat, level = geo_level, st = st, 
                               county = county)
  
  
  # merge ICE data ----
  merged_dat <- get_ice_metric(data = merged_dat, level = geo_level, st = st, 
                               county = county)
  
  
  # merge PM2.5 data ----
  merged_dat <- get_pm_data(data = merged_dat, level = geo_level, st = st, 
                            county = county)
  
  
  # merge HUD data ----
  merged_dat <- get_hud_data(data = merged_dat, level = geo_level)
  
  
  # merge park access data ----
  merged_dat <- get_nephtn_data(data = merged_dat, level = geo_level)
  
  
  # merge low access to food data ----
  merged_dat <- get_usda_data(data = merged_dat, level = geo_level)
  
  
  # export merged data ----
  merged_dat <- rescale_ice(merged_dat)
  
  
  # Replace all NULL values with NA
  merged_dat[is.null(merged_dat)] <- NA_real_
  
  
  # Remove Puerto Rico (72)
  merged_dat <- merged_dat %>%
    filter(!(substr(GEOID, 1, 2) %in% c(72)))
  
  
  return(list("shp" = geo_shp, "dat" = merged_dat))
}
