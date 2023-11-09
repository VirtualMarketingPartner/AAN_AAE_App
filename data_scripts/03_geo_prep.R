# Places Geography Prep 
# By Hannah De los Santos, Karen Jiang, Julianna Bernardi, AJ Liberatore
# Originated on: 12/15/22

## LOADING DATA --------------

# Pull census places with shapefiles (generalized)
places <- tigris::places(state = c(state.abb, "DC"), cb = TRUE)

# Pull all tracts with shapefiles (generalized)
tracts <- NULL
for(state in c(state.abb, "DC")){
  # Pulls tract data for a state
  state_tract <- tigris::tracts(state = state, cb = TRUE)
  
  # Adds tract data to other states of data
  tracts <- rbind(tracts, state_tract)
}
rm(state_tract,state)

# Pull 2020 acs place population estimates
places_population <-
  tidycensus::get_acs(geography = "place", 
          variables = "B01003_001", 
          year = 2020, 
          state = c(state.abb, "DC"),
          geometry = FALSE)


## PROCESSING DATA --------------

# Creates table with place classifications
places_population_lookup <- places_population %>%
  tidyr::separate(col = NAME,
           into = c("Place", "State"),
           sep = ", ") %>%
  mutate(place_type = word(Place, -1)) %>%
  mutate(Place = str_remove(Place, fixed(place_type))) %>%
  filter(!is.null(estimate)) %>%
  mutate(place_class = ifelse(estimate >= 10000, 
                              ifelse(estimate >= 50000, 
                                     "Metropolitan", 
                                     "Micropolitan"
                                     ),
                              "Other"
                              )
         )

# Pulls states that have less than 10 metropolitan areas
micro_states <- places_population_lookup %>%
  filter(place_class != "Other") %>%
  group_by(State) %>%
  slice_max(order_by = estimate, n = 10) %>%
  filter(place_class == "Micropolitan") %>%
  pull(State) %>%
  unique()

# Pulls micropolitan places
selected_micro <- places_population_lookup %>%
  filter(place_class == "Micropolitan" & State %in% micro_states) %>%
  group_by(State) %>%
  slice_max(order_by = estimate, n = 10) %>%
  pull(GEOID)

# Pulls metropolitan places
selected_metro <- places_population_lookup %>%
  filter(place_class == "Metropolitan") %>%
  pull(GEOID)

# Full list of places to pull
selected_places <- c(selected_micro, selected_metro)

# Filters places to our list
places_cutout <- places %>%
  filter(GEOID %in% selected_places)

### final_places_tracts.RDS
# Creates dataframe with a tract's GEOID, state, place, and geometry.
# Takes roughly 10 minutes
places_tracts <- NULL
for (state in unique(places_cutout[["STATEFP"]])) {
  print(state)
  
  # Pulls places within state
  state_place <- places_cutout %>%
    dplyr::filter(STATEFP == state) %>%
    dplyr::select(STATEFP, NAME, geometry)
  
  # Pulls tracts within state
  state_tract <- tracts %>%
    dplyr::filter(STATEFP == state) %>%
    dplyr::select(GEOID, geometry)
  
  # Pulls the geometries for all tracts cropped by places
  state_place_tract <- st_intersection(state_tract, state_place)
  
  # Adds state data to data for other states
  places_tracts <- rbind(places_tracts, state_place_tract)
}

# Drops segmented geometry and adds tract geometry
final_places_tracts <- places_tracts %>%
  sf::st_drop_geometry() %>%
  dplyr::left_join(tracts %>% dplyr::select(GEOID, geometry), by = "GEOID")

saveRDS(final_places_tracts, file.path(data_folder, "Supporting_Files", "Geography", "final_places_tracts.RDS"))
