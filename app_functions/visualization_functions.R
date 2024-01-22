# Map Tab Plotting Functions
# By Hannah De los Santos, Karen Jiang, Julianna Bernardi, Cassandra Okechukwu, AJ Liberatore
# Originated on: 3/15/21

source(file.path("directory_config.R"))

# plotting constants ----

# color coding for redlining data
holc_colors <- c(
  "A" = "#759e64", # green
  "B" = "#75a3b6", # blue
  "C" = "#c5c473", # yellow
  "D" = "#c27e8d" # red
)


# point coding for redlining data
holc_points <- c(
  "A" = 1, # green
  "B" = 2, # blue
  "C" = 3, # yellow
  "D" = 4 # red
)

# MITRE theme and colors
mitre_blue <- "#005B94"
mitre_blue_rgba <- 'rgba(0, 091, 148, 0.25)'
mitre_highlighter <- "#FFF601"
dark_navy <- "#0B2338"
navy <- "#0D2F4F"
light_blue <- "#87DEFF"
black <- "#111921"
dark_gray <- "#7E8284"
silver <- "#D4D4D3"
light_silver <- "#F1F3F4"
white <- "#FFFFFF"

# Get colors for each covariate category
cat_colors <- dat_info[dat_info$Category != "",
                       c("Category", "Colorbrewer.Palette.New")]
cat_colors <- cat_colors[!duplicated(cat_colors$Colorbrewer.Palette.New),]
cat_colors <- cat_colors[cat_colors$Colorbrewer.Palette.New != "",]
cat_colors$Category[cat_colors$Colorbrewer.Palette.New == "Other"] <- "Redlining (HOLC Grade)"

bcolor <- list()
for (ct in 1:nrow(cat_colors)){
  bcolor[[cat_colors$Category[ct]]] <- 
    if (!cat_colors$Category[ct] %in% c("Redlining (HOLC Grade)", 
                                        "Social: Residential Segregation")){
      brewer.pal(3, cat_colors$Colorbrewer.Palette.New[ct])[3]
    } else if (cat_colors$Category[ct] == "Redlining (HOLC Grade)"){
      colorRampPalette(holc_colors)(nchar("Redlining (HOLC Grade)"))
    } else { # disparities
      colorRampPalette(
        brewer.pal(11, cat_colors$Colorbrewer.Palette.New[ct])
      )(nchar("Social: Residential Segregation"))
    }
}


# styling helper functions ----

# Function to get stylized word
style_word <- function(x, n = NULL){
  if (!is.null(n)){
    names(x) <- n
  }
  
  new_bcolor <- if (names(x) != "holc_grade_pop"){
    bcolor[[dat_info[names(x), "Category"]]]
  } else {
    bcolor[["Redlining (HOLC Grade)"]]
  }
  
  p <- if (length(new_bcolor) == 1){
    paste0("<font color = ", new_bcolor, " ><b>", 
           x, "</font></b>")
  } else {
    paste(
      sapply(1:nchar(x), function(y){
        bchar <- strsplit(x, "")[[1]][y]
        paste0("<font color = ", new_bcolor[y], " ><b>", 
               bchar, "</font></b>")
      }), collapse = "")
  }
  return(p)
}


# Generates a color palette for plotting that matches the chosen display color of a variable
get_color_palette <- function(var_name, var_data, chosen_palette = FALSE, holc_colors = holc_colors <- c(
  "A" = "#759e64", # green
  "B" = "#75a3b6", # blue
  "C" = "#c5c473", # yellow
  "D" = "#c27e8d" # red
)) {
  
  # The Rank variable and chosen_palette must match correctly
  if (var_name == "Rank" & !chosen_palette) {
    stop("'var_name' must have a TRUE 'chosen_palette'")
  }
  if (var_name != "Rank" & chosen_palette) {
    stop("'var_name' must be Rank given a TRUE 'chosen_palette'")
  }
  
  # Get domain of variable values
  var_domain <- 
    if (chosen_palette){
      c(
        1, 
        1,
        var_data,
        length(var_data)
        )
    } else{
      c(
        min(c(dat_info[var_name, "Range.Low"], var_data), na.rm = T), 
        min(c(dat_info[var_name, "Range.Low"], var_data), na.rm = T) +
          dat_info[var_name, "Min.Range"], 
        var_data,
        max(c(dat_info[var_name, "Range.High"], var_data), na.rm = T)
        )
    }
  
  # Create color palette for fill
  pal <-
    if (var_name == "Rank"){
      colorNumeric(
        palette = "Reds",
        domain = var_domain,
        na.color = "transparent",
        reverse = T
        )
    } else if (var_name != "holc_grade_pop"){
      colorNumeric(
        palette = dat_info[var_name, "Colorbrewer.Palette.New"],
        domain = var_domain,
        na.color = "transparent",
        reverse = F
        )
    } else {
      colorNumeric(
        palette = holc_colors,
        domain = c(1, var_data, 4),
        na.color = "transparent",
        reverse = F
        )
    }
  
  return(pal)
}


# rank functions ----

#' Function to add mean_rank to dataframe for visuals
#' 
#' @param data, dataframe object of data
#' @param cov, string variable of covariate
#' @param outcome, string variable of outcome
#' 
#' @return dataframe object with additional rank columns
get_rank <- function(dat, cov, outcome){
  
  dat <- dat[which(!is.na(dat[[cov]])),]
  
  # assigns order based on variable
  cov_order <- 
    if(dat_info[dat_info$Colname == cov,"Rank.Cardinality"] == "increasing"){
      FALSE
    } else{
      TRUE
    }
  
  # ICE should be reversed
  rank_parity <-
    if (dat_info[dat_info$Colname == cov, "Pretty.Name"] %in% c("ICE, Nonwhite by Income ", "ICE, Rent Versus Owned ")) {
      1
    } else {
      -1
    }
  
  # Only include asthma in rank calculation if in data
  if (outcome %in% colnames(dat) & !(all(is.na(dat[[outcome]])))) {
    # if category is Population Composition: Race/Ethnicity then calculate based on asthma prev only
    if (dat_info[dat_info$Colname == cov,"Category"] == "Population Composition: Race/Ethnicity"){
      
      # break ties with population
      dat <- dat %>%
        arrange(!!as.name(outcome), pop_acs)
      dat$final_rank <- 1:nrow(dat)
      
      return(dat)
    }
    
    # order outcome and add rank, higher as variable increases
    order.scores <- order(dat[,outcome], decreasing = F)
    dat <- dat[order.scores,]
    dat$outcome_rank <- (rank(-1*dat[,outcome]))
    
    # order cov and add rank, depends on variable
    order.scores <- order(dat[,cov], decreasing = cov_order)
    dat <- dat[order.scores,]
    dat$covariate_rank <- rank(rank_parity*dat[,cov])
    
    # calculate mean cov/outcome rank and order
    dat$mean_rank_raw <- (dat$outcome_rank + dat$covariate_rank) / 2
    order.scores <- order(dat[,"mean_rank_raw"], decreasing = F)
    dat <- dat[order.scores,]
    dat$mean_rank <- floor(rank(dat[,"mean_rank_raw"]))
    
    # break ties with population
    dat <- dat %>%
      arrange(mean_rank, pop_acs)
    dat$final_rank <- 1:nrow(dat)
    
  } else {
    # if category is Population Composition: Race/Ethnicity then everything is treated equal - not meaningful
    if (dat_info[dat_info$Colname == cov,"Category"] == "Population Composition: Race/Ethnicity"){
      
      dat$final_rank <- rep(1, nrow(dat))
      
      return(dat)
    }
    
    # order cov and add rank, depends on variable
    order.scores <- order(dat[,cov], decreasing = cov_order)
    dat <- dat[order.scores,]
    dat$covariate_rank <- rank(rank_parity*dat[,cov])
    
    # calculate mean cov/outcome rank and order
    dat$mean_rank_raw <- dat$covariate_rank
    order.scores <- order(dat[,"mean_rank_raw"], decreasing = F)
    dat <- dat[order.scores,]
    dat$mean_rank <- floor(rank(dat[,"mean_rank_raw"]))
    
    # break ties with population
    dat <- dat %>%
      arrange(mean_rank, pop_acs)
    dat$final_rank <- 1:nrow(dat)
  }
  
  return(dat)
}


# map functions ----

# Function to get units of a variable
get_units <- function(var, pretty = F){
  unit <- 
    if (pretty){
      dat_info %>%
        dplyr::filter(Pretty.Name == var) %>%
        dplyr::pull(Unit)
    } else{
      dat_info %>%
        tibble::rownames_to_column() %>%
        dplyr::filter(rowname == var) %>%
        dplyr::pull(Unit)
    }
  
  return(unit)
}


# function for size legends in leaflet
# https://stackoverflow.com/questions/58505589/circles-in-legend-for-leaflet-map-with-addcirclemarkers-in-r-without-shiny
addLegendCustom <- function(map, colors, labels, sizes, 
                            opacity = 0.7, title = "Asthma Prevalence"){
  colorAdditions <- paste0(colors, "; border-radius: 50%; margin-top: 4px; width:",
                           sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                           sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", 
                           labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, 
                   labels = labelAdditions, 
                   opacity = opacity, 
                   position = "bottomright",
                   title = title))
}


build_map <- function(geo_dat_map, geo_level, cov_map, 
                      show_asthma = T, show_missing = T,
                      highlighted_geography, map_type = "leaflet",
                      fqhc_dat = NULL,
                      school_dat = NULL, chosen_palette = FALSE){
  
  # set up map and fill variables
  gd_map <- geo_dat_map$shp
  # add fill variable
  rownames(geo_dat_map$dat) <- geo_dat_map$dat$GEOID
  gd_map@data$Fill <- geo_dat_map$dat[gd_map@data$GEOID, cov_map]
  # add asthma and centroids for points
  if (show_asthma){
    gd_map@data$Asthma <- geo_dat_map$dat[gd_map@data$GEOID,
                                          "CASTHMA_CrudePrev_BRFSS"]
    gd_map@data <- cbind(gd_map@data,
                         geo_dat_map$dat[gd_map@data$GEOID,
                                         c("unw_centroid_long", 
                                           "unw_centroid_lat")])
    gd_map@data$Asthma_Size <- 
      floor(gd_map$Asthma-min(floor(gd_map$Asthma)+1, na.rm = T))+3
    
  }
  
  rank_df <- get_rank(geo_dat_map$dat, cov_map, "CASTHMA_CrudePrev_BRFSS") %>%
    arrange(GEOID)
  gd_map@data$final_rank <- rank_df[gd_map@data$GEOID,
                                    "final_rank"]
  
  # if a place, we want to remove everything that doesn't fall in the place's
  # bounding box -- defined by holc grade
  if (geo_level == "Place (Census-tract level data)"){
    gd_map@data$holc_grade <- geo_dat_map$dat[gd_map@data$GEOID,
                                              "holc_grade_pop"]
    sub_map <- gd_map[!is.na(gd_map@data$holc_grade),]
    sub_map <- raster::crop(gd_map, bbox(sub_map))
    gd_map <- gd_map[gd_map@data$GEOID %in% sub_map@data$GEOID,]
  }
  # if remove missing, we need to remove the places that they don't line up
  if (!show_missing){
    gd_map <- gd_map[!is.na(gd_map$Fill),]
    if (show_asthma){
      gd_map <- gd_map[!is.na(gd_map$Asthma),]
    }
  }
  
  if (chosen_palette){
    cov_domain <- c(
      1, 
      1,
      gd_map$final_rank,
      length(gd_map$final_rank)
    )
    
    pal <- colorNumeric(
      palette = "Reds",
      domain = cov_domain,
      na.color = "transparent",
      reverse = T
    )
    pal_wo_na <- colorNumeric(
      palette = "Reds",
      domain = cov_domain,
      na.color = rgb(0,0,0,0),
      reverse = T
    )
  } else {
    cov_domain <- c(
      min(c(dat_info[cov_map, "Range.Low"], gd_map$Fill), na.rm = T), 
      min(c(dat_info[cov_map, "Range.Low"], gd_map$Fill), na.rm = T) +
        dat_info[cov_map, "Min.Range"], 
      gd_map$Fill,
      max(c(dat_info[cov_map, "Range.High"], gd_map$Fill), na.rm = T)
    )
    
    # create color palette for fill
    if (cov_map != "holc_grade_pop"){
      pal <- colorNumeric(
        palette = dat_info[cov_map, "Colorbrewer.Palette.New"],
        domain = cov_domain,
        na.color = "transparent",
        reverse = ifelse(
          dat_info[cov_map, "Colorbrewer.Palette.New"] == 
            "Demographic: Disparities",
          T, F
        )
      )
      pal_wo_na <- colorNumeric(
        palette = dat_info[cov_map, "Colorbrewer.Palette.New"],
        domain = cov_domain,
        na.color = rgb(0,0,0,0),
        reverse = ifelse(
          dat_info[cov_map, "Colorbrewer.Palette.New"] == 
            "Demographic: Disparities",
          T, F
        )
      )
    } else {
      pal <- colorNumeric(
        palette = holc_colors,
        domain = c(1, gd_map$Fill, 4),
        na.color = "transparent",
        reverse = F
      )
      pal_wo_na <- colorNumeric(
        palette = holc_colors,
        domain = c(1, gd_map$Fill, 4),
        na.color = rgb(0,0,0,0),
        reverse = F
      )
    }
  }
  
  if (show_asthma){
    asthma_domain <- c(
      floor(min(gd_map$Asthma, na.rm = T)),
      min(gd_map$Asthma, na.rm = T) +
        dat_info["CASTHMA_CrudePrev_BRFSS", "Min.Range"], 
      gd_map$Asthma
    )
    # create color palette for points
    pal_points <- colorNumeric(
      palette = c("#cbcdd4", "#0a0e14"),
      domain = asthma_domain,
      na.color = "transparent"
    )
    pal_points_wo_na <- colorNumeric(
      palette = c("#cbcdd4", "#0a0e14"),
      domain = asthma_domain,
      na.color = rgb(0,0,0,0)
    )
    pal_points_outline <- colorNumeric(
      palette = c("#9b9fab", "#000000"),
      domain = asthma_domain,
      na.color = "transparent"
    )
    pal_points_outline_wo_na <- colorNumeric(
      palette = c("#9b9fab", "#000000"),
      domain = asthma_domain,
      na.color = rgb(0,0,0,0)
    )
  }
  
  # Add units to labels
  cov_unit <- get_units(cov_map)
  outcome_unit <- get_units("CASTHMA_CrudePrev_BRFSS")
  
  # labels 
  labels <- 
    paste0(
      "<b>", gd_map$NAME, "</b><br>",
      trimws(col_to_pretty[cov_map])," (", cov_unit, "): ", signif(gd_map$Fill, 4),
      if(show_asthma){
        paste0("<br>Asthma Prevalence (", outcome_unit, "): ",
               signif(gd_map$Asthma, 4))
      } else { "" }, 
      "<br>", 
      "Rank: ", gd_map$final_rank
    ) %>%
    lapply(htmltools::HTML)
  
  # get initial zoom area
  bounds <- bbox(gd_map)
  # update for national to zoom on continental US
  if (geo_level == "State (County level data)"){
    bounds <- bbox(gd_map[!gd_map$NAME %in% c("Alaska", "Puerto Rico"),])
  } else if (geo_level == "Place (Census-tract level data)"){
    bounds <- bbox(gd_map[!is.na(gd_map$holc_grade),])
  }

  legend_title <-
    if (chosen_palette){
      unname("Rank")
    } else{
      unname(col_to_pretty[cov_map])
    }
  
  fill_color <-
    if (chosen_palette){
      "final_rank"
    } else{
      "Fill"
    }
  
  
  if (map_type == "leaflet") {
    return(build_leaflet_map(gd_map=gd_map,
                             show_asthma=show_asthma,
                             geo_level=geo_level,
                             cov_unit=cov_unit,
                             cov_domain=cov_domain,
                             cov_map=cov_map,
                             outcome_unit=outcome_unit,
                             bounds=bounds,
                             pal=pal,
                             pal_wo_na=pal_wo_na,
                             pal_points=pal_points,
                             pal_points_wo_na=pal_points_wo_na,
                             pal_points_outline=pal_points_outline,
                             pal_points_outline_wo_na=pal_points_outline_wo_na,
                             labels=labels,
                             highlighted_geography=highlighted_geography,
                             fqhc_dat=fqhc_dat,
                             school_dat=school_dat,
                             legend_title = legend_title,
                             fill_color = fill_color))
  } else if (map_type == "ggplot") {
    return(build_ggplot_map(gd_map=gd_map,
                            cov_map=cov_map,
                            pal=pal,
                            pal_points=pal_points,
                            fill_color = fill_color,
                            legend_title = legend_title,
                            show_asthma = show_asthma))
  } else {
    stop("Map type must be either 'leaflet' or 'ggplot'")
  }
}


# Function to filter points by given geography
get_points_in_geography <<- function(point_dat, geo_dat) {
  
  # Remove empty points
  dat_wona <- point_dat %>%
    dplyr::filter(!is.na(long) &
                    !is.na(lat))
  # Convert point data frame to spatialPointsDataFrame
  coordinates(dat_wona) <- ~long + lat
  proj4string(dat_wona) <- proj4string(geo_dat)

  new_crs <- CRS("+proj=longlat +datum=NAD83 +no_defs")
  dat_wona <- spTransform(dat_wona, new_crs)
  geo_dat <- spTransform(geo_dat, new_crs)
  
  # Only keep points that are in geo_dat polygons
  sub_dat <- over(geo_dat, dat_wona) %>% 
    # bind_cols(dat_wona@data) %>% 
    na.omit()
  # print('%%%')
  # print(over(dat_wona, geo_dat) %>% bind_cols(dat_wona@data) %>% na.omit())
  # print(over(dat_wona, geo_dat) %>% na.omit())
  # print(geo_dat@data)
  # print(over(geo_dat,dat_wona) %>% na.omit())
  # print('***')
  # print(sub_dat)
  # print('+++')

  return(sub_dat)
}

# [1] "%%%"
# class       : SpatialPolygonsDataFrame 
# features    : 27 
# extent      : -85.69675, -85.23485, 32.40984, 32.73783  (xmin, xmax, ymin, ymax)
# crs         : +proj=longlat +datum=NAD83 +no_defs 
# variables   : 9
# names       :       GEOID,                                     NAME,             Fill, Asthma, unw_centroid_long, unw_centroid_lat, Asthma_Size, final_rank, holc_grade 
# min values  : 01081040201, Census Tract 402.01, Lee County, Alabama, 6.92114603887458,    8.9,      -85.60733114,       32.4837704,           2,          1,         NA 
# max values  : 01081042104, Census Tract 421.04, Lee County, Alabama, 8.25394538317172,   14.5,      -85.32532124,      32.65976223,           8,         27,         NA 
# [1] "***"


# function to plot the explorer map (LEAFLET)
build_leaflet_map <- function(gd_map,
                              show_asthma,
                              geo_level,
                              cov_unit,
                              cov_domain,
                              cov_map,
                              outcome_unit,
                              bounds,
                              pal,
                              pal_wo_na,
                              pal_points,
                              pal_points_wo_na,
                              pal_points_outline,
                              pal_points_outline_wo_na,
                              labels,
                              highlighted_geography,
                              fqhc_dat,
                              school_dat,
                              legend_title,
                              fill_color){
  
  # Plot
  p <- leaflet() %>%
    addProviderTiles("CartoDB.Voyager") %>% #OpenStreetMap
    addPolygons(data = gd_map,
                weight = 1,
                opacity = 1,
                fillColor = ~pal(get(fill_color)),
                fillOpacity = .7,
                color = "#b2aeae",
                dashArray = "",
                layerId = ~GEOID,
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "14px",
                  direction = "auto"
                ),
                highlight = highlightOptions(weight = 2,
                                             color = "#666",
                                             dashArray = "",
                                             fillOpacity = 0.8
                ),
    ) %>% 
    addLegend(data = gd_map,
              pal = pal_wo_na,
              values = cov_domain, 
              opacity = 0.7, 
              position = "bottomright",
              title = legend_title) %>%
    fitBounds(
      lng1 = bounds[1,1],
      lng2 = bounds[1,2],
      lat1 = bounds[2,1],
      lat2 = bounds[2,2]
    )
  
  # Add FQHC and school overlay data
  if (geo_level != "State (County level data)") {
    if (!is.null(fqhc_dat)) {
      # Spatial manipulation for FQHC points
      fqhc_dat$long <-
        fqhc_dat$`Geocoding Artifact Address Primary X Coordinate`
      fqhc_dat$lat <-
        fqhc_dat$`Geocoding Artifact Address Primary Y Coordinate`
      
      fqhc_sub <- get_points_in_geography(fqhc_dat, gd_map)

      fqhc_icon <- makeAwesomeIcon(icon = "hospital-o",
                                   iconColor = "white",
                                   library = "fa")
      
      p <- p  %>%
        addAwesomeMarkers(
          data = fqhc_sub,
          lng = ~ `Geocoding.Artifact.Address.Primary.X.Coordinate`,
          lat = ~ `Geocoding.Artifact.Address.Primary.Y.Coordinate`,
          group = "Federally Qualified Health Centers (FQHCs)",
          icon = fqhc_icon,
          label = ~ paste0(
            "<b>Health Center Name:</b> ",
            `Health.Center.Name`,
            "<br>",
            "<b>Site Name:</b> ",
            `Site.Name`,
            "<br>",
            "<b>City:</b> ",
            `Site.City`,
            "<br>"
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "14px",
            direction = "auto"
          ),
          clusterOptions = markerClusterOptions()
        )
    }
    
    if (!is.null(school_dat)) {
      # Spatial manipulation for public school points
      school_dat$long <- school_dat$LON
      school_dat$lat <- school_dat$LAT
      school_dat$school_name <- school_dat$NAME
      # print('ho ho ho')
      # print(gd_map)
      # print('wo wo wo')
      # print(school_dat)
      # print('hi hi hi')
      school_sub <- get_points_in_geography(school_dat, gd_map)
      
      # Create icons
      school_icon <- makeAwesomeIcon(icon = "graduation-cap",
                                     iconColor = "white",
                                     library = "fa")
      # print(school_sub)
      p <- p %>%
        addAwesomeMarkers(
          data = school_sub,
          lng = ~ LON,
          lat = ~ LAT,
          group = "Public Schools",
          icon = school_icon,
          label = ~ paste0(
            "<b>Name:</b> ",
            school_name,
            "<br>",
            "<b>City:</b> ",
            `CITY`,
            "<br>"
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "14px",
            direction = "auto"
          ),
          clusterOptions = markerClusterOptions()
        )
    }
    
    if (!is.null(fqhc_dat) & is.null(school_dat)) {
      p <- p %>%
        addLayersControl(
          overlayGroups = c("Federally Qualified Health Centers (FQHCs)"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup("Federally Qualified Health Centers (FQHCs)")
    } else if (is.null(fqhc_dat) & !is.null(school_dat)) {
      p <- p %>%
        addLayersControl(
          overlayGroups = c("Public Schools"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup("Public Schools")
    } else {
      p <- p %>%
        addLayersControl(
          overlayGroups = c(
            "Federally Qualified Health Centers (FQHCs)",
            "Public Schools"
          ),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup("Federally Qualified Health Centers (FQHCs)")  %>%
        hideGroup("Public Schools")
    }
  }
  
  # Can't do multiple polygons????
  if (!is.null(highlighted_geography)){
    gd_map_sub <- gd_map[gd_map$GEOID == highlighted_geography,]
    
    this_label <-
      paste0(
        "<b>", gd_map_sub$NAME, "</b><br>",
        trimws(col_to_pretty[cov_map])," (", cov_unit, "): ", signif(gd_map_sub$Fill, 4),
        if(show_asthma){
          paste0("<br>Asthma Prevalence (", outcome_unit, "): ",
                 signif(gd_map_sub$Asthma, 4))
        } else { "" }, "<br>", 
        "Rank: ", gd_map_sub$final_rank
      ) %>%
      lapply(htmltools::HTML)
    
    p <- p %>%
      addPolygons(data = gd_map[gd_map$GEOID == highlighted_geography,],
                   weight = 2,
                   opacity = 0.8,
                   fillColor = "#15B4B7",
                   fillOpacity = 0.7,
                   color = "#5A5A5A",
                   dashArray = "",
                   layerId = ~GEOID,
                   highlight = highlightOptions(weight = 1,
                                                stroke = 1,
                                               color = "#666",
                                               dashArray = "",
                                               fillOpacity = 0.8),
                  label = this_label,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "14px",
                    direction = "auto"
                  ))
  }
  
  
  if (show_asthma){
    p <- p %>%
      addCircleMarkers(
        data = gd_map,
        radius = ~floor(Asthma-min(floor(gd_map$Asthma)+1, na.rm = T))+3,
        lng = ~unw_centroid_long,
        lat = ~unw_centroid_lat,
        weight = 1,
        fillColor = ~pal_points(Asthma),
        color = ~pal_points_outline(Asthma),
        fillOpacity = .7,
        opacity = .7,
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "14px",
          direction = "auto"
        ),
        layerId = ~GEOID
      ) %>%
      addLegendCustom(
        opacity = .7,
        colors = pal_points_wo_na(
          rev(seq(max(gd_map$Asthma_Size, na.rm = T), 
                  min(gd_map$Asthma_Size, na.rm = T), by = -2))+
            min(floor(gd_map$Asthma - gd_map$Asthma_Size), na.rm = T)
        ),
        labels = 
          rev(seq(max(gd_map$Asthma_Size, na.rm = T), 
                  min(gd_map$Asthma_Size, na.rm = T), by = -2))+
          min(floor(gd_map$Asthma - gd_map$Asthma_Size), na.rm = T),
        sizes = 
          rev(seq(max(gd_map$Asthma_Size, na.rm = T), 
                  min(gd_map$Asthma_Size, na.rm = T), by = -2))*2
      )
  }
  
  return(p)
}

# function to plot the explorer map (ggplot)
build_ggplot_map <- function(gd_map,
                             cov_map,
                             pal,
                             pal_points,
                             fill_color,
                             legend_title,
                             show_asthma) {
  
  if (fill_color == "final_rank" & !show_asthma) {
    return(ggplot())
  }
  
  cov_domain <- c(
    min(c(dat_info[cov_map, "Range.Low"], gd_map[[fill_color]]), na.rm = T), 
    min(c(dat_info[cov_map, "Range.Low"], gd_map[[fill_color]]), na.rm = T) +
      dat_info[cov_map, "Min.Range"], 
    gd_map[[fill_color]],
    max(c(dat_info[cov_map, "Range.High"], gd_map[[fill_color]]), na.rm = T)
  ) %>% na.omit()
  
  s <- scales::rescale(cov_domain, c(0,1))
  s <- seq(min(s), max(s), length.out = 250)
  news <- scales::rescale(s, c(min(cov_domain), max(cov_domain)))
  
  temp <-
    # Convert to spatial for plotting
    sf::st_as_sf(gd_map) %>%
    # Assign colors to values
    mutate(color_col = pal(Fill))
  
  if (show_asthma) {
    temp <- temp %>%
      mutate(points_color_col = pal_points(Asthma))
  }
  
  arranged_scale <- c(
      pal(seq(min(news),median(news), length.out = 125)),
      pal(seq(median(news), max(news), length.out = 125)))
  
  
  # Plot
  g <- ggplot(temp) +
    # Set map with factor shading
    geom_sf(aes(geometry = geometry, fill = get(fill_color)),
            # fill = pal(temp$Fill),
            color = "#b2aeae",
            alpha = .7) +
    # Manual colorbar
    scale_fill_gradientn(colors = arranged_scale,
      limits = c(min(cov_domain), max(cov_domain)))+
    # Adjust formatting
    theme(
      panel.background = element_blank(),
      legend.key = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16)
    ) +
    xlab("") +
    ylab("") +
    # labs(fill = ) +
    guides(
      fill = guide_colorbar(reverse = TRUE, 
                            title = paste(strwrap(legend_title, width = 30), collapse = "\n"))
    )
  
  if (show_asthma) {
    g <- g +
      # Add asthma overlay
    geom_point(aes(
      x = unw_centroid_long,
      y = unw_centroid_lat,
      color = Asthma,
      size = Asthma
    ),
    alpha = .7) +
      # Manual color scale for points
      scale_color_gradient(low = "#cbcdd4",
                           high = "#0a0e14") +
      scale_size_continuous(
        range = c(1, 6)
      ) +
      guides(
        size = guide_legend(title = "Asthma Prevalence"),
        color = guide_legend(title = "Asthma Prevalence"))
  }
  return(g)
  
}





# scatterplot functions ----

# Build outcome vs. covariate scatterplot
build_cov_outcome_scatterplot <-
  function(all_data,
           cov,
           outcome,
           show_legend = TRUE,
           highlighted_geography = NULL,
           bubble_size = "Population Density",
           chosen_palette = FALSE,
           scatter_type = "plotly") {
    all_data$dat <- get_rank(all_data$dat, cov, outcome)
    all_data$shp <- all_data$shp[all_data$shp$GEOID %in% all_data$dat$GEOID,]
    
    # Handle missing values
    temp <-
      all_data$dat[, c(cov, outcome, "pop_acs", "NAME", "GEOID", "final_rank")]
    valid_data <- complete.cases(temp)
    temp <- temp[valid_data, ]
    
    # Create color palette that matches the covariate's display color
    if (!chosen_palette){
      exp_color_pal <- get_color_palette(cov, temp[[cov]], chosen_palette)
      color_pal <- exp_color_pal(temp[[cov]])
      color_pal_line <- exp_color_pal(temp[[cov]])
    } else{
      exp_color_pal <- get_color_palette("Rank", temp[["final_rank"]], chosen_palette)
      color_pal <- exp_color_pal(temp[["final_rank"]])
      color_pal_line <- exp_color_pal(temp[["final_rank"]])
    }
    
    if (!is.null(highlighted_geography)){
      color_pal[which(temp$GEOID == highlighted_geography)] <- "#15B4B7"
        color_pal_line[which(temp$GEOID == highlighted_geography)] <- "#5A5A5A"
    }
    
    # Add units to labels
    xlab_unit <- get_units(cov)
    ylab_unit <- get_units(outcome)
    
    # Compute the population density for each data point and scale bubble size according to pop_dens
    polygon_area <-
      area(all_data$shp)[all_data$shp@data$NAME != 'Puerto Rico'][valid_data]
    polygon_area <- polygon_area / 10 ^ 6  #area in km^2
    pop_dens <- temp$pop_acs / polygon_area
    pop_size <- temp$pop_acs
    
    if (!(bubble_size %in% c("Population Density", "Population Size"))) {
      stop("'bubble_size' must be one of 'Population Density', 'Population Size'")
    }
    
    bubble_size_var <- if (bubble_size == "Population Density") {
      pop_dens
    } else {
      pop_size
    }
    
    temp$pop_var <- if (bubble_size == "Population Density") {
      paste0(round(bubble_size_var, 2), " (per square kilometer)")
    } else {
      scales::label_comma(accuracy = .1)(bubble_size_var)
    }
    
    pop_var_label <-  if (bubble_size == "Population Density") {
      paste0(bubble_size, "\n (per square kilometer)")
    } else {
      bubble_size
    }
    
    desired_maximum_marker_size <- 40
    sizeref <-
      2.0 * max(bubble_size_var) / (desired_maximum_marker_size ** 2) #sizes all bubbles relative to max_marker_size
    
    # Determine scales of X and Y variables
    scales_x <- c(min(temp[[cov]], na.rm = T), max(temp[[cov]], na.rm = T))
    scales_y <-
      c(min(temp[[outcome]], na.rm = T), max(temp[[outcome]], na.rm = T))
    
    # Add padding
    scales_x[1] <- scales_x[1] - (scales_x[2] - scales_x[1]) * 0.12
    scales_x[2] <- scales_x[2] + (scales_x[2] - scales_x[1]) * 0.12
    scales_y[1] <- scales_y[1] - (scales_y[2] - scales_y[1]) * 0.12
    scales_y[2] <- scales_y[2] + (scales_y[2] - scales_y[1]) * 0.12
    
    
    js <- "
    function(el, x, inputName){
      var id = el.getAttribute('id');
      var gd = document.getElementById(id);
      var d3 = Plotly.d3;
      Plotly.update(id).then(attach);
        function attach() {
          var coordinates = [null, null]

          gd.addEventListener('click', function(evt) {
            var xaxis = gd._fullLayout.xaxis;
            var yaxis = gd._fullLayout.yaxis;
            var bb = evt.target.getBoundingClientRect();
            var x = xaxis.p2d(evt.clientX - bb.left);
            var y = yaxis.p2d(evt.clientY - bb.top);
            var coordinates = [x, y];
            Shiny.setInputValue(inputName, coordinates);
          });
        };
  }
  "
    
    if (scatter_type == "plotly") {
      return(
        build_plotly_scatter(
          all_data,
          temp,
          cov,
          outcome,
          color_pal,
          color_pal_line,
          bubble_size,
          bubble_size_var,
          scales_x,
          scales_y,
          pop_var_label,
          xlab_unit,
          ylab_unit,
          sizeref,
          show_legend,
          js
        )
      )
    } else if (scatter_type == "ggplot") {
      return(
        build_ggplot_scatter(
          all_data,
          temp,
          cov,
          outcome,
          color_pal,
          bubble_size,
          bubble_size_var,
          scales_x,
          scales_y,
          pop_var_label,
          xlab_unit,
          ylab_unit
        )
      )
    } else {
      stop("Scatter type must be either 'plotly' or 'ggplot'")
    }
  }


build_ggplot_scatter <-
  function(all_data,
           temp,
           cov,
           outcome,
           color_pal,
           bubble_size,
           bubble_size_var,
           scales_x,
           scales_y,
           pop_var_label,
           xlab_unit,
           ylab_unit) {
    
    temp$size_var <- bubble_size_var
    
    
    # Plot
    ggplot(temp) +
      geom_point(aes(
        x = get(cov),
        y = get(outcome),
        size = size_var
      ),
      alpha = .7,
      color = color_pal) +
      theme_bw() +
      geom_smooth(
        aes(
          x = get(cov),
          y = get(outcome),
          color = "grey"
        ),
        method = 'lm',
        formula = y ~ x,
        se = FALSE,
        lty = 2,
        size = 1
      ) +
      scale_color_manual(name = element_blank(),
                         values = "grey",
                         labels = "Linear") +
      scale_size_continuous(
        range = c(1, 15),
        breaks = c(
          min(bubble_size_var, na.rm = T),
          max(bubble_size_var, na.rm = T)
        ),
        labels = c("Low", "High")
      ) +
      theme(
        legend.key = element_blank(),
        panel.border = element_blank(),
        legend.title.align = 0.5,
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16)
      ) +
      scale_x_continuous(limits = scales_x) +
      scale_y_continuous(limits = scales_y) +
      xlab(paste0(col_to_pretty[cov], "\n(", xlab_unit, ")")) +
      ylab(paste0(col_to_pretty[outcome], "\n(", ylab_unit, ")")) +
      guides(size = guide_legend(
        title = pop_var_label,
        override.aes = list(color = silver, size = c(3, 10))
      ))
  }


build_plotly_scatter <- function(all_data,
                                 temp,
                                 cov,
                                 outcome,
                                 color_pal,
                                 color_pal_line,
                                 bubble_size,
                                 bubble_size_var,
                                 scales_x,
                                 scales_y,
                                 pop_var_label,
                                 xlab_unit,
                                 ylab_unit,
                                 sizeref,
                                 show_legend,
                                 js) {
  # Plot the main covariate vs. outcome scatter plot
  #  *Traces named "High" and "Low" are added in order to make a faux-legend for bubble size.
  #   Plotly doesn't currently have functionality for a bubble size legend.
  p <- plot_ly() %>%
      add_trace(
        x = temp[[cov]],
        y = temp[[outcome]],
        key = temp[["GEOID"]],
        type = 'scatter',
        mode = 'markers',
        marker = list(
          color = color_pal,
          line = list(color = color_pal_line,
                      width = 2),
          size = bubble_size_var,
          alpha = 0.8,
          sizeref = sizeref,
          sizemode = 'area'
        ),
        hoverinfo = "text",
        text = ~ paste0(
          "<b>",
          temp[["NAME"]],
          "</b><br>",
          trimws(col_to_pretty[cov]),
          " (",
          xlab_unit,
          "): ",
          signif(temp[[cov]], 4),
          "<br>",
          "Asthma Prevalence (",
          ylab_unit,
          "): ",
          signif(temp[[outcome]], 4),
          "<br>",
          "Rank: ",
          temp[["final_rank"]],
          "<br>",
          bubble_size,
          ": ",
          temp[["pop_var"]]
        ),
        showlegend = F
      ) %>%
      layout(
        font = list(color = dark_navy),
        # title = paste0(str_trim(col_to_pretty[cov]), "\nvs ", col_to_pretty[outcome]),
        xaxis = list(
          title = paste0(col_to_pretty[cov], "\n(", xlab_unit, ")"),
          range = scales_x
        ),
        yaxis = list(
          title = paste0(col_to_pretty[outcome], "\n(", ylab_unit, ")"),
          range = scales_y
        ),
        hoverlabel = list(bgcolor = "white")
      ) %>% 
    config(displayModeBar = F) %>%
    add_lines(
      x = temp[[cov]],
      y = predict(lm(
        formula = paste(outcome, "~", paste(cov, collapse = " + ")),
        data = temp
      )),
      line = list(
        dash = "dash",
        width = 1.5,
        color = "grey"
      ),
      name = "Linear"
    ) %>%
    onRender(js, data = "clickposition")
  
  # Determine whether to show axis labels and size legend
  if (show_legend) {
    p <- p %>%
      add_trace(
        x = 0,
        y = 0,
        name = 'High',
        type = 'scatter',
        mode = 'markers',
        marker = list(
          color = silver,
          line = list(color = dark_gray),
          size = 15
        )
      ) %>%
      add_trace(
        x = 0,
        y = 0,
        name = 'Low',
        type = 'scatter',
        mode = 'markers',
        marker = list(
          color = silver,
          line = list(color = dark_gray),
          size = 5
        )
      ) %>%
      layout(font = list(color = dark_navy),
             legend = list(
               title = list(text = bubble_size),
               orientation = "h",   # show entries horizontally
               xanchor = "center",  # use center of legend as anchor
               x = 0.5,
               y = -.2
               # x = 1.009,
               # y = 0,
               # bgcolor = 'rgba(0,0,0,0)'
             ))
  }
  
  return(p)
}



# table functions ----

# build table of covariates and outcome
build_cov_table <- function(all_data, cov, outcome, geo_unit, geo_level, highlighted_geography) {
  
  all_data$dat <- get_rank(all_data$dat, cov, outcome)
  
  geo_unit <- 
    if (geo_level == "State (County level data)"){
      "County"
    } else if (geo_level == "County (Census-tract level data)"){
      "Census Tract"
    } else{
      "Census Tract"
    }
  
  # Pull name of geography, outcome, and covariate
  if (!(outcome %in% colnames(all_data$dat))) {
    all_data$dat[[outcome]] <- rep("N/A", nrow(all_data$dat))
  }
  temp <- all_data$dat[, c("NAME", outcome, cov, "final_rank")]
  colnames(temp) = c(
    geo_unit,
    paste0(col_to_pretty[[outcome]], "<br> (", get_units(outcome), ")"),
    ifelse(
      cov == "holc_grade_pop",
      col_to_pretty[[cov]],
      paste0(col_to_pretty[[cov]], "<br> (", get_units(cov), ")")
    ),
    "Rank"
  )
  
  temp <- as.data.frame(temp)
  temp <- temp %>%
    mutate(across(where(is.numeric), signif, digits = 4)) %>%
    arrange(Rank)
  
  if(!is.null(highlighted_geography)){
    row_num <- which(rownames(temp) == highlighted_geography)-1
    
    p <- datatable(
      temp,
      options = list(
        pageLength = nrow(all_data$dat),
        scrollY = "200px",
        columnDefs = list(
          list(targets = 0, visible = FALSE),
          list(targets = c(2), width = "140px"),
          list(targets = c(3), width = "200px")
        ),
        scroller = TRUE,
        paging = TRUE,
        initComplete  = JS(
          paste0(
            'function() {
              this.api().table().row(',
              row_num,
              ').node().scrollIntoView({
                block: "nearest", inline: "start"
              });
            }'
            
          )
        )
      ),
      escape = FALSE,
      selection = 'none',
      callback = JS(
        "table.on('click.dt', 'tr', function() {
                    table.$('tr.selected').removeClass('selected');
                    $(this).toggleClass('selected');
                    Shiny.onInputChange('rows',
                      table.rows('.selected').data()[0][0]);
                  });"
      )
    ) %>%
        formatStyle(
          0,
          backgroundColor = styleEqual(highlighted_geography, "#15B4B7"),
          fontWeight = styleEqual(highlighted_geography, "bold"),
          target = "row"
        ) # ADDED TO TEST HIGHLIGHTING FEATURE
    }
 else{
  p <- datatable(
    temp,
    options = list(
      pageLength = nrow(all_data$dat),
      scrollY = "200px",
      columnDefs = list(
        list(targets = 0, visible = FALSE),
        list(targets = c(2), width = "140px"),
        list(targets = c(3), width = "200px")
      )
    ),
    selection = 'none',
    callback = JS(
      "table.on('click.dt', 'tr', function() {
                    table.$('tr.selected').removeClass('selected');
                    $(this).toggleClass('selected');
                    Shiny.onInputChange('rows',
                      table.rows('.selected').data()[0][0]);
                  });"
    ),
    escape = FALSE
  )
}
  
  return(p)
}


# explanation functions ----

# create interpretation of map
# Build explanation of relationship between variables
write_explanation <- function(all_data, cov, outcome, geo_name, geo_unit, highlighted_geography){
  
  all_data <- get_rank(all_data, cov, outcome)
  
  if (length(na.omit(all_data[[cov]])) < 5) {
    return(
      c(
        NA, 
        paste0(
          "Due to the small sample size, we cannot determine a relationship between <b>",
          col_to_pretty[cov],
          "</b> and <b>Asthma Prevalence</b>."
        )
      )
    )
  }
  if (!(outcome %in% colnames(all_data)) | sum(! is.na(all_data$CASTHMA_CrudePrev_BRFSS)) == 0) {
    return(
      c(
        NA,
        paste0(
          "Due to the absence of Asthma Prevalence data in ", geo_name, ", we cannot determine a relationship between<b> ",
          col_to_pretty[cov],
          "</b> and <b>Asthma Prevalence</b>."
        )
      )
    )
  }
  
  model <- lm(formula = paste(outcome, "~", paste(cov, collapse = " + ")), 
              data = all_data)
  # Write out interpretation statement of the model

  # Get p-values of main_cov and other covs
  p_vals <- summary(model)$coefficients[,"Pr(>|t|)"]
  p_vals <- p_vals[2:length(p_vals)] #remove intercept
  main_pval <- p_vals[cov]
  sig <- main_pval < 0.05
  p_vals <- p_vals[names(p_vals) != cov] #remove main_cov

  # Get model coefficients
  model_coefs <- round(model$coefficients, 3)[2:length(model$coefficients)]

  # Get model covariate names (excluding main_cov)
  covariates <- names(p_vals)

  # Outcome variable name
  outcome <- col_to_pretty[as.character(as.list(attr(model$terms, "variables"))[[2]])]

  # Write interpretation
  # return_strs is an array of strings which will be used to write the interpretation statement. It's returned
  # as an array so that differen elements, such as info tooltips can be inserted between sentences/paragraphs.
  return_strs <- c()

  if (sig) {
    # If the p-value on the main explanatory variable is significant (<0.05)

    # Direction of relationship
    pos_neg <- 
      if (model_coefs[cov] < 0){
        model_coefs[cov] <- model_coefs[cov]*-1
        "negative"
      } else{ 
        "positive" 
      }
    
    # Determine strength of relationship
    str_wea <- 
      if (sqrt(summary(model)$r.squared) < 0.5){
        "weak"
      } else{
        "strong"
      }

    # Get units of main_cov
    unit <- dat_info %>%
      filter(Pretty.Name == col_to_pretty[names(model_coefs[cov])]) %>%
      pull(Unit)

    str <- paste0("<p>With the factor you have specified, ", style_word(trimws(col_to_pretty[names(model_coefs[cov])])), " is <b>significantly</b> associated with ", style_word(outcome), " for ", geo_name, ". The relationship is ", pos_neg, " and ", str_wea, ", considering the ", length(which(!is.na(all_data[[cov]]))), " ", geo_unit, " within ", geo_name, ".</p>")
    return_strs <- append(return_strs, str)

  } else {
    # If the p-value on the main explanatory variable is NOT significant (>=0.05)
    # Removed info about what it means in favor of eventual extra info about specific geography
    str1 <- paste0("<p>With the factor you have specified, ", style_word(col_to_pretty[cov]), " is <b>not significantly</b> associated with ", style_word(outcome), " for ", geo_name, ".</p>")
    str2 <- paste0("<p><i>What does it mean if my chosen association isn't significant?</i></p>")
    return_strs <- append(return_strs, str1)
    return_strs <- append(return_strs, str2)
  }
  
  if (!is.null(highlighted_geography)){
    highlighted_outcome <- all_data %>% filter(GEOID == highlighted_geography) %>% pull(CASTHMA_CrudePrev_BRFSS)
    highlighted_cov <- all_data %>% filter(GEOID == highlighted_geography) %>% pull(cov)
    highlighted_name <- all_data %>% filter(GEOID == highlighted_geography) %>% pull(NAME)
    highlighed_rank <- all_data %>% filter(GEOID == highlighted_geography) %>% pull(final_rank)
    highlight_str <- paste0("<font color = black ><b><p>For ", highlighted_name, ", ", outcome, " is ", signif(highlighted_outcome, 4), " percent and ", col_to_pretty[names(model_coefs[cov])], " is ", signif(highlighted_cov, 4), " ", get_units(cov), " with a rank of ", highlighed_rank, ".</font></b></p>")
    return_strs <- append(return_strs, highlight_str)
  }


  return(c(sig, return_strs))
}


# processing functions ----

# function to get available geographies for asthma app
get_avail_geographies <- function(data_folder, geo_level){
  if (geo_level == "place"){
    return(
      gsub(".RData", "", 
          gsub("Asthma_Dashboard_", "", 
                list.files(file.path(data_folder, geo_level), 
                          ".RData", recursive = TRUE)))
    )   
  }
  return(
    gsub(".RData", "", 
         gsub("Asthma_Dashboard_", "", 
              list.files(file.path(data_folder, geo_level), 
                         ".RData",recursive=TRUE)))
  )
}

# Create name for the geography
geo_name_func <- function(geo_level, state = NULL, county = NULL, place = NULL) {
  if (geo_level == "State (County level data)"){
    state
  } else if (geo_level == "County (Census-tract level data)"){
    sub("_", ", ", county)
  } else{
    sub("_", ", ", place)
  }
}

# Get unit based on geo level
geo_unit_func <- function(geo_level) {
  if (geo_level == "State (County level data)"){
    "counties"
  } else if (geo_level == "County (Census-tract level data)"){
    "census tracts"
  } else{
    "census tracts"
  }
}


# Report Visualization Functions ----

# Function to produce histograms in report download
histogram_for_report <- function(dat, cov) {
  ggplot(dat) +
    geom_histogram(
      aes(x = get(cov), y = stat(density)),
      alpha = .6,
      fill = mitre_blue,
      boundary = 0,
      color = "grey",
      size = .1
    ) +
    geom_density(aes(x = get(cov)),
                 fill = NA,
                 alpha = .8,
                 color = "black") +
    xlab(gsub("<br>", "\n", str_trim(cov))) +
    ylab("Density") +
    ggtitle("") +
    theme_bw() +
    theme(
      text = element_text(),
      axis.text = element_text(size = 12, colour = "#555555"),
      axis.title = element_text(size = 12),
      axis.ticks = element_line(colour = "#555555"),
      legend.key = element_rect(colour = NA, fill = NA),
      # legend.text = element_text(size = base_size),
      # legend.direction = legend_direction,
      legend.spacing = unit(0, "cm"),
      legend.background = element_rect(fill = alpha("white", .8), colour = "grey92"),
      legend.justification = c("right", "top"),
      legend.title = element_text(size = 12),
      plot.margin = unit(c(2, 2, .5, .5), "cm"),
      strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
      strip.text = element_text(face = "bold"),
      panel.border = element_blank()
    )
}