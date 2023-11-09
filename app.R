# Main asthma equity explorer app
# By Hannah De los Santos, Karen Jiang, Julianna Bernardi, AJ Liberatore
# Originated on: 3/15/21

# load packages ----

library(bsplus)
library(dplyr)
library(DT)
library(htmltools)
library(htmlwidgets)
library(leaflet)
library(plotly)
library(raster)
library(RColorBrewer)
library(rintrojs)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(stringi)
library(stringr)
library(shinycssloaders)
library(shinyWidgets)

options(spinner.type = 2, spinner.color.background="#ffffff")

source(file.path("app_functions", "directory_config.R"), chdir = TRUE)

css = HTML("
  .leaflet-top, .leaflet-bottom {
    z-index: unset !important;
  }

  .leaflet-touch .leaflet-control-layers, .leaflet-touch .leaflet-bar {
    z-index: 0 !important;
  }
")


# Initial data loads and preprocessing ----


addResourcePath(prefix = "markdown", directoryPath = "markdown")

# ??source
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    source(file.path(path, nm), ...)
  }
}


covariate_definitions <- 
  read.csv(
    file.path(
      data_folder,
      "Supporting_Data",
      "covariates_definitions.csv"
    )
  )

# define categories of covariates
covariate_categories <- 
  sapply(
    unique(dat_info$Category[dat_info$Category != ""]),
    function(x){
      temp <- str_split(x, " ")
      temp <- str_flatten(temp[[1]], collapse = "_")
      temp <- str_split(temp, "/")
      temp <- str_flatten(temp[[1]], collapse = "_")
      temp <- str_remove(temp, ":")
      str_remove(temp, "/")
    }
  )

# Public School Data
load(file.path(data_folder,
               "Supporting_Data",
               "public_school_dat.Rdata"))

# FQHC Data
load(file.path(data_folder,
               "Supporting_Data",
               "fqhc_dat.Rdata"))


# getting state names
st_abb_to_name <- setNames(state.name, state.abb)
st_abb_to_name["DC"] <- "District of Columbia"
st_abb_to_name <- st_abb_to_name[complete.cases(st_abb_to_name)]


# sourcing analysis and plotting functions
sourceDir("app_functions", local = environment(), chdir = TRUE)

# Load initial variables and data ----

# where we'll store the data that users look at
# pre-allocate each level
data_cache <- list()
data_cache[["State (County level data)"]] <- 
  data_cache[["County (Census-tract level data)"]] <- data_cache[["Place (Census-tract level data)"]] <- 
  list()


# we need something that has a list of all available jurisdictions for each
avail_data <- list()
avail_data[["State (County level data)"]] <- 
  avail_data[["County (Census-tract level data)"]] <- avail_data[["Place (Census-tract level data)"]] <-
  list()
avail_data$`State (County level data)` <- get_avail_geographies(data_folder, "county")
avail_data$`County (Census-tract level data)` <- get_avail_geographies(data_folder, "tract")
avail_data$`Place (Census-tract level data)` <- get_avail_geographies(data_folder, "place")
place_states <- unique(
  sapply(str_split(avail_data$`Place (Census-tract level data)`, "_"), function(x){ x[2] })
)


# Global variables
mod_outcome <- "CASTHMA_CrudePrev_BRFSS"



# UI ----
ui <- 
  MITREnavbarPage2(
    "Asthma Equity Explorer",
    
    tags$html(lang="en"),
    
    # Explore tab ----
    
    tabPanel(
      title = div("Explore", class="explore"),
      class="explore-panel",
      
      # To activate the use of popovers in your page
      use_bs_popover(),
      
      # To activate the disabling/enabling of buttons
      shinyjs::useShinyjs(),
      
      # To activate the rintrojs tutorial package
      introjsUI(),
      
      fluidRow(
        
        # Spacing column
        column(
          width = 1
        ),
        
        # Content column
        column(
          width = 10, 
            # Adds box around input and update button
            wellPanel(
              fluidRow(
                # Geography and factor input ----
                column(
                  width = 12, 
                  bsCollapse(
                    id = "model_output",
                    multiple = TRUE,
                    open = c("welcome_tab"),
                    bsCollapsePanel(
                      title = "Selection",
                      value = "welcome_tab",
                      
                      htmlOutput(
                        outputId = "input_text"
                      ),
                      fluidRow(
                        column(
                          width = 3,
                          offset = 2,
                          # Main covariate selection drop-down menu
                          selectInput(
                            "main_cov",
                            "Which factor would you like to investigate?",
                            choices = setNames("Particulate.Matter",
                                               "Particulate Matter") # will update on place selection
                          )
                        ),
                        column(
                          align = "center",
                          width = 2,
                          p(HTML("<b>Choose Color Palette:</b>"),
                            popify(
                              shiny_iconlink(),
                              title = NULL,
                              content = paste0(
                                "Toggle switch to color the plots by the chosen factor or by the rank. See the About page for more details on how rank is defined."
                              ),
                              placement = "right",
                              trigger = "hover",
                              options = list(container = "body")
                            )
                            ),
                          # Removes blue hilight for color palette toggle
                          tags$head(
                            tags$style(
                              HTML('.bootstrap-switch.bootstrap-switch-focused {
                                  -webkit-box-shadow: none;
                                  border-color: #CCCCCC;
                                  box-shadow: none;
                                  outline: none;
                                  }')
                              )
                            ),
                          # Color palette toggle
                          switchInput(
                            "cov_rank_toggle",
                            value = F,
                            onLabel = "Rank",
                            onStatus = "default",
                            offLabel = "Factor",
                            size = "small"
                          )
                        ),
                        column(
                          width = 5,
                          actionButton(
                            "show",
                            "Update Geography Selections",
                            class = "update-btn",
                            style = "color: white; background-color: #005B94;font-size: 20px; margin-top: 20px"
                          )
                        )
                      )
                    )
                  ),
                  div(
                    customModal(
                      id = "welcomeModal",
                      title = div(
                        "Welcome to the Asthma Equity Explorer!",
                        a(href = "https://allergyasthmanetwork.org/",
                          img(src = "Horizontal-logo-black-400.png", height = 50, align = "right"), 
                          target = "blank") 
                      ),
                      trigger = "placeholder",
                      HTML(
                        "<p style=\"font-size: 14px\">This interactive mapping tool is intended to help asthma programs, researchers, and community members understand the relationships between the Social Determinants of Health (SDOH) and asthma prevalence within our communities.
                        To get started, use the following drop down menus to choose a location and a relationship to explore.</p>"
                      ),
                      div(
                        actionButton(
                          inputId = "continue_to_selections",
                          label = "Start!",
                          style = "color: white; background-color: #005B94; display: block; margin-left: auto; margin-right: auto; font-size: 14px;"
                        ),
                      )
                    ),
                    customModal(
                      id = "dataModal",
                      title = div("Selections",
                                  shiny_iconlink(class = "help_btn") %>%
                                    bs_embed_popover(
                                      title = NULL,
                                      content = "<p>Use <b>Geography Selections</b> to define the scope of your question and the corresponding unit of analysis. 
                                There are four options for you to consider:  
                                <ul><li>Select <i>State (County level data)</i> to compare between <u>Counties</u>.</li>
                                <li>Select <i>County (Census-tract level data)</i> to compare between <u>Tracts</u>.</li>
                                <li>Select <i>Place (Census-tract level data)</i> to compare between <u>Tracts</u>.</li></ul>
                                <p>Once the geographic level and location is chosen, you can then choose the factor you think is most important for 
                                understanding Asthma Prevalence within your scope.</p>
                                <p>When you’re satisfied with your main factor, click <b>[Submit]</b> to add in covariates.</p>",
                                HTML = TRUE,
                                placement = "right",
                                trigger = "hover",
                                options = list(container = "body"))
                      ),
                      trigger = "show",
                      # Geography level drop-down menus
                      tags$style(
                        HTML(
                          ".selectize-input { color: #0B2338 }
                          .selectize-dropdown-content{ color: #0B2338 }
                          .selectize-dropdown-content .active { color: #0B2338 }
                          .selectize-dropdown .optgroup-header { color: #0B2338; font-weight: bold; }
                          "
                        )
                      ),
                      selectizeInput(
                        "geo_level",
                        "Which geographical level would you like to explore?",
                        choices = rev(names(data_cache)),
                        selected = "State (County level data)",
                        options = list(color = "#0B2338")
                      ),
                      
                      conditionalPanel(
                        condition = "input.geo_level == 'State (County level data)' || input.geo_level == 'County (Census-tract level data)' || input.geo_level == 'Place (Census-tract level data)'",
                        selectInput(
                          "state",
                          "Which state would you like to view?",
                          choices = c(),
                          selected = c()
                        )
                      ),
                      
                      conditionalPanel(
                        condition = "input.geo_level == 'County (Census-tract level data)'",
                        selectInput(
                          "county",
                          "Which county would you like to view?",
                          choices = c(), # will update in server
                          selected = c() #will update in server
                        )
                      ),
                      
                      conditionalPanel(
                        condition = "input.geo_level == 'Place (Census-tract level data)'",
                        selectInput(
                          "place",
                          "Which place would you like to view?",
                          choices = c(), # will update in server
                          selected = c() #will update in server
                        )
                      ),
                      div(
                        actionButton(
                          inputId = "update_vis",
                          label = "Submit",
                          class="update_btn",
                          style = "color: white; background-color: #005B94; display: block; margin-left: auto; "
                        ))
                    ),
                    
                  )
                ),
              )
            ),
            # Adds padding between visuals
              # Row for map and scatterplot
              fluidRow(
                # Map ----
                column(
                  style = "border: 2px solid #D4D4D3; padding: 0px; height: 650px;",
                  width = 6, 
                  tags$head(tags$style(css)),
                  withSpinner(leafletOutput("zoom_map", height='646px'))
                ), 
                # Scatterplot ----
                column(
                  style = "border: 2px solid #D4D4D3; padding: 10px; height: 650px;", 
                  width = 6, 
                  dropdownButton(
                    radioGroupButtons(
                      inputId = "bubble_size_toggle",
                      label = "Toggle Scatter Bubble Size",
                      choices = c("Population Density", "Population Size"),
                      status = "primary",
                      checkIcon = list(
                        yes = icon("ok", 
                                   lib = "glyphicon"),
                        no = icon("xmark",
                                  lib = "glyphicon"))
                    ),
                    size = "s",
                    circle = TRUE,
                    status = "primary", 
                    icon = tags$i(class = "fas fa-bars", style="font-size: 14px"), width = "350px",
                    tooltip = tooltipOptions(title = "Click to change population parameter")
                  ),
                  withSpinner(plotlyOutput("scatter_maincov", height='600'))
                )
              ),
          fluidRow(
            div(style = "margin-bottom: 10px;")
          ),
            # Row for table, explanation, and download buttons
            fluidRow(
              # Table ----
              column(
                width = 6, 
                withSpinner(dataTableOutput("cov_table"))
              ),
              # Column with explanation and download buttons
              column(
                width = 6, 
                fluidRow(
                  # Explanation ----
                  column(
                    width = 12,
                    bsCollapse(
                      id = "model_output",
                      multiple = TRUE,
                      open = c("interpret_tab"),
                      bsCollapsePanel(
                        title = "Explanation",
                        value = "interpret_tab",
                        withSpinner(uiOutput("mod_interpretation", width = 12))
                      )
                    )
                  )
                ),
                fluidRow(
                  # Spacing column
                  column(
                    width = 3
                  ),
                  # Download data ----
                  column(
                    width = 3, 
                    div(
                      HTML("<center>"),
                      downloadButton(
                        style = "color: white; background-color: #005B94;", 
                        "download_data", 
                        "Download Data", 
                        class="download_btn"),
                      HTML("</center>")
                    )
                  ),
                  # Download report ----
                  column(
                    width = 3, 
                    div(
                      HTML("<center>"),
                      downloadButton(
                        style = "color: white; background-color: #005B94;", 
                        outputId = "download_report", 
                        label = "Download Report"),
                      HTML("</center>")
                    )
                  ),
                  # Spacing column
                  column(
                    width = 3
                  )
                )
              )
            )
        ),
        
        # Spacing column
        column(
          width = 1
        )
        
      )
    ),
    
    # About tab ----
    tabPanel(
      title = div("About", class="about"),
      htmltools::tags$iframe(
        src = "markdown/about.html",
        class = "about-panel",
        frameborder = 0,
        scrolling = "auto")
    )
  )



# Server ----
server <- function(input, output, session){
  
  toggleModal(session, modalId = "welcomeModal", toggle = "open")
  
  observeEvent(input$continue_to_selections, {
    toggleModal(session, modalId = "welcomeModal", toggle = "close")
    toggleModal(session, modalId = "dataModal", toggle = "open")
  })
  
  # Allocate and update observers for the cache across all tabs ----
  cache_size <- reactiveVal(1)
  cache_queue <- reactiveVal(c("County")) # first in first out
  
  # Preallocate values ----
  
  # will contain all of the inputs, but will only update on build button selection
  output_vals <- reactiveValues(
    geo_level = "State (County level data)",
    main_cov = "Particulate.Matter"
  )
  
  # Geography level data
  geo_dat <- reactiveVal(data_cache[["State (County level data)"]])
  
  # Geography level data that is ONLY updated when the [Update Visualization] and [Build Model] buttons are pressed
  geo_dat_btn_update <- reactiveVal(data_cache[["State (County level data)"]])
  
  
  # Create listeners for map levels
  county_listener <- reactive(list(input$state, input$geo_level))
  tract_listener <- reactive(list(input$county, input$geo_level))
  place_listener <- reactive(list(input$place, input$geo_level))
  
  # Create a listener for multiple map levels
  geo_listener <- reactive(list(input$state, 
                                input$county, 
                                input$place,
                                input$geo_level
  ))
  
  # Highlight geography
  highlighted_geo <- reactiveVal(NULL)
  
  
  
  # Observe input and update accordingly ----
  
  # Places aren't available in every state
  observeEvent(input$geo_level, {
    new_places <- sort(setNames(place_states, st_abb_to_name[place_states]))
    all_placess <- setNames(avail_data$`State (County level data)`, 
                            st_abb_to_name[avail_data$`State (County level data)`])
    if (input$geo_level == "Place (Census-tract level data)"){
      # counties need to be updated for selected state
      updateSelectInput(
        session,
        "state",
        "Which state would you like to view?",
        choices = new_places,
        selected = "AL"
      )
    }else{
      updateSelectInput(
        session,
        "state",
        "Which state would you like to view?",
        choices = all_placess,
        selected = "AL"
      )
    }
  })
  
  
  observeEvent(geo_listener(), {
    if (input$geo_level == "State (County level data)"){
      # state needs nothing updated -- in for clarity
      
    } else if (input$geo_level == "County (Census-tract level data)"){
      # counties need to be updated for selected state
      cty_log <- grepl(paste0("_", input$state), avail_data[["County (Census-tract level data)"]])
      all_ctys <- unlist(strsplit(avail_data[["County (Census-tract level data)"]][cty_log], "_"))
      all_ctys_pretty <- all_ctys[all_ctys != input$state]
      all_ctys_col <- avail_data[["County (Census-tract level data)"]][cty_log]
      
      if (!(input$county %in% all_ctys_col)) {
        updateSelectInput(
          session,
          "county",
          "Which county would you like to view?",
          choices = setNames(all_ctys_col, all_ctys_pretty),
          selected = all_ctys_col[1]
        )
      }
    } else if (input$geo_level == "Place (Census-tract level data)"){
      
      # counties need to be updated for selected state
      place_log <- grepl(paste0("_", input$state), avail_data[["Place (Census-tract level data)"]])
      all_places <- unlist(strsplit(avail_data[["Place (Census-tract level data)"]][place_log], "_"))
      all_places_pretty <- all_places[all_places != input$state]
      all_places_col <- avail_data[["Place (Census-tract level data)"]][place_log]
      
      if (!(input$place %in% all_places_col)) {
        updateSelectInput(
          session,
          "place",
          "Which place would you like to view?",
          choices = setNames(all_places_col, all_places_pretty),
          selected = all_places_col[1]
        )
      }
    }
  })
  
  
  # avail geographies update factors and cache data
  observeEvent(county_listener(), {
    # don't update if we haven't selected it
    if (input$geo_level == "State (County level data)"){
      # load the selected data
      if (input$state %in% names(data_cache[["State (County level data)"]])){
        # we already have the data cached
        geo_dat(data_cache[["State (County level data)"]][[input$state]])
      } else {
        
        state_selection <- if (input$state == "") {
          "AL"
        } else { input$state }
        
        # we need to load and add data to cache
        load(
          file.path(
            data_folder, 
            "county", 
            paste0("Asthma_Dashboard_", state_selection, ".RData")
          )
        )
        data_cache[["State (County level data)"]][[state_selection]] <<- geo_shp_dat
        geo_dat(data_cache[["State (County level data)"]][[state_selection]])
        
        # keep track of what we've cached (level///name)
        cache_queue(c(cache_queue(), 
                      paste0(input$geo_level, "///", state_selection)))
      }
      if (!(mod_outcome %in% colnames(geo_dat()$dat))) {
        show_alert(
          title = NULL,
          text = paste0(
            col_to_pretty[mod_outcome], " data is not available for ",
            state_selection, 
            ". Only factor data will be displayed."
          ),
          type = "info",
          btn_colors = "#005B94"
        )
      }     
    }
  })
  
  
  observeEvent(tract_listener(), {
    # don't update if we haven't selected it
    if (input$geo_level == "County (Census-tract level data)" & input$county != ""){
      # load the selected data
      if (input$county %in% names(data_cache[["County (Census-tract level data)"]])){
        # we already have the data cached
        geo_dat(data_cache[["County (Census-tract level data)"]][[input$county]])
      } else {
        # we need to load and add data to cache
        load(
          file.path(
            data_folder, 
            "tract",
            str_split(input$county,'/')[[1]][1],
            paste0("Asthma_Dashboard_", str_split(input$county,'/')[[1]][2], ".RData")
          )
        )
        data_cache[["County (Census-tract level data)"]][[input$county]] <<- geo_shp_dat
        geo_dat(data_cache[["County (Census-tract level data)"]][[input$county]])
        
        # keep track of what we've cached (level///name)
        cache_queue(c(cache_queue(), 
                      paste0(input$geo_level, "///", input$county)))
      }
      if (!(mod_outcome %in% colnames(geo_dat()$dat))) {
        if (str_detect(input$county, input$state)) {
          show_alert(
            title = NULL,
            text = paste0(
              col_to_pretty[mod_outcome], " data is not available for ",
              str_split(input$county, "_")[[1]][1], ", ",
              st_abb_to_name[str_split(input$county, "_")[[1]][2]], 
              ". Only factor data will be displayed."
            ),
            type = "info",
            btn_colors = "#005B94"
          )
        }
      }      
    }
  })
  
  
  observeEvent(place_listener(), {
    # don't update if we haven't selected it
    if (input$geo_level == "Place (Census-tract level data)" & input$place != ""){
      # load the selected data
      if (input$place %in% names(data_cache[["Place (Census-tract level data)"]])){
        # we already have the data cached
        geo_dat(data_cache[["Place (Census-tract level data)"]][[input$place]])
      } else {
        # we need to load and add data to cache
        print(file.path(data_folder, "place",str_split(input$place,'/')[[1]][1],paste0("Asthma_Dashboard_", str_split(input$place,'/')[[1]][2], ".RData")))
        load(
          file.path(
            data_folder, 
            "place",
            str_split(input$place,'/')[[1]][1],
            paste0("Asthma_Dashboard_", str_split(input$place,'/')[[1]][2], ".RData")
          )
        )
        data_cache[["Place (Census-tract level data)"]][[input$place]] <<- geo_shp_dat
        geo_dat(data_cache[["Place (Census-tract level data)"]][[input$place]])
        
        # keep track of what we've cached (level///name)
        cache_queue(c(cache_queue(), 
                      paste0(input$geo_level, "///", input$place)))
      }
      if (!(mod_outcome %in% colnames(geo_dat()$dat))) {
        if (str_detect(input$place, input$state)) {
          show_alert(
            title = NULL,
            text = paste0(
              col_to_pretty[mod_outcome], " data is not available for ",
              str_split(input$place, "_")[[1]][1], ", ",
              st_abb_to_name[str_split(input$place, "_")[[1]][2]], 
              ". Only factor data will be displayed."
            ),
            type = "info",
            btn_colors = "#005B94"
          )
        }
      }     
    }
  })
  
  # This is the same regardless of which state/county/city updates
  observeEvent(input$update_vis, {
    
    if (!is.null(geo_dat()$dat)) {
      
      # Filter by non-na columns
      avail_cov_dat <- geo_dat()$dat[,colSums(is.na(geo_dat()$dat) |
                                                geo_dat()$dat == "") != nrow(geo_dat()$dat)]
      
      # Get the available factor columns
      avail_cov <-
        na.omit(
          colnames(avail_cov_dat)[
            colnames(geo_dat()$dat) %in% rownames(dat_info)[dat_info$Category != ""]]
        )
      
      # Get the covariate choices available at this geographic level
      mod_explain_choices <-
        setNames(colnames(geo_dat()$dat)[match(avail_cov, colnames(geo_dat()$dat))],
                 col_to_pretty[colnames(geo_dat()$dat)[match(avail_cov, colnames(geo_dat()$dat))]])
      
      # Explanatory variable can't be the same as the outcome variable
      mod_explain_choices <-
        mod_explain_choices[!(names(mod_explain_choices) %in% col_to_pretty[mod_outcome])]
      
      # Nest them
      cov_opt <- list()
      for (ct in unique(dat_info$Category[dat_info$Category != ""])){
        cov_opt[[ct]] <-
          pretty_to_col[pretty_to_col %in% 
                          dat_info$Colname[dat_info$Category == ct] &
                          pretty_to_col %in% mod_explain_choices]
      }
      cov_opt <- Filter(length, cov_opt)
      
      # Get the one that should be selected
      cov_select <- 
        if (input$geo_level == "State (County level data)" |
            input$geo_level == "County (Census-tract level data)") {
          if (!("Particulate.Matter" %in% avail_cov)) {
            cov_opt$`Social/Environmental`[1]
          } else {
            "Particulate.Matter"
          }
        } else if (input$geo_level == "Place (Census-tract level data)") {
          if (!("holc_grade_pop" %in% avail_cov)) {
            cov_opt$`Social/Environmental`[1]
          } else {
            "holc_grade_pop"
          }
        }
      
      # Update main explanatory variable
      updateSelectInput(
        session,
        "main_cov",
        "Which factor would you like to investigate?",
        choices = cov_opt,
        selected = cov_select
      )
    }
    
  })
  
  
  # Button presses ----
  
  # Update Visualizations Button
  observeEvent(input$update_vis, {
    toggleModal(session, modalId = "dataModal", toggle = "close")
    
    # Updates to new inputs
    geo_dat_btn_update(geo_dat()) 
    
    # Update output_vals to render the plots
    for (nm in names(input)){
      output_vals[[nm]] <- input[[nm]]
    }
  })
  
  observeEvent(input$main_cov, {
    # Updates to new inputs
    geo_dat_btn_update(geo_dat()) 
    
    # Update output_vals to render the plots
    for (nm in names(input)){
      output_vals[[nm]] <- input[[nm]]
    }
  })
  
  # Create name for the geography
  geo_name <- reactive({
    geo_name_func(output_vals$geo_level,
                  output_vals$state,
                  output_vals$county,
                  output_vals$place)
  })
  
  # Get unit based on geo level
  geo_unit <- reactive({
    geo_unit_func(output_vals$geo_level)
  })
  
  # Covariate definitions
  definition_to_print <- reactive({
    covariate_definitions[
      covariate_definitions$Factor == col_to_pretty[output_vals$main_cov],
    ]
  })
  
  output$input_text <- renderUI({
    # Geo level
    number_str <- ""
    geo_str <- ""
    
    # asthma_available <- (mod_outcome %in% colnames(geo_dat_btn_update()$dat))
    asthma_available <- (mod_outcome %in% colnames(geo_dat_btn_update()$dat))
    dat_sub <- geo_dat_btn_update()
    
    # Get number of geographic units available for selection
    if (output_vals$geo_level == "State (County level data)") {
      # Get number of counties in state (with nonempty data)
      number_str <-
        if (asthma_available) {
          length(
            which(!is.na(
              dat_sub$dat[[output_vals$main_cov]] &
                dat_sub$dat[[mod_outcome]]
            ))
          )
        } else {
          length(
            which(!is.na(
              dat_sub$dat[[output_vals$main_cov]]
            ))
          )
        }
        
    } else if (output_vals$geo_level == "County (Census-tract level data)") {
      # Get number of census tracts in county (with nonempty data)
      number_str <- if (output_vals$county == "") {
        ""
      } else if (asthma_available) {
        length(
          which(!is.na(
            dat_sub$dat[[output_vals$main_cov]] &
              dat_sub$dat[[mod_outcome]]
          ))
        )
      } else {
        length(
          which(!is.na(
            dat_sub$dat[[output_vals$main_cov]]
          ))
        )
      }
    } else if (output_vals$geo_level == "Place (Census-tract level data)") {
      # Get number of census tracts in place (with nonempty data)
      number_str <- if (output_vals$place == "") {
        ""
      } else if (asthma_available) {
        length(
          which(!is.na(
            dat_sub$dat[[output_vals$main_cov]] &
              dat_sub$dat[[mod_outcome]]
          ))
        )
      } else {
        length(
          which(!is.na(
            dat_sub$dat[[output_vals$main_cov]]
          ))
        )
      }
    }
    
    geo_str <- 
      stri_replace_all_regex(geo_name(), 
                             pattern = state.abb, 
                             replacement = state.name, 
                             vectorize = FALSE)
    
    # Create sentence components
    sentence_phrase_one <- if (asthma_available) {
      HTML(
        paste0(
          "<div class=\"text-center\"><p style=\"font-size:20px;\" text-align: center>Asthma Prevalence and <b>",
          col_to_pretty[output_vals$main_cov]
        )
      )
    } else {
      HTML(
        paste0(
          "<div class=\"text-center\"><p style=\"font-size:20px;\" text-align: center><b>",
          col_to_pretty[output_vals$main_cov]
        )
      )
    }
      
    
    sentence_phrase_two <- popify(
      shiny_iconlink(),
      title = NULL,
      content = paste0(
        "<p><b>Group:</b> ",
        definition_to_print()$Group[1],
        "</p>",
        "<p><b>Source:</b> ",
        definition_to_print()$Source[1],
        "</p>",
        "<p><b>Definition: </b>",
        definition_to_print()$Definition[1],
        "</p>"
      ),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    )
    
    sentence_phrase_three <- HTML(
      paste0(
        "</b> is available for ",
        number_str,
        " ",
        geo_unit(),
        " in <b>",
        geo_str,
        "</b></p></div>"
      )
    )
    
    # Want simplified version for report
    sentence_for_report <- paste0(
      "**",
      str_trim(col_to_pretty[output_vals$main_cov]),
      "** are available for ",
      number_str,
      " ",
      geo_unit(),
      " in **",
      geo_str, "**."
    )
    visuals_for_report[["selection_sentence"]] <-
      sentence_for_report
    
    # Craft statement
    sentence <-
      div(sentence_phrase_one,
          sentence_phrase_two,
          sentence_phrase_three)
    
    # Display
    sentence
    
  })
  
  
  
  # Button presses ----
  
  # Visuals ----
  
  # Build map
  visuals_for_report <- reactiveValues()  
  
  output$zoom_map <- renderLeaflet({
    if (!is.null(geo_dat_btn_update()) & output_vals$main_cov != "" & !is.null(output_vals$geo_level)) {
      show_asthma <- if (!(mod_outcome %in% colnames(geo_dat_btn_update()$dat)) || sum(! is.na(geo_dat_btn_update()$dat$CASTHMA_CrudePrev_BRFSS)) <= 0) {
        FALSE
      } else {
        TRUE
      }
      p <- build_map(
        geo_dat_btn_update(),
        output_vals$geo_level,
        output_vals$main_cov,
        show_asthma = show_asthma,
        show_missing = FALSE,
        highlighted_geography = highlighted_geo(),
        fqhc_dat = fqhc_dat,
        school_dat = public_school_dat,
        chosen_palette = input$cov_rank_toggle
      )
      
      p_ggplot_rank <- build_map(
        geo_dat_btn_update(),
        output_vals$geo_level,
        output_vals$main_cov,
        show_asthma = show_asthma,
        show_missing = FALSE,
        highlighted_geography = highlighted_geo(),
        map_type = "ggplot",
        chosen_palette = TRUE
      )
      
      p_ggplot <- build_map(
        geo_dat_btn_update(),
        output_vals$geo_level,
        output_vals$main_cov,
        show_asthma = show_asthma,
        show_missing = FALSE,
        highlighted_geography = highlighted_geo(),
        map_type = "ggplot",
        chosen_palette = FALSE
      )
      
      visuals_for_report[["map"]] <- list(rank_plot=p_ggplot_rank, plot=p_ggplot)
      p
    }
  })
  
  changed_geo <- reactiveVal(FALSE)
  geo_highlighted <- reactiveVal(FALSE)
  
  # Observes click in zoom_map
  observeEvent(input$zoom_map_shape_click, {
    
    if (geo_highlighted()){
      geo_highlighted(FALSE)
      highlighted_geo(NULL)
    } else{
      geo_highlighted(TRUE)
      highlighted_geo(input$zoom_map_shape_click$id)
      changed_geo(TRUE)
    }
    
  })
  
  observeEvent(input$zoom_map_marker_click, {
    
    if (geo_highlighted()){
      geo_highlighted(FALSE)
      highlighted_geo(NULL)
    } else{
      geo_highlighted(TRUE)
      highlighted_geo(input$zoom_map_marker_click$id)
      changed_geo(TRUE)
    }
    
  })
  
  observeEvent(input$zoom_map_click, {

    if (geo_highlighted() & !changed_geo()){
      geo_highlighted(FALSE)
      highlighted_geo(NULL)
    }
    changed_geo(FALSE)

  })
  
  # Used this.
  # https://stackoverflow.com/questions/56193127/plotly-click-events-from-anywhere-on-the-plot
  # Try this instead.
  # https://stackoverflow.com/questions/42798377/shiny-leaflet-ploygon-click-event
  # Main covariate vs. outcome scatter plot
  output$scatter_maincov <- renderPlotly({
    if (!is.null(geo_dat_btn_update()) & output_vals$main_cov != "") {
      tryCatch({
        s <- build_cov_outcome_scatterplot(
          geo_dat_btn_update(),
          output_vals$main_cov,
          mod_outcome,
          highlighted_geography = highlighted_geo(),
          chosen_palette = input$cov_rank_toggle,
          bubble_size = input$bubble_size_toggle
        )
        
        s_ggplot <-
          build_cov_outcome_scatterplot(
            geo_dat_btn_update(),
            output_vals$main_cov,
            mod_outcome,
            highlighted_geography = highlighted_geo(),
            bubble_size = input$bubble_size_toggle,
            scatter_type = "ggplot",
            chosen_palette = FALSE
          )
        
        s_ggplot_rank <-
          build_cov_outcome_scatterplot(
            geo_dat_btn_update(),
            output_vals$main_cov,
            mod_outcome,
            highlighted_geography = highlighted_geo(),
            bubble_size = input$bubble_size_toggle,
            scatter_type = "ggplot",
            chosen_palette = TRUE
          )
        
        visuals_for_report[["scatter"]] <-
          list(plot = s_ggplot, rank_plot = s_ggplot_rank)
        s
      },
      error = function(e) {
        if (!(mod_outcome %in% geo_dat_btn_update())) {
          message(paste0(mod_outcome, " data is not available. Nothing to display."))
        } else {
          message(paste0(mod_outcome, " data is not available. Nothing to display."))
        }
      })
    }
  })
  
  # Observes click in scatter_maincov
  observeEvent(event_data("plotly_click", priority = "event"), {
    
    if (geo_highlighted()){
      geo_highlighted(FALSE)
      highlighted_geo(NULL)
    } else{
      geo_highlighted(TRUE)
      highlighted_geo(event_data("plotly_click", priority = "event")$key)
      changed_geo(TRUE)
    }
    
  })
  
  observeEvent(input$clickposition, {
    
    if (geo_highlighted() & !changed_geo()){
      geo_highlighted(FALSE)
      highlighted_geo(NULL)
    }
    changed_geo(FALSE)
    
  })
  
  
  data_table_for_report <- reactiveVal()
  
  # https://stackoverflow.com/questions/36132730/determine-if-dt-datatable-is-clicked-in-shiny-app
  # Descriptive table
  output$cov_table <- renderDataTable({
    if (!is.null(geo_dat_btn_update()) & output_vals$main_cov != "") {
      # Get unit based on geo level
      geo_unit <-
        if (output_vals$geo_level == "National (State level data)") {
          "National (State level data)"
        } else if (output_vals$geo_level == "State (County level data)") {
          "State (County level data)"
        } else if (output_vals$geo_level == "County (Census-tract level data)") {
          "Census Tract"
        } else{
          "Census Tract"
        }
      
      t <- build_cov_table(
        geo_dat_btn_update(),
        output_vals$main_cov,
        mod_outcome,
        geo_unit,
        input$geo_level,
        highlighted_geo()
      )
      data_table_for_report(as.data.frame(t$x$data))
      return(t)
    }
  })
  
  # Observes click in cov_table
  observeEvent(input$rows, {
    
    if (geo_highlighted()){
      geo_highlighted(FALSE)
      highlighted_geo(NULL)
    } else{
      geo_highlighted(TRUE)
      highlighted_geo(input$rows)
      changed_geo(TRUE)
    }
    
  })
  
  
  # Model interpretation
  output$mod_interpretation <- renderUI({
    HTML("<b>Show Model</b>")
    if (output_vals$main_cov != ""){
      
      # Convert state abbreviation to full names
      geo_name <- stri_replace_all_regex(geo_name(), 
                                         pattern = state.abb, 
                                         replacement = state.name, 
                                         vectorize = FALSE)
      
      # Get the output from building the interpretation
      # write_interpretation() now returns an array that has a boolean to determine if coefficient of main_cov is significant
      # and a series of strings to write the interpretation paragraph. The strings are broken up so that the tooltips could
      # be inserted in the appropriate places.
      interpret_info <- write_explanation(geo_dat_btn_update()$dat, 
                                          output_vals$main_cov, 
                                          mod_outcome, geo_name(), 
                                          geo_unit(),
                                          highlighted_geography = highlighted_geo())
      
      definition_to_print <- 
        covariate_definitions[
          covariate_definitions$Factor == col_to_pretty[output_vals$main_cov],
        ]
      
      # Determine whether main_cov was significant or not
      sig <- interpret_info[1]
      
      if (is.na(sig)) {
        div(
          HTML(
            interpret_info[2]
          )
        )
      }
      else if (sig) {
        # Main_cov coefficient IS significant
        div(
          HTML(str_sub(interpret_info[2], 1, nchar(interpret_info[2])-4)), #remove the </p> tag
          #add tooltip for meaning of significance
          popify(
            shiny_iconlink(),
            title = NULL,
            content = "<p>A significant association here is defined as having a p-value less than 0.05. The p-value is the probability of obtaining a t test statistic as extreme or more extreme as was observed from sample against the null hypothesis that there is no relationship (0 value for the coefficient) between the main factor and Asthma Prevalence.</p>",
            placement = "right",
            trigger = "hover",
            options = list(container = "body")),
          if (!is.null(highlighted_geo())){
            HTML(interpret_info[3])
          },
          HTML("</p>") #add </p> tag back
        )
        
      } else {
        # Main_cov coefficient is NOT significant
        div(
          HTML(interpret_info[2]),
          if (!is.null(highlighted_geo())){
            HTML(interpret_info[4])
          },
          HTML(str_sub(interpret_info[3], 1, nchar(interpret_info[3])-4)), #remove the </p> tag
          #add tooltip for how to interpret non-significance
          popify(
            shiny_iconlink(),
            title = NULL,
            content = "<p>Non-significant associations can occur for multiple reasons:</p><ul><li>There might not be any relationship between your main factor and asthma within the scope you’ve chosen.</li><li>There might be confounding by one of your selected factors with your main factor and Asthma Prevalence.</li><li>There might be a mediating relationship between your selected factors and main factor with Asthma Prevalence.</li></ul><p>If you observe a non-significant association between your main factor and Asthma Prevalence, is there another factor that might be more important to Asthma Prevalence than the one you’ve chosen? If so, you can create a new model by clicking on the <b>[Reset Inputs]</b>.</p>",
            placement = "right",
            trigger = "hover",
            options = list(container = "body")),
          HTML("</p>") #add tag back
        )
        
      }
    
    
    
  } 
    })
  
    
  # Format data to download
  output$download_data <- downloadHandler(
    filename = function() {

      # Name of file: <state/county/place>_<geo unit>_asthma_equity_data_<MMDDYYYY>.csv
      paste(geo_name(), "_", geo_unit(), "_asthma_equity_data_", gsub("-", "", Sys.Date()), ".csv", sep="")
    },
    content = function(file) {
      dat <- geo_dat_btn_update()$dat  # Data of interest
      
      # Make sure column names are pretty
      rename_cols <- which(!is.na(match(colnames(dat), intersect(colnames(dat), pretty_to_col))))
      colnames(dat)[rename_cols] <- col_to_pretty[colnames(dat)[rename_cols]]
      
      # Write
      # TODO: Add data dictionary as a separate sheet? Add units to headers.
      write.csv(dat, file, row.names = FALSE)
    }
  )  
  
  
  # Format data for report
  output$download_report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      paste0(gsub(", ", "_", geo_name()), 
             "_",
             gsub(" ", "_", col_to_pretty[output_vals$main_cov]),
             "_report_", 
             gsub("-", "", Sys.Date()) ,
             ".doc")
    },
    content = function(file) {
      asthma_available <- if (!(mod_outcome %in% colnames(geo_dat_btn_update()$dat))) {
        FALSE
      } else {
        TRUE
      }
      
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      # Temporary dir for file saving and report generation
      tempReport <- normalizePath(tempdir())
      newTempReport <-
        if (asthma_available) {
          file.path("markdown",
                    "Dashboard_Report",
                    "report.Rmd")
        } else {
          file.path("markdown",
                    "Dashboard_Report",
                    "report_no_asthma.Rmd")
        }
        
      file.copy(newTempReport, file.path(tempReport, "report.Rmd"), overwrite = TRUE)
      file.copy(file.path(
        "markdown",
        "Dashboard_Report",
        "report_template.docx"
      ), file.path(tempReport, "report_template.docx"), overwrite = TRUE)
      
      id <- showNotification(
        "Rendering report...", 
        duration = NULL, 
        closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)
      
      # Get number of geographic units available for selection
      if (output_vals$geo_level == "State (County level data)") {
        # Get number of counties in state (with nonempty data)
        dat_sub <- geo_dat_btn_update()
        number_str <-
          length(
            which(!is.na(
              dat_sub$dat[[output_vals$main_cov]]
            ))
          )
      } else if (output_vals$geo_level == "County (Census-tract level data)") {
        # Get number of census tracts in county (with nonempty data)
        number_str <- if (output_vals$county == "") {
          ""
        } else {
          dat_sub <- geo_dat_btn_update()
          length(
            which(!is.na(
              dat_sub$dat[[output_vals$main_cov]]
            ))
          )
        }
      } else if (output_vals$geo_level == "Place (Census-tract level data)") {
        # Get number of census tracts in place (with nonempty data)
        number_str <- if (output_vals$place == "") {
          ""
        } else {
          dat_sub <- geo_dat_btn_update()
          length(
            which(!is.na(
              dat_sub$dat[[output_vals$main_cov]]
            ))
          )
        }
      }
      
      
      # Convert state abbreviation to full names
      geo_str <- 
        stri_replace_all_regex(geo_name(), 
                               pattern = state.abb, 
                               replacement = state.name, 
                               vectorize = FALSE)
      
      
      # Save map as png
      ggsave(
        plot = visuals_for_report$map$rank_plot,
        filename = file.path(tempReport, "map_rank.png"),
        width = 12, height = 9
      )
      ggsave(
        plot = visuals_for_report$map$plot,
        filename = file.path(tempReport, "map.png"),
        width = 12, height = 9
      )
      
      # Save scatter plot as png
      ggsave(
        plot = visuals_for_report$scatter$plot,
        filename = file.path(tempReport, "scatter.png"),
        width = 12, height = 9
      )
      ggsave(
        plot = visuals_for_report$scatter$rank_plot,
        filename = file.path(tempReport, "scatter_rank.png"),
        width = 12, height = 9
      )
      
      # File paths to grab them from
      map_path <- file.path(tempReport, "map.png")
      map_rank_path <- file.path(tempReport, "map_rank.png")
      scatter_path <- file.path(tempReport, "scatter.png")
      scatter_rank_path <- file.path(tempReport, "scatter_rank.png")
      
      # Make sure to get the appropriate column names from the table
      factor_col <- colnames(data_table_for_report())[
        stri_detect_fixed(tolower(colnames(data_table_for_report())),
                   tolower(col_to_pretty[output_vals$main_cov]))
        ]
      asthma_col <- colnames(data_table_for_report())[
        str_detect(tolower(colnames(data_table_for_report())),
                   "asthma")
        ]
      
      
      # Histograms
      g_factor <- histogram_for_report(data_table_for_report(),
                                factor_col)
      g_asthma <- if (!asthma_available) {
        ggplot()
      } else {
        histogram_for_report(data_table_for_report(), asthma_col)
      }
      
      ggsave(
        plot = g_factor,
        filename = file.path(tempReport, "hist_factor.png")
      )
      ggsave(
        plot = g_asthma,
        filename = file.path(tempReport, "hist_asthma.png")
      )
      
      
      # Model interpretation
      # Convert state abbreviation to full names
      geo_name <- stri_replace_all_regex(geo_name(), 
                                         pattern = state.abb, 
                                         replacement = state.name, 
                                         vectorize = FALSE)
      interpret_info <- if (output_vals$main_cov != ""){
        # Get the output from building the interpretation
        # write_interpretation() now returns an array that has a boolean to determine if coefficient of main_cov is significant
        # and a series of strings to write the interpretation paragraph. The strings are broken up so that the tooltips could
        # be inserted in the appropriate places.
        write_explanation(geo_dat_btn_update()$dat, 
                                            output_vals$main_cov, 
                                            mod_outcome, geo_name(), 
                                            geo_unit(),
                                            highlighted_geography = highlighted_geo())
      } else { "" }
      
      
      # Set up parameters to pass to Rmd document
      params <- if (asthma_available) {
        list(
          "geo_name" = geo_str,
          "geo_unit" = geo_unit(),
          "num_geo" = paste(number_str, " available ", geo_unit()),
          "factor" = col_to_pretty[output_vals$main_cov],
          "map_path" = map_path,
          "map_rank_path" = map_rank_path,
          "scatter_path" = scatter_path,
          "scatter_rank_path" = scatter_rank_path,
          "data_table" = data_table_for_report(),
          "hist_factor" = file.path(tempReport, "hist_factor.png"),
          "hist_asthma" = file.path(tempReport, "hist_asthma.png"),
          "factor_units" = get_units(output_vals$main_cov),
          "factor_def" = definition_to_print(),
          "interpret_info" = interpret_info[2],
          "dat_info" = dat_info,
          "factor_og_name" = output_vals$main_cov,
          "selection_sentence" = visuals_for_report[["selection_sentence"]]
        )
      } else {
        list(
          "geo_name" = geo_str,
          "geo_unit" = geo_unit(),
          "num_geo" = paste(number_str, " available ", geo_unit()),
          "factor" = col_to_pretty[output_vals$main_cov],
          "map_path" = map_path,
          "data_table" = data_table_for_report(),
          "hist_factor" = file.path(tempReport, "hist_factor.png"),
          "factor_units" = get_units(output_vals$main_cov),
          "factor_def" = definition_to_print(),
          "interpret_info" = interpret_info[2],
          "dat_info" = dat_info,
          "factor_og_name" = output_vals$main_cov,
          "selection_sentence" = visuals_for_report[["selection_sentence"]]
        )
      }
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(newTempReport, output_file = file,
                        params = params,
                        output_format = "word_document",
                        output_options = list(reference_docx = "report_template.docx"),
                        envir = new.env(parent = globalenv())
      )
    })
  
}

# Execute app ----
shinyApp(ui = ui, server = server)