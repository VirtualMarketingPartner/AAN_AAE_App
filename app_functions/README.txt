## Application Functions

The Asthma Equity Explorer Dashboard relies on various scripts to run as expected. These scripts, located in the `app_functions` directory, serve multiple purposes, including the configuration of globabl variables, defining functions used to create the many visualizations in the app, calculating model outcomes, as well as an assortment of other helper functions. The following table provides a short summary of each:

+---------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------+
| Script                    | Description                                                                                                                                       |
+---------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------+
| directory_config.R        | Initializes the main file path directory to supporting data and the reference data frame (dat_info) variable                                      |
+---------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------+
| visualization_functions.R | Contains plotting constants (color palettes, reference data, etc.) and plotting functions used in the app (leaflet map, model scatterplots, etc.) |
+---------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------+
| ui_functions.R            | Contains custom UI component functions, including MITRE-specific logos and themes to the application.                                             |
+---------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------+

In order for the application to work properly, it is important that the file path defined in the `data_folder` variable in `directory_config.R` is correct. This file path should point to the location of all data for the application (see the application README for more details). This should be taken care of automatically, but should be verified. If the user has not configured their repository correctly, this script will not run properly.

Below is a short description of the functions in `app_functions/`:

visualization_functions.R
=========================
- Plotting Constants
	- holc_colors
	- holc_points
	- MITRE themes and colors
- Rank Functions
	- get_rank: Function to calculate the rank of each geographic unit
- Styling Helper Functions
	- style_word: Function to get stylized word
	- get_color_palette: Generates a color palette for plotting that matches the chosen display color of a variable
- Map Functions
	- get_units: Function to get units of a variable
	- addLegendCustom: Function for size legends in leaflet
	- build_map: Function to initialize all mapping variables
	- build_leaflet_map: Function to create the interactive leaflet map in the application
	- build_ggplot_map: Function to create the ggplot map in the application report
- Scatterplot Functions
	- build_cov_outcome_scatterplot: Initialize all variables to build the outcome vs. covariate scatterplot
	- build_plotly_scatter: Function to create the interactive plotly outcome vs. covariate scatterplot in the application
	- build_ggplot_scatter: Function to create the ggplot outcome vs. covariate scatterplot in the application report
- Table Functions
	- build_cov_table: Function to build a table of covariates and outcome
- Explanation Functions
	- write_explanation: Function to build explanation of relationship between variables
- Processing Functions
	- get_avail_geographies: Function to get available geographies for the application
	- geo_name_func: Function to create a "pretty" name for the given geographic input from the application
	- geo_unit_func: Function to get the geographic unit of the minor geographies at the chosen geographic level (state, county, place) from the application
- Report Visualization Functions
	- histogram_for_report: Function to build a histogram of the covariate and outcome for the application report


ui_functions.R
==============
- customModal: Function to create modal used on loadup and geographic selections in application
- MITREfooter: Function that returns the custom HTML for the MITRE copyright in application footer
- MITREnavbarPage2: Function to create the navigation bar at the top of the application, including the addition of logos