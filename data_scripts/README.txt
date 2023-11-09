README: Data Scripts

This folder hold the scripts which are used to update all original data sources used to generate the processed data for the app. 
IMPORTANT! Before updating the data, read through this document to make you understand the process and relationships between each script file.

The following libraries are required.

- ACAGPM
- broom
- dplyr
- ggplot2
- parallel
- raster
- readxl
- rgdal
- rgeos
- rstudioapi
- sp
- sf
- stringr
- tictoc
- tidycensus
- tidyr
- tigris

`update_all_data.R`: Master script. 
- This is the master script which will run all other scripts in the correct order to fully process the data.
- Before running this master script, the only script you will need to update is `01_update_sources.R`. Further instructions for downloading and organizing all data can be found in the repository README.Rmd


`01_update_sources.R`: File pathing script.
- Script to update all the file paths and file names for future data ingests.
- Defines `data_folder` directory path
- **You only need to update this file to accomodate future data updates!!

`02_helper_functions.R`: All helper functions.
- Contains main data processing function, and all helper functions.
- Main function is get_merged_shp; all other functions utilized within.
- get_merged_shp returns the geographic info and available data for a selected 
geography as a list containing data.frame (dat) and SpatialPolygonsDataFrame 
(shp) objects.

`03_geo_prep.R`: Prepares Places Geography file
- Creates the places shapefile datasets used within the app. 
- File execution takes around 10-15 minutes, but only needs to be re-run with the decennial update (every 10 years). 
- The code does not need to be updated, as the functions which pull the shapefiles from tigris (tigris::tracts(), tigris::places(), tigris::states() ), default to using the most recent decennial data year.
- **Running this file is optional! The data generated from this file is already provided.

`04_load_data.R`: Preprocesses data files and loads them into environment
- Loads data from various sources in `Supporting_Files` folder
- Processes any data based on 2010 census tract geographies to convert to 2020 census tract geographies
- Prepares single `all_data.RData` to be used for parallel processing
- ACAG PM data not available for AK and HI; uses EPA data for state and county 
levels.

`05_process_and_save.R`: Main execution file.
- Saves data to data_folder directory, specified at the top of the file.
- Compiles all packages, directories, and preprocessed data necessary for data 
pull, then pulls national state data, county data by state, tract data by county, 
and tract data by place.
- File execution takes multiple hours (depends on number of processors in computer, with Macbook Pro )
- Saves 51 files in county folder, 1032 files in the places folder, and 3142 files in the tracts folder


