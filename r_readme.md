---
output:
  word_document: default
  html_document: default
---
# Asthma Equity Explorer

> Explore how social determinants of health impact your community's asthma outcomes

# Overview

The Asthma Equity Explorer is an interactive, population-level dashboard that applies health equity research to inform community-based asthma interventions within their unique social determinants of health landscapes. The Asthma Equity Explorer is built using the [Shiny](https://shiny.rstudio.com/) R package and publicly available data.

## Basic Usage

### Download R and R Studio

Download and Install R language for your specific operating system here: <https://cran.r-project.org/>

Download and Install RStudio here: <https://posit.co/download/rstudio-desktop/>

***Note:** Make sure to install any required packages, listed in [**Package Dependencies**](#package_dependencies)*.

To run the shiny application locally, clone the repository in the terminal and run the app in the console.

```         
# in terminal
git cd path/to/dir/location
git clone git@gitlab.mitre.org:health-equity-mip/asthma-dashboard-v3.git

# in console
shiny::runApp()
```

This repository contains the data, code, and documentation required to create and update the application.

### <a name="package_dependencies"></a>Package Dependencies

The following list of packages are required:

-   [ACAGPM](https://github.com/mitre/ACAGPM)
-   DT
-   RColorBrewer
-   broom
-   bsplus
-   dplyr
-   flextable
-   ggplot2
-   htmltools
-   htmlwidgets
-   leaflet
-   parallel
-   plotly
-   raster
-   rgdal
-   rgeos
-   rintrojs
-   sf
-   shiny
-   shinyBS
-   shinyWidgets
-   shinycssloaders
-   shinyjs
-   sp
-   stringi
-   stringr
-   testthat
-   tictoc
-   tidycensus
-   tigris

## Repository Structure

-   `app.R` (shiny application file that contains the code to run the app)

-   `data_scripts/` folder

    -   `01_update_sources.R` (This script will be used to help redirect all file pathing to assist with future data updates)
    -   `02_helper_functions.R` (This script reads in data from various sources and does initial cleaning and alignment across the various sources)
    -   `03_geo_prep.R` (This script prepares the geography place files, and does not need to be run when updating)
    -   `04_load_data.R` (This script reads in data, loads them into environment, and preprocesses them as needed)
    -   `05_preprocess_and_save.R` (This script processes and saves data to the `county`, `place`, and `tract` folders to increase application speed and efficiency on load)
    -   `update_all_data.R` (This is the master script which will run all other scripts in the correct order)
    -   `README.txt` (Provides more detailed documentation around each of the script functions)

-   `app_functions/` folder

    -   `visualization_functions.R` (This script is loaded into the app, and contains functions used for all visualization elements, including the maps, tables, and plots)
    -   `README.txt`(Provides more detailed documentation around the functions included in the `visualization_functions.R` script)

-   `www/` folder

    -   `app.css` (stylesheet for defining color, fonts, styling within the app)
    -   `MITRE_logo.png`

-   `markdown/` folder

    -   `about.rmd` (markdown document which provides information about the app, features, and data sources. This file generates about.html, which is loaded into the About tab within the app)
    -   `about.html` (output page from about.rmd which is loaded within the app)
    -   `report.rmd` (markdown document which generates downloadable reports in the app)
    -   `report_template.docx` (word template used to style the report.rmd)

-   `data/` folder

    -   NOTE: This folder will initially be empty when you clone the repository. We will provide a link to download all the data, and describe the data structure in more detail below.

## Data Structure

To initially set up the data to run this app, all data required to run the app can be downloaded from the [MITRE Asthma Equity Explorer Box](https://mitre.app.box.com/folder/188360326588) account.

The Box drive will contain the following folder and file structure, which should be replicated exactly in the repository.

-   `Supporting Files/` folder contains all the original data and is organized by the source data. This folder is where data for future updates will be sorted into (described further in the Data Updates section below).

-   `Supporting Data/` folder contains 3 .csv files which provide tooltip definitions, crosswalks, and other helper files and 3 .Rdata files which provide map overlays or pre-processed data used for parallelization.

    -   `covariates_definitions.csv`
    -   `column_name_map.csv`
    -   `data_processing_variable_map.csv`
    -   `fqhc_dat.Rdata`
    -   `public_school_dat.Rdata`
    -   `all_data.RData`

-   `county/` folder: contains 51 files encompassing state data at the county level.

-   `place/` folder: contains 1032 files encompassing place data at the census tract level.

-   `tract/` folder: contains 3142 files encompassing county data at the census tract level.

There are two options to obtain and copy the data from the `H` folder into the repository folder.

**Option 1:** Directly download all data from the Box drive into the `data` folder. This option will use the pre-processed data files, however, the initial download may be quite time intensive (5+ hours) depending on the speed of your internet connection.

We recommend this option for those who may be new to R.

**Option 2:** Download the `Supporting_Files` and `Supporting_Data` folders, and run the `update_all_data.R` script within the `data_scripts/` folder. This script option will create and write the output files according with the proper folder structure. This process may take up to a few hours (generally 2-4 hours), depending on the number of processers within your computer.

We recommend this option for users who may be involved with future data updates, and would like to familiarize themselves with the data scripts and update process. This does require some introductory knowledge of R.

# Data Updates

In the future, the publicly available data sources used in the application will become outdated and will likely have newly updated counterparts. This section goes through the process of accessing, downloading, and re-integrating updated data into the application

**Step 1:** Download the most recently released data sources from the following websites:

-   [CDC's PLACES: Local Data for Better Health data](https://chronicdata.cdc.gov/browse?category=500+Cities+%26+Places)
    -   Download the file with the title: PLACES: County Data (GIS Friendly Format) for the most recent release
    -   Download the file with the title: PLACES: Census Tract Data (GIS Friendly Format) for the most recent release
    -   Save these into `data/Supporting_Files/PLACES` into the `County` and `CT` folders respectively
-   [Behavorial Risk Factor Surveillance System (BRFSS) data](https://chronicdata.cdc.gov/browse?category=Behavioral+Risk+Factors)
    -   Behavioral Risk Factor Surveillance System (BRFSS) Prevalence Data
    -   Save this into `data/Supporting_Files/BRFSS` folder
-   [US Department of Housing and Urban Development (HUD) Comprehensive Housing Affordability Strategy (CHAS) Data](https://www.huduser.gov/portal/datasets/cp.html)
    -   Click Data tab at the bottom
    -   Data Year: `2015-2019 ACS 5-year average data` or most recent available
    -   Select Geographic Summary Level: `Census Tracts`
    -   Select File Type: `csv`
    -   Open downloaded zipped folder, and save the `Table 1.csv` and `CHAS data dictionary 15-19.xlsx` into `data/Supporting_Files/HUD` folder.
-   [National Environmental Public Health Tracking Network (NEPHT) Data](https://ephtracking.cdc.gov/DataExplorer/)
    -   This data source does not appear to be consistently maintained, as the data has not yet been updated since 2015.
-   [US Department of Agriculture (USDA) Economic Research Service Food Access Research Atlas](https://www.ers.usda.gov/data-products/food-access-research-atlas/download-the-data/)
    -   Download Current Version
    -   Save this into `data/Supporting_Files/USDA` folder.
-   [Health Resources and Services Administration (HRSA) Federally Qualified Health Centers Data](https://data.hrsa.gov/data/download)
    -   Click Health Center Service Delivery and Look--Alike Sites Download CSV option
    -   Save this into `data/Supporting_Files/HRSA` folder.
-   [National Center for Education Statistics (NCES) School Locations & Geoassignments](https://nces.ed.gov/programs/edge/geographic/schoollocations)
    -   Scroll down and click tab corresponding to the most recent release
    -   Download `Public School File` (\~25 MB)
    -   Open downloaded folder, and save `EDGE_GEOCODE_PUBLICSCH_####.xlsx` into `data/Supporting_Files/NCES`

**Step 2:** Ensure that all data are correctly moved to their corresponding source folders within the `data/Supporting_Files/` folder. Rename any files to avoid writing over prior files.

**Step 3:** Update `01_update_sources.R` script within the `data_scripts/` folder by changing all the file names (as shown in the blue text) to the updated file names. Save this file.

![Screenshot of the \`01_update_sources.R\` script which contains file names of various downloaded data](www/images/update_sources_screenshot.png){width="614"}

**Step 4:** Read through the `README.txt` within the `data_scripts` folder.

**Step 5:** Install any package dependencies by following the instructions listed in the `update_all_data.R` script within the `data_scripts/` folder. Run `update_all_data.R` by running the following command in the RStudio Console or by clicking the "Source" button

```         
setwd("path/to/asthma_dashboard_v3/data_scripts")
source(update_all_data.R)
```

**Step 6:** Check that the data in `data/county`, `data/place`, and `data/tract` folders have been updated.

# Contact & Attribution

This application was created with support from the MITRE Innovation Program by the Case Study for Health Equity Framework in Population Health Team.

For any questions or concerns, please contact Hannah De los Santos, PhD at [hdelossantos\@mitre.org](mailto:hdelossantos@mitre.org){.email} or Karen Jiang, MPH at [kjiang\@mitre.org](mailto:kjiang@mitre.org){.email}

Many thanks and tremendous gratitude to the MITRE team who helped develop this tool including:

-   Cassandra Okechukwu ScD, MSN, MPH

-   Julianna Bernardi

-   AJ Liberatore

-   Elizabeth Murphy MPH

-   Carla Bezold ScD MPH

-   Emilie Gilde MPP

-   Guhan Thuran

-   Kirbi Joe PhD

-   Sarah Ober MS

-   Meenu Ravi

-   Stephen Deese PhD
