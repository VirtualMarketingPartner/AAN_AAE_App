# Update all Data
# By Hannah De los Santos, Karen Jiang, Julianna Bernardi, AJ Liberatore
# Originated on: 12/20/22

# Requires the following package dependencies. Uncomment the following lines (remove the # from beginning of the lines).

install.packages("devtools")
devtools::install_github("mitre/ACAGPM", ref = "main") # ACAGPM is only available through github.
install.packages("broom")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("parallel")
install.packages("raster")
install.packages("readxl")
install.packages("rgdal")
install.packages("rgeos")
install.packages("rstudioapi")
install.packages("sp")
install.packages("sf")
install.packages("stringr")
install.packages("tictoc")
install.packages("tidycensus") # must install API key to use tidycensus.
install.packages("tigris")


# census_api_key('YOUR API KEY HERE') # Request API key from https://api.census.gov/data/key_signup.html

# Packages
library(ACAGPM)
library(broom)
library(dplyr)
library(ggplot2)
library(parallel)
library(raster)
library(rgdal)
library(rgeos)
library(sp)
library(sf)
library(stringr)
library(tictoc)
library(tidycensus)
library(tigris)

# set working directory - only works in RStudio (with rstudioapi)

data_folder <- file.path(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "data")

# data_folder <- file.path(gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
#                          "H",
#                          "Data",
#                          "H")

# Set Working Directory to this current document
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("01_update_sources.R") # Only update this file!
source("02_helper_functions.R")
# source("03_geo_prep.R") # This file does not need to be re-run
source("04_load_data.R")
source("05_process_and_save.R")