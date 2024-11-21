# Update Data Sources
# By Hannah De los Santos, Karen Jiang, Julianna Bernardi, AJ Liberatore
# Originated on: 12/12/22

# UPDATE FILENAMES -----------------

# IMPORTANT!!
# Use this file to update the file names and for any updated data sources, with quotation marks around the full file name, including the file extension (e.g. "File_1.csv")
# Copy and paste the file name, without the file path information, to the corresponding variable. 
# Make sure the EXACT spelling, capitalization, and punctuation are used.
# Once done, save this file. Update the data by running source(`update_all_data.R`) in the console.

# Detailed information on how to download this data can be found in the repository README.Rmd

# Year for the most recent 5-year ACS release from Census
acs_year <- 2020

# File name of data downloaded from CDC's PLACES - County Data (GIS Friendly Format)
places_county_fn <- "PLACES__County_Data__GIS_Friendly_Format___2024_release.csv"

# File name of data downloaded from CDC's PLACES - Census Tract Data (GIS Friendly Format)
places_censustract_fn <- "PLACES__Census_Tract_Data__GIS_Friendly_Format___2024_release.csv"

# File name of data downloaded from CDC's BRFSS - Age-Adjusted Prevalence data
brfss_state_fn <- "Behavioral_Risk_Factor_Surveillance_System__BRFSS__Age-Adjusted_Prevalence_Data__2011_to_present_.csv"

# File name of data downloaded from HUD Comprehensive Housing Affordability Survey
hud_fn <- "Table1 17-21.csv"

# File name of data downloaded from NEPHTN
nephtn_fn <- "Half_Mile_Park_Data.csv"

# File name of data downloaded from USDA Economic Research Services
usda_fn <- "FoodAccessDataProcessed2019.csv"

# File name of data downloaded from HRSA Federally Qualified Health Center 
fqhc_fn <- "Health_Center_Service_Delivery_and_LookAlike_Sites.csv"

# File name of data downloaded from NCES School Locations and Geoassignments
schools_fn <- "EDGE_GEOCODE_PUBLICSCH_2223.xlsx"