# Redlining Data Update
Last Updated: 11/13/24

This README explains the general process of updating the redlining data (All_Population_Weighted_HOLC_Grades_pt2_Threshold.csv).

## Background

The redlining data is a mapping of the original redlined neighborhoods, which have been digitized by [Mapping Inequality](https://dsl.richmond.edu/panorama/redlining/), to census tracts -- in this case, using 2020 boundaries. Note that this data updates relatively infrequently, rarely includes large changes, and includes cities that have not been "graded" by HOLC standards (which results in a letter grade A - D for each neighborhood). Only cities with that grading system are included. Updates largely seem to consist of adding new cities.

This mapping is done using the Population Weighted, 20% Threshold method from our publication [Evaluating Methods for Mapping Historical Redlining to Census Tracts for Health Equity Research](https://link.springer.com/article/10.1007/s11524-024-00841-3#citeas). [Link for shareable, readable paper.](https://rdcu.be/dCbRH) This README walks through the general steps of executing that method, practically.

Additionally with that paper, we released a package called [{holcmapr}](https://github.com/mitre/holcmapr) that allows users to view and compare these different methodologies through a streamlined application. Much of the code and methodology used in this README is reflected from that package, and there will be many references to it throughout. This README references v1.0.0 of that package ([release notes here](https://github.com/mitre/holcmapr/releases/tag/v1.0.0)).

## Step 1: Data Download and Loading Libraries

For several of the following steps, we'll be relying on the [redlining_preprocessing.R](https://github.com/mitre/holcmapr/blob/main/inst/Data_Manipulation/redlining_preprocessing.R) script from the {holcmapr} package, an internal script for preprocessing data used in the package.

In order to download the updated data, go to "Download the Data" on Mapping Inequality's website [here](https://dsl.richmond.edu/panorama/redlining/data). The methodologies below assume the GeoJSON version of that data (mappinginequality.json).

Once you've downloaded that, run lines 1 - 66 (the "load data and libraries" section) of redlining_preprocessing.R. You may have to change file paths to be relative to your workspace. To run this, you will also need to download:

- Scripts:
  - [R/redlining_global_var.R](https://github.com/mitre/holcmapr/blob/main/R/redlining_global_var.R)
  - [R/redlining_map_functions.R](https://github.com/mitre/holcmapr/blob/main/R/redlining_map_functions.R)
- Data:
  - [inst/extdata/All_US_cities.csv](https://github.com/mitre/holcmapr/blob/main/inst/extdata/All_US_cities.csv)
  - [inst/extdata/Specific_US_cities.csv](https://github.com/mitre/holcmapr/blob/main/inst/extdata/Specific_US_cities.csv)
  - [inst/extdata/CenPop2020_Mean_TR.csv](https://github.com/mitre/holcmapr/blob/main/inst/extdata/CenPop2020_Mean_TR.csv)
- Libraries (install in R using `install.packages()`):
  - tidycensus
  - sf

More information about datasets used can be found [here](https://github.com/mitre/holcmapr/tree/main/inst/extdata) and in the general {holcmapr} README.

## Step 2: Initial Preprocessing

This is where you'll do the bulk of the processing, and what will take you the most time. When I was making the update for 2024, this took me 4 - 6 hours (with starts and stops depending on bugs, more on that below). There are some things to consider here.

It's likely previously existing redlined cities will not have updated (alluded to in the "Background" section). Only those that are added will be new. Further, new cities may not be HOLC-graded on a A - D scale (these will have been filtered out in lines 25 - 41). In order to cut processing time, you may want to edit line 72 of the processing script to be a vector of cities that were updated (in the same City/State format).

This will likely require some stop/start updating over the course of this, due to the fact that redlined cities as they're encoded by Mapping Inequality may not have the same names as those in "All_US_cities.csv", which searches for city/state combinations in order to find the counties of tracts to download for that city. In those cases:
- Look at the city name that has thrown an error. You may need to check the Mapping Inequality website to see what that city is. Examples of cities that have not matched in the past include "St. Cloud" (which was encoded in our data as Saint Cloud), "Holyoke Chickopee" (which is two cities), and "Bergen Co." (which is a county, and not a city).
- Find out which county(-ies) the redlined area falls into (the Mapping Inequality website helps to view the area -- then looking at county maps for the state(s) through crossing Google Maps, Wikipedia articles, generally googling, etc.) that it falls into.
- Add row(s) to the file "Specific_US_Cities.csv" with:
  - the Mapping Inequality file's exact city name under the column "place.name"
  - the Mapping Inequality file's exact state name under the column "state.code"
  - the 3-digit county code for the county that it corresponds to. If the city is in multiple counties, add multiple rows for each county. Existing examples of this in the file include "St. Joseph" and "St.Paul"
Once you've made those changes, the cities should work and you can continue on.

Now, with these considerations in mind, run lines 68 - 173 of the processing script. Once that is complete, I would recommend running lines 178 - 179, which will output contents in the console. Each should have values -- if not, something went wrong in processing, perhaps due to one of the considerations above.

After this section is complete, you will have `redlining_info` a list-type value that contains the preprocessed information for each city. I would recommend saving the workspace at this point in order not to lose information.

## Step 3: Final Data Output

Once your data has been preprocessed, you need to calculate the redlining grade for each track using the population-weighting, 20% threshold method. This will utilize {holcmapr}'s `test_assignment()` function (an internal function for the package -- it will be in your workspace from the previous preprocessing).

Run the following code, which will calculate the values for each tract in all cities, then write it out to the file "All_Population_Weighted_HOLC_Grades_pt2_Threshold.csv":

```
all_redlining_df <- data.frame()
for (cts in names(redlining_info)){
  print(paste0("[", Sys.time(), "] ", cts))

  city <- strsplit(cts, "/")[[1]][1]
  st <- strsplit(cts, "/")[[1]][2]

  # census tract ----
  print(paste0("[", Sys.time(), "] Getting census tracts..."))

  ct <- redlining_info[[cts]]$ct

  # census block ----
  print(paste0("[", Sys.time(), "] Getting census blocks..."))

  # get census block data, but cut it down -- we don't need all of it
  cb <- redlining_info[[cts]]$cb

  # get census areas and populations ----
  print(paste0("[", Sys.time(), "] Getting census areas and populations..."))

  c_area_pop <- redlining_info[[cts]]$c_area_pop

  intr_df <- test_assignment(city, st, ct, cb, "prop_pop_20thr", c_area_pop)
  intr_df <- add_threshold(intr_df, "prop_pop_20thr")
  intr_df$city <- city
  intr_df$state <- st
  colnames(intr_df)[colnames(intr_df) == "prop_pop_20thr"] <- "holc_grade_pop"

  intr_df <- intr_df[, c("city", "state", "GEOID", "holc_grade_pop")]

  all_redlining_df <- rbind(all_redlining_df, intr_df)

  rm(cb)
}

all_redlining_df$holc_grade_pop[is.nan(all_redlining_df$holc_grade_pop)] <- NA

write.csv(all_redlining_df, "All_Population_Weighted_HOLC_Grades_pt2_Threshold.csv", na = "", row.names = F)
```

Note that if you took the suggestion above of only running the redlining processing for cities which have changed, you will want to append the resulting All_Population_Weighted_HOLC_Grades_pt2_Threshold.csv to your previous version.

Then you should be all set to use the redlining data!

