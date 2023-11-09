## Preparation
- Most of the info related to updating data sources and the layout of the app is in `r_readme.md`. 
- Also check out readme in `data_scripts/README.txt`
- Ensure all files in `Data\Supporting_Files` are up to date.
## Processing
- The scripts in `data_scripts` are to be run sequentially.
- `data_scripts/05_process_and_save.R` may error out when collecting local data. The process in retrieving the data is parallelized so errors are generally not useful. The comments between these two function calls in this fifth script can be swapped in order to run through the localities linearly in order determine where the error exists at the expense of speed:
```
parLapply(counties$county,
          fun = save_county_rds,
          state = state,
          cl = clust)
# lapply(counties$county,save_county_rds,state = state)
```
- In most cases, there is a mismatch in the name of the locality. A hardcoded exception can be added in the `save_county_rds` function:
```
if (state == "SD" && county == "Shannon County") {
    return()
}
```
- In the above example, Shannon County was renamed to Oglala Lakota County at one point, and the originating data source still contained this name in addition to the new name.
## Deployment
A few things to note for deployment:
- The `data_folder` value in `app_function/directory_config.R` needs to be updated depending upon whether the app is to be run locally or on the shiny app server. There is probably more to be done here for optimization.
- RStudioConnect does not seem to upload directories in which there are more than 1,000 files. The error is vague. `consolidate_state.py` can be run (update directory within) in order to consolidate tract and place data (`Data/tract`,`Data/place`) into their appropriate states to prevent too many files in any given directory. **NOTE** that the scripts in `app_functions/ui_functions.R` and `app_functions\visualization_function.R` were adapted to depend on this state level directory structure and this must be set up as such even when running locally.
- When publishing, move the `Data/Supporting_Files` directory outside the project directory as there is a bundle size limit for deployement. The files contained within this directory can be both large and unneeded post data processing. There is probably an exclusion flag or some method available in rsconnect deployApp to be utilized in order to replace this method.