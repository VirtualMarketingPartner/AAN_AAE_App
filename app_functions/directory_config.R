# DIRECTORY CONFIG

# supporting data -- data folder
# data_folder <- normalizePath(file.path("..", "data"))

# check out here() library

data_folder <- file.path(
                        # gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 

                        ## SWAP IN BELOW WHEN DEPLOYING LOCALLY \/ \/ \/
                        # here::here(),
                        ## /\ /\ /\

                        ## SWAP IN WHEN DEPLOYING TO SHINY \/ \/ \/
                        getwd(),
                        "..",
                        ## /\ /\ /\

                         "Data"
                         )

dat_info_path <- file.path(
  data_folder, 
  "Supporting_Data",
  "column_name_map.csv"
)

# Reference data frame used by all scripts
dat_info <-
  read.csv(normalizePath(path = dat_info_path))
rownames(dat_info) <- dat_info$Colname

# creates a dictionary pointing between coded columns and names
pretty_to_col <- setNames(dat_info$Colname, dat_info$Pretty.Name)
col_to_pretty <- setNames(dat_info$Pretty.Name, dat_info$Colname)
