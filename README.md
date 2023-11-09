## Preparation
Most of the info related to updating data sources and the layout of the app is in r_readme.md. 
A few things to note:
- RStudioConnect does not seem to upload directories in which there are more than 1,000 files. The error is vague. `consolidate_state.py` can be run (update directory within) in order to consolidate tract and place data (`Data/tract`,`Data/place`) into their appropriate states to prevent too many files in any given directory.