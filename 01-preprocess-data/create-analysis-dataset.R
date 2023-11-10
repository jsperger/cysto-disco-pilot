# Source script containing data cleaning functions
source("01-preprocess-data/clean-data.R")

# Load data
raw_col_spec <- readr::read_csv("01-preprocess-data/raw_spec_info.csv")

cysto_raw <- readr::read_csv("data/cysto_qi_pilot.csv",
                             col_types = paste0(raw_col_spec$RevisedSpecChr, collapse = ""))

cysto_cleaned <- CleanInputData(cystodf = cysto_raw)

# Write to file
readr::write_csv(cysto_cleaned, "./data/cleaned_cysto_qi_pilot.csv")