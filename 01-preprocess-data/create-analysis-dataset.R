# Source script containing data cleaning functions
source("01-preprocess-data/clean-data.R")

# Load data
raw_col_spec <- readr::read_csv("01-preprocess-data/raw_spec_info.csv")

cysto_raw <- readr::read_csv("data/cysto_qi_pilot.csv",
                             col_types = paste0(raw_col_spec$RevisedSpecChr, collapse = ""))

cysto_cleaned <- CleanInputData(cystodf = cysto_raw)

cysto_treated <- cysto_cleaned %>%
  dplyr::filter(TrtMusic == TRUE | TrtViz == TRUE)

# Filter to only those whose received treatment matches their assigned treatment
per_protocol <- cysto_treated %>%
  dplyr::filter(LidoAssignedReceivedAgree == TRUE & AsAssignedMusic == TRUE & AsAssignedViz == TRUE)

# Something is off about the visualization assignment.
per_protocol_ignoring_viz <- cysto_treated %>%
  dplyr::filter(LidoAssignedReceivedAgree == TRUE & AsAssignedMusic == TRUE)

# Write to file
readr::write_csv(cysto_cleaned, "./data/cleaned_cysto_qi_pilot.csv")