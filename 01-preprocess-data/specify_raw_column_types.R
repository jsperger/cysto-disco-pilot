# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)

# Create the spec string
spec_string <- "
cols(
  Site = col_character(),
  `Intervention assigned` = col_character(),
  `Intervention actually completed` = col_character(),
  Age = col_double(),
  `Time (lidocaine instilled) - use military time` = col_time(format = ''),
  `Time (cystoscopy started) -use military time` = col_time(format = ''),
  `Time elapsed (h:mm)` = col_time(format = ''),
  `Blue light used?` = col_character(),
  `Music?` = col_character(),
  `If no music, why?` = col_character(),
  `If music, type?` = col_character(),
  `Visualization?` = col_character(),
  `If no visualization, why?` = col_character(),
  `Family members/friends present in the room?` = col_character(),
  `Hydrostatic pressure applied?` = col_character(),
  `Cystoscopy result` = col_character(),
  `Reason for cystoscopy - specific` = col_character(),
  `Reason for cystoscopy - general` = col_character(),
  `Number of prior cystoscopies` = col_double(),
  `Months since last cystoscopy` = col_character(),
  `Intravesical therapy in last 3 months?` = col_character(),
  `Nurse/MA/Staff` = col_character(),
  `Challenges encountered before and during cystoscopy (include specifics regarding intervention)` = col_character(),
  Gender = col_character(),
  Race = col_character(),
  Ethnicity = col_character(),
  `PROMIX Anxiety: I felt fearful (1-5)` = col_double(),
  `PROMIS Anxiety: I found it hard to focus on anything other than my anxiety (1-5)` = col_double(),
  `PROMIS Anxiety: My worries overwhelmed me (1-5)` = col_double(),
  `PROMIS Anxiety: I felt uneasy (1-5)` = col_double(),
  `PROMIS Pain: Pain at its worst (1-5)` = col_double(),
  `PROMIS Pain: Pain on average (1-5)` = col_double(),
  `PROMIS Pain: Current pain (1-5)` = col_double(),
  `VAS Score: Pain at its worst (0-10)` = col_double(),
  `If you were to undergo cystoscopy again, would you use the same methods to manage pain or discomfort? (1-4)` = col_character(),
  `In an overall, general sense, how satisfied are you with the way in which you received cystoscopy today with regard to pain or discomfort? (1-4)` = col_character(),
  `To what extent did the way in which cystoscopy was performed meet your needs for managing pain or discomfort? (1-4)` = col_character(),
  `If there was a study that tests ways to reduce pain or discomfort associated with cystoscopy, would you consider hearing more and participating if it did not require additional visits or procedure?` = col_character()
)"

# Define a function to extract the column spec into a data frame
extract_spec <- function(spec_string) {
  # Use regex to match the column spec
  spec_list <- str_match_all(spec_string, "`?([^`=]+)`?\\s*=\\s*(col_[^()]+)\\(")[[1]]

  # Create a data frame
  spec_df <- as.data.frame(spec_list[, -1, drop = FALSE], stringsAsFactors = FALSE)
  names(spec_df) <- c("Colname", "InitialGuessSpec")

  # Map the InitialGuessSpec to a single character abbreviation
  spec_map <- setNames(c('l', 'i', 'd', 'c', 'f', 'D', 't', 'T', 'n', '_', '?'),
                       c('col_logical', 'col_integer', 'col_double', 'col_character',
                         'col_factor', 'col_date', 'col_time', 'col_datetime',
                         'col_number', 'col_skip', 'col_guess'))

  spec_df$InitialGuessSpecChr <- spec_map[spec_df$InitialGuessSpec]

  return(spec_df)
}

revised_spec_string <- 'cols(
  Site = col_factor(),
  `Intervention assigned` = col_character(),
  `Intervention actually completed` = col_character(),
  Age = col_integer(),
  `Time (lidocaine instilled) - use military time` = col_time(format = ""),
  `Time (cystoscopy started) -use military time` = col_time(format = ""),
  `Time elapsed (h:mm)` = col_time(format = ""),
  `Blue light used?` = col_factor(),
  `Music?` = col_factor(),
  `If no music, why?` = col_character(),
  `If music, type?` = col_character(),
  `Visualization?` = col_factor(),
  `If no visualization, why?` = col_character(),
  `Family members/friends present in the room?` = col_factor(),
  `Hydrostatic pressure applied?` = col_factor(),
  `Cystoscopy result` = col_character(),
  `Reason for cystoscopy - specific` = col_character(),
  `Reason for cystoscopy - general` = col_character(),
  `Number of prior cystoscopies` = col_integer(),
  `Months since last cystoscopy` = col_character(),
  `Intravesical therapy in last 3 months?` = col_factor(),
  `Nurse/MA/Staff` = col_factor(),
  `Challenges encountered before and during cystoscopy (include specifics regarding intervention)` = col_character(),
  Gender = col_factor(),
  Race = col_factor(),
  Ethnicity = col_factor(),
  `PROMIX Anxiety: I felt fearful (1-5)` = col_integer(),
  `PROMIS Anxiety: I found it hard to focus on anything other than my anxiety (1-5)` = col_integer(),
  `PROMIS Anxiety: My worries overwhelmed me (1-5)` = col_integer(),
  `PROMIS Anxiety: I felt uneasy (1-5)` = col_integer(),
  `PROMIS Pain: Pain at its worst (1-5)` = col_integer(),
  `PROMIS Pain: Pain on average (1-5)` = col_integer(),
  `PROMIS Pain: Current pain (1-5)` = col_integer(),
  `VAS Score: Pain at its worst (0-10)` = col_double(),
  `If you were to undergo cystoscopy again, would you use the same methods to manage pain or discomfort? (1-4)` = col_factor(),
  `In an overall, general sense, how satisfied are you with the way in which you received cystoscopy today with regard to pain or discomfort? (1-4)` = col_factor(),
  `To what extent did the way in which cystoscopy was performed meet your needs for managing pain or discomfort? (1-4)` = col_factor(),
  `If there was a study that tests ways to reduce pain or discomfort associated with cystoscopy, would you consider hearing more and participating if it did not require additional visits or procedure?` = col_factor()
)
'

# Extract the spec into a data frame
spec_df <- extract_spec(spec_string)

revised_spec <- extract_spec(revised_spec_string) %>%
  rename(ColnameUsedForRevision = Colname,
    RevisedSpecChr = InitialGuessSpecChr) %>%
  select(ColnameUsedForRevision, RevisedSpecChr)

spec_info <- cbind(spec_df, revised_spec)

write_csv(spec_info, "spec_info.csv")