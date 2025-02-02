# 27.08.2024
# Melissa Vanderheyden

# CSIall for females with a litter

# For now, data will be the copied CSI FRDs sheet from the affiliation file
# Aim is to import raw data and calculate CSI values in R rather than excel, 
# but for now we'll start like this... 

library (tidyverse)
library(readxl)
library(lubridate)
library(here)
library(writexl)

# 1. IMPORT DATA ---------------------------------------------------------------
excel_file_path <- here("data", "CSI_FRD_20241024.xlsx")
csi_data <- read_excel(excel_file_path, sheet = "Sheet1")

# Only include known females
csi_data <- csi_data %>%
  filter(dyadsex == "ff") %>%
  filter(Doublecounter == 1)

# Import pup-juv file
excel_file_path <- here("data", "pup_juv_20241028.xlsx")
litters <- read_excel(excel_file_path, sheet = "Sheet1")


# Make sure date format is ok
litters <- litters %>%
  mutate(date = ymd(date)) %>% 
  mutate(date = as.Date(date)) %>%
  separate_rows(mother, sep = "/") %>% # separate nova and croft
  # Remove litters without known mother
  filter(!(mother == "NA" | mother == "Unknown"))

# Adjust litter name for shared litter Croft
litters <- litters %>%
  mutate(name_of_litter = ifelse(mother == "Croft" & date == as.Date("2023-05-11"),
                                 "croft_may_23", name_of_litter))


# 2. DYADIC LITTER FILE --------------------------------------------------------

# Only maintain one row per litter
data <- litters %>%
  group_by(mother, date) %>%
  slice(1) %>%
  ungroup() %>%
  # To be sure that the data collection was more or less stable, let's only take
  # data after Jan23. 
  filter(date > "2022-07-01") %>%
  filter(estimation_error < 16)

# Change mother name into 4-character code
data <- data %>%
  mutate(mother = str_pad(tolower(substr(mother, 1, 4)), 
                          width = 4, side = "right", pad = "_"))
  
# Remove unused columns 
data <- data %>% select(mother, date, name_of_litter, den_location) %>%
  # Create new name of litter with short mother name
  mutate(litter_last = substr(name_of_litter, nchar(name_of_litter) - 5, nchar(name_of_litter))) %>%
  mutate(name_of_litter = paste(mother, litter_last, sep = '_')) %>%
  select (-litter_last) #Remove temporaty column


# Make data frame with dyadic data from all litters.
# Only retain mother dyads with a known CSI 

litter_combinations <- data %>%
  rename(litter1 = name_of_litter, mother1 = mother, date1 = date) %>%
  cross_join(data %>%
               rename(litter2 = name_of_litter, mother2 = mother, date2 = date)) %>%
  filter(mother1 != mother2) %>%  # Remove comparisons with self or siblings
  rowwise() %>%
  mutate(dyadcode = paste(sort(c(litter1, litter2)), collapse = ".")) %>%
  ungroup() %>%  # Ungroup after rowwise 
  
  # Create a code including both mothers in alphabetical order, to link w CSI data
  # concatenate names of mothers (4 letters) in alph order, divided by '.'
  mutate(motherdyad = apply(select(., mother1, mother2), 1, function(x) {
    # Sort alphabetically
    sorted_mothers <- sort(x)
    # Concatenate with '.'
    paste(sorted_mothers, collapse = ".")
  })) %>%
  # Remove duplicate litterdyads
  group_by(dyadcode) %>% 
  slice(1) %>%
  # calculate birth time difference
  mutate(time_diff_days = abs(as.numeric(date1 - date2))) %>%
  # Remove time differences larger than the cutoff value
  # with 1/2 of maximum of main inter-birth interval as cut-off
  filter(time_diff_days < 113)
  
  # with 1/2 of mean of main IBI as cut-off
  #filter(time_diff_days < 98) 

# 3. Remove cases where one litter is linked to the same to two litters from the same second mother ---------------
# 3.1 Repeats = 1 ---------------------------------------------------------- 
# In Repeats: Remain 0 if the dyadcode is unique. 
# Return 1 if the dyadcode repeats, except for the 6 characters at the end. 
repeated <- litter_combinations %>%
  mutate(repeats = 0) %>%
# Create a new column that removes the last 6 characters of 'dyadcode'
  mutate(truncated_dyadcode = substr(dyadcode, 1, nchar(dyadcode) - 6)) %>%
# Update 'repeats' column with 1 where 'truncated_dyadcode' repeats
  group_by(truncated_dyadcode) %>%
  mutate(repeats = ifelse(n() > 1, 1, 0)) %>%
  ungroup() 


# It's not strictly necessay to use the 'repeats' values. Could also directly 
# check if the dyadcodes are the same and then select the dyad with the shortes
# time difference, but this way it's possible to verify if everything is correct

# 3.2 Repeats = 2 -------------------------------------------------------------
# Return 2 if the dyadcode repeats, except for the 6 characters before the '.'
# Step 1: Split the dyadcode at the first dot (.) and move to two columns
repeated <- repeated %>%
  mutate(before_dot = sub("\\..*", "", dyadcode),  # Part before the dot
         after_dot = sub(".*\\.", "", dyadcode)) %>%   # Part after the dot
# Step 2: Create a new column with the part before the dot minus the last 6 characters
  mutate(truncated_before_dot = substr(before_dot, 1, nchar(before_dot) - 6)) %>%
# Step 3: Group by 'after_dot' and 'truncated_before_dot'. If a group has more than
# 2 rows, return '2' in repeats. 
  group_by(after_dot, truncated_before_dot) %>%
  mutate(repeats = ifelse(n() > 1, 2, repeats)) %>%  # Set repeats to 2 where condition holds and ensure it doesn't overwrite '1'
  ungroup() 


# 3.3 CLEAN UP based on 'Repeats' code -----------------------------------------

# If repeats = 1, group by 'truncated_dyadcode' and only retain the row with the smallest time_diff_days
# Create a new column 'keep_row' that marks the rows to retain within each group
repeated1 <- repeated %>%
  mutate (keep_row = if_else(repeats == 0, TRUE, FALSE)) %>%
  group_by(truncated_dyadcode) %>%
  mutate(keep_row = if_else(repeats == 1 & time_diff_days == min(time_diff_days), TRUE, keep_row)) %>%
  ungroup()

# If repeats = 2, group by 'truncated_before_dot' and only retain the row with the smallest time_diff_days
repeated2 <- repeated1 %>%
  group_by(after_dot, truncated_before_dot) %>%
  mutate(keep_row = if_else(repeats == 2 & time_diff_days == min(time_diff_days), TRUE, keep_row)) %>%
  ungroup() 


# Check dyads with the same number of days 

# For repeats = 1 group. 
# I don't explicitly filter for repeats == 1 or repeats == 2 to also include the 
# overlap between these two groups. It's rare but happens (e.g. Nova & Fig). 
# So it makes sense to first filter out based on the smallest difference in days, 
# then to check if there are still dyads that have the same number. 
check1 <- repeated2 %>%
  group_by(truncated_dyadcode) %>%
  mutate(diff_value = time_diff_days - lag(time_diff_days)) %>%
  mutate(diff_value = ifelse(is.na(diff_value), 1, diff_value)) %>%
  ungroup() %>%
  mutate(keep_row = if_else(diff_value == 0, FALSE, keep_row)) %>%
  arrange(diff_value)

check2 <- check1 %>%
  group_by(after_dot, truncated_before_dot) %>%
  mutate(diff_value = time_diff_days - lag(time_diff_days)) %>%
  mutate(diff_value = ifelse(is.na(diff_value), 1, diff_value)) %>%
  ungroup() %>%
  mutate(keep_row = if_else(diff_value == 0, FALSE, keep_row)) %>%
  arrange(diff_value)

final_litters <- check2 %>%
  filter (keep_row = TRUE) %>%
  select(-repeats, -truncated_dyadcode, -truncated_before_dot, -before_dot, -after_dot,
         -keep_row, -diff_value) #%>%
# Only include individuals that we have CSI data from
  #filter(motherdyad %in% csi_data$dyad)

# Filter CSI data to only keep dyads included in the litter dataset
csi_data <- csi_data %>% filter(dyad %in% final_litters$motherdyad)

# Create list to check if dyad CSI makes sense
check5 <- csi_data %>%
  select(dyad, CSIall) %>%
  mutate(CSIall = as.numeric(CSIall)) %>%           
  arrange(desc(CSIall)) 


# 4. COMBINE litter data with CSI data ----------------------------------------
# Make name for dyadcolumn the same
csi_data <- csi_data %>%
  mutate(motherdyad = dyad) %>%
  select(-dyad)

# Merge
CSI_litter_dyads <- merge(csi_data, litter_combinations, by = "motherdyad")

# Export data frame
write_xlsx(CSI_litter_dyads, "CSI_litter_dyads.xlsx") 




# 5. Compare included litters AB with dataframe after cleaning -----------------------------
# This step is just to check

excel_file_path <- here("data", "included_litters_synchrony_AB_20241022.xlsx")
included <- read_excel(excel_file_path, sheet = "Sheet1")

# Make sure names format matches
included <- included %>%
  mutate(mother1 = str_pad(substr(tolower(mother1), 
                                  1, 4), width = 4, side = "right", pad = "_"))
 
combined_id <- c(final_litters$mother1, final_litters$mother2)
all_in_combined <- all(combined_id %in% included$mother1) # TRUE: all are in included$mother1

unique_to_combined <- setdiff(combined_id, included$mother1)
unique_to_included <- setdiff(included$mother1, combined_id)

combined_list <- unique(combined_id)
included_list <- unique(included$mother1)
view(combined_list)

