# 23.12.2024
# Melissa Vanderheyden

# Clean focal data Nina for affiliation

library (tidyverse)
library(readxl)
library(lubridate)
library(here)

From <- ymd("2023-03-19")
To <- ymd("2024-03-20")

excel_file_path <- here("data", "continuous_20241205.xlsx")
continuous <- read_excel(excel_file_path, sheet = "Sheet1") %>%
  mutate(Date = ymd(Date)) %>% # Handle date formats
  # Sort chronologically.
  # time_num is protocol start, time2_num time in focal. Both converted to number (do in Excel)
  arrange(Date, time_num, time2_num) %>%
  # Create dyadcode (in alphabetical order)
  mutate(dyad = paste(pmin(Actor, Receiver), pmax(Actor,Receiver), sep=".")) %>%
  # Create columns with short id codes for filtering
  mutate(Actor_short = str_pad(str_sub(tolower(Actor), 1, 4), 4,
                               pad = "_", side = "right")) %>%
  mutate(Receiver_short = str_pad(str_sub(tolower(Receiver), 1, 4), 4,
                                  pad = "_", side = "right")) %>%
  # Unique identified per day and protocol
  mutate(identifier = paste(Date, Prot_nr, sep = "_")) %>%
  # Only include rows where Actor or Receiver are the focal individual
  filter(Actor == Focal_id | Receiver == Focal_id) %>%
  filter(Date > From & Date < To) #%>% # Set time window
  #filter(Obs != "nt")



file_path <- here("data", "adult_id.xlsx")
id_groups <- read_excel(file_path, sheet = "all_20232024") %>%
  mutate(id = str_pad(str_sub(tolower(id_long), 1, 4), 4,
                      pad = "_", side = "right")) %>%
  mutate(id = ifelse(id == "date", "datt", id)) %>%
  select(-sex, -id_long)


id_all <- read_excel(file_path, sheet = "epicollect") %>%
  mutate(id = str_pad(str_sub(tolower(ID), 1, 4), 4,
                      pad = "_", side = "right")) %>%
  mutate(id = ifelse(id == "date", "datt", id)) %>%
  filter(!is.na(id)) %>% # Ótherwise the list also contains dens etc
  mutate(Sex = case_when(
    Sex == "Male" ~ "m",
    Sex == "Female" ~ "f",
    TRUE ~ Sex  # Keep other values unchanged, if any
  )) %>%
  full_join(id_groups, by = "id")

file_path <- here("output", "focal_time.xlsx")
focal_time <- read_xlsx(file_path, "Sheet 1")



# 5. SP EP UNDIRECTED ---------------------------------------------------------
# _5.1 Couple sp-ep duos -------------------------------------------------------

# Filter out sp ep 
df4 <- continuous %>% 
  filter(Action %in% c("sp", "ep")) %>% # Only sp and ep
  # Create action_sort so Action is sorted with sp before ep
  mutate(action_sort = factor(Action, 
                              levels = c("sp", "ep", setdiff(unique(Action), c("sp", "ep"))))) %>%
  arrange(Date, Prot_nr, dyad, action_sort, time2_num)


# Create a unique pairing number
df4$pairing_num <- NA
pairing_counter <- 1

# Iterate over the dataframe to assign pairing numbers
for (i in seq_len(nrow(df4))) {
  if (df4$Action[i] == "sp" && is.na(df4$pairing_num[i])) {
    # Find the next 'ep' for the same dyad and identifier
    ep_index <- which(df4$dyad == df4$dyad[i] & df4$Action == "ep" 
                      & is.na(df4$pairing_num) & df4$identifier == df4$identifier[i])
    if (length(ep_index) > 0) {
      df4$pairing_num[i] <- pairing_counter
      df4$pairing_num[ep_index[1]] <- pairing_counter
      pairing_counter <- pairing_counter + 1
    }
  }
}

df4$pairing_num[is.na(df4$pairing_num)] <- "unmatched" # Identify unmatched approaches or leaves





df4$pairing_num[is.na(df4$pairing_num)] <- "unmatched" # Identify unmatched approaches or leaves
check <- df4 %>%
  filter(pairing_num == "unmatched") %>%
  select(Prot_nr, Date, Obs, time2, Actor, Action, Receiver)
