# 2025-01-31
# Melissa Vanderheyden


#####  Calculating CSI values for included females synchrony     #####


library(tidyverse)
library(readxl)
library(lubridate)
library(here)
library(writexl)

synchrony <- function(dyad, From, To) {

  #TODO use dyad

  # _1.1 IMPORT BIRTH DATA ---------------------------------------------------------------
  path <- here("data", "litterdyads_cutoffmax_allmothers_norepeats_20250131.xlsx")
  birth_data <- read_xlsx(path, sheet = "Sheet1")

  motherdyad <- birth_data %>%
    filter(pack1 == pack2) %>% # !!! Distinction nova - market to be made later
    pull(motherdyad)

  # Create file to fill
  focal_mothers_withinpack <- birth_data %>%
    filter(pack1 == pack2) %>% # Still need to split market and nova, but from which date?
    mutate(first_litter = pmin(date1, date2),
           From = first_litter - days(63) - months(3),
           To = first_litter - days(63),
           in_focal = ifelse(To > as.Date("2023-06-17"), "yes", "no"))

  #path <- here("output", "litterdyads_cutoffmax_focalpackmates_norepeats_[date].xlsx")
  #write_xlsx(focal_mothers_withinpack, path)


  # _1.2 CSI adjusted --------------------------------------------------------------
  # Only include same-group dyads

  # Import data

  # Included individuals
  file_path <- here("data", "adult_id.xlsx")
  id_all <- read_excel(file_path, sheet = "all_20232024") %>%
    mutate(id = str_pad(str_sub(tolower(id_long), 1, 4), 4,
                        pad = "_", side = "right")) %>%
    #mutate(id = ifelse(id == "date", "datt", id)) %>%
    filter(sex == "f")


  excel_file_path <- here("data", "Continuous_20241224.xlsx")
  continuous <- read_excel(excel_file_path, sheet = "ALL") %>%
    mutate(Date = ymd(Date)) %>% # Handle date formats
    # Sort chronologically.
    # time_num is protocol start, Time_sec time in focal. Both converted to number (do in Excel)
    arrange(Date, Time_num, Time_sec) %>%
    # Create dyadcode (in alphabetical order)
    mutate(dyad = paste(pmin(Actor, Receiver), pmax(Actor, Receiver), sep = ".")) %>%
    # Create columns with short id codes for filtering
    mutate(Actor_short = str_pad(str_sub(tolower(Actor), 1, 4), 4,
                                 pad = "_", side = "right")) %>%
    mutate(Receiver_short = str_pad(str_sub(tolower(Receiver), 1, 4), 4,
                                    pad = "_", side = "right")) %>%
    # Unique identified per day and protocol
    mutate(identifier = paste(Date, Prot_nr, sep = "_")) %>%
    filter(Date > From & Date < To)


  file_path <- here("output", "focal_time.xlsx")
  focal_time <- read_xlsx(file_path, "Sheet 1")


  # 2. AP LE UNDIRECTED -----------------------------------------------------------

  # Filter out everything that is not an affiliative interaction
  ###!!!!!!!!!!!!!!!! STILL CHECK WITH ETHOGRAM IF ALL AFFIL BEHAV IN THERE
  df <- continuous %>%
    filter(Action %in% c("ap", "le"))

  # _2.1 Couple ap-le duos --------------------------------------------------------

  ###### Give unique numbers to each ap-le duo

  # Order by date, protocol, dyad, and action (to ensure 'ap' comes before 'le')
  df <- df[order(df$Date, df$Prot_nr, df$dyad, df$Action, df$Time_sec),]

  # Create a unique pairing number
  df$pairing_num <- NA
  pairing_counter <- 1

  # Iterate over the dataframe to assign pairing numbers
  for (i in seq_len(nrow(df))) {
    if (df$Action[i] == "ap" && is.na(df$pairing_num[i])) {
      # Find the next 'le' for the same dyad and identifier
      le_index <- which(df$dyad == df$dyad[i] &
                          df$Action == "le"
                          &
                          is.na(df$pairing_num) &
                          df$identifier == df$identifier[i])
      if (length(le_index) > 0) {
        df$pairing_num[i] <- pairing_counter
        df$pairing_num[le_index[1]] <- pairing_counter
        pairing_counter <- pairing_counter + 1
      }
    }
  }


  df$pairing_num[is.na(df$pairing_num)] <- "unmatched" # Identify unmatched approaches or leaves

  # _2.2 Calculate time spent together -------------------------------------------

  # Create extra column that shows the time of the protocol in minutes
  # (but working with comma! So does not show seconds)
  df_prox1 <- df %>%
    arrange(Date, Prot_nr, Time_sec) %>% # sort again
    mutate(protocol_minutes = Time_sec / 60)


  #### Use pairing_num to calculate the time spent together for each dyad


  # Create a new column showing the time difference between ap and le for
  # rows with a corresponding pairing_num
  df_prox1$time_diff <- NA

  # Calculate the difference for each pairing_num directly
  for (pair_num in unique(df_prox1$pairing_num[df_prox1$pairing_num != "unmatched"])) {
    # Subset df_prox1 to contain only rows for the current pairing number
    pair_rows <- df_prox1[df_prox1$pairing_num == pair_num,]
    # Calculate the difference
    time_diff <- pair_rows$protocol_minutes[pair_rows$Action == "le"] -
      pair_rows$protocol_minutes[pair_rows$Action == "ap"]
    # Store the difference in the 'time_diff' column for all rows with current pairing number
    df_prox1$time_diff[df_prox1$pairing_num == pair_num] <- time_diff
  }

  # Decide on cut-off value for time spent together. If below this value, ap-le
  # won't be included as it could just mean passing by.

  cut_off <- 0.0333 # 2 sec = 0.0333 min

  proximity_summary <- df_prox1 %>%
    distinct(dyad, pairing_num, .keep_all = TRUE) %>%  # Keep only unique dyad-pairing_num combinations
    filter(time_diff > cut_off) %>%
    group_by(dyad) %>%
    summarize(
      ap_le_time = sum(time_diff, na.rm = TRUE), # proximity sum
      ap_le_count = as.numeric(n()) # number of times in proximity (considering the cutoff)
    )

  # 3. SB EB UNDIRECTED ---------------------------------------------------------
  # _3.1 Couple sb-eb duos -------------------------------------------------------

  # Filter out SB EB
  df2 <- continuous %>%
    filter(Action %in% c("sb", "eb")) %>% # Only sb and eb
    # Create action_sort so Action is sorted with sb before eb
    mutate(action_sort = factor(Action,
                                levels = c("sb", "eb", setdiff(unique(Action), c("sb", "eb"))))) %>%
    arrange(Date, Prot_nr, dyad, action_sort, Time_sec)


  # Create a unique pairing number
  df2$pairing_num <- NA
  pairing_counter <- 1

  # Iterate over the dataframe to assign pairing numbers
  for (i in seq_len(nrow(df2))) {
    if (df2$Action[i] == "sb" && is.na(df2$pairing_num[i])) {
      # Find the next 'eb' for the same dyad and identifier
      eb_index <- which(df2$dyad == df2$dyad[i] &
                          df2$Action == "eb"
                          &
                          is.na(df2$pairing_num) &
                          df2$identifier == df2$identifier[i])
      if (length(eb_index) > 0) {
        df2$pairing_num[i] <- pairing_counter
        df2$pairing_num[eb_index[1]] <- pairing_counter
        pairing_counter <- pairing_counter + 1
      }
    }
  }

  df2$pairing_num[is.na(df2$pairing_num)] <- "unmatched" # Identify unmatched approaches or leaves


  # _3.2 Calculate time in bodycontact -------------------------------------------

  # Create extra column that shows the time of the protocol in minutes
  # (but working with comma! So does not show seconds)
  df2_contact <- df2 %>%
    arrange(Date, Prot_nr, Time_sec) %>% # sort again
    mutate(protocol_minutes = Time_sec / 60)


  #### Use pairing_num to calculate the time in body contact for each dyad

  # Create a new column showing the time difference between sb and eb for
  # rows with a corresponding pairing_num
  df2_contact$time_diff <- NA

  # Calculate the difference for each pairing_num directly
  for (pair_num in unique(df2_contact$pairing_num[df2_contact$pairing_num != "unmatched"])) {
    # Subset df_prox1 to contain only rows for the current pairing number
    pair_rows <- df2_contact[df2_contact$pairing_num == pair_num,]
    # Calculate the difference
    time_diff <- pair_rows$protocol_minutes[pair_rows$Action == "eb"] -
      pair_rows$protocol_minutes[pair_rows$Action == "sb"]
    # Store the difference in the 'time_diff' column for all rows with current pairing number
    df2_contact$time_diff[df2_contact$pairing_num == pair_num] <- time_diff
  }

  # Decide on cut-off value for time spent together. If below this value, ap-le
  # won't be included as it could just mean passing by.

  cut_off <- 0.0333 # 2 sec = 0.0333 min

  contact_summary <- df2_contact %>%
    distinct(dyad, pairing_num, .keep_all = TRUE) %>%  # Keep only unique dyad-pairing_num combinations
    filter(time_diff > cut_off) %>%
    group_by(dyad) %>%
    summarize(
      sb_eb_time = sum(time_diff, na.rm = TRUE), # proximity sum
      sb_eb_count = as.numeric(n()) # number of times in proximity (considering the cutoff)
    )


  # 4. SG EG UNDIRECTED ---------------------------------------------------------
  # _4.1 Couple sg-eg duos -------------------------------------------------------

  # Filter out sg eg
  df3 <- continuous %>%
    filter(Action %in% c("sg", "eg")) %>% # Only sg and eg
    # Create action_sort so Action is sorted with sg before eg
    mutate(action_sort = factor(Action,
                                levels = c("sg", "eg", setdiff(unique(Action), c("sg", "eg"))))) %>%
    arrange(Date, Prot_nr, dyad, action_sort, Time_sec)


  # Create a unique pairing number
  df3$pairing_num <- NA
  pairing_counter <- 1

  # Iterate over the dataframe to assign pairing numbers
  for (i in seq_len(nrow(df3))) {
    if (df3$Action[i] == "sg" && is.na(df3$pairing_num[i])) {
      # Find the next 'eg' for the same dyad and identifier
      eg_index <- which(df3$dyad == df3$dyad[i] &
                          df3$Action == "eg"
                          &
                          is.na(df3$pairing_num) &
                          df3$identifier == df3$identifier[i])
      if (length(eg_index) > 0) {
        df3$pairing_num[i] <- pairing_counter
        df3$pairing_num[eg_index[1]] <- pairing_counter
        pairing_counter <- pairing_counter + 1
      }
    }
  }

  df3$pairing_num[is.na(df3$pairing_num)] <- "unmatched" # Identify unmatched approaches or leaves


  # _4.2 Calculate time grooming ------------------------------------------------

  # Create extra column that shows the time of the protocol in minutes
  # (but working with comma! So does not show seconds)
  df3_grooming <- df3 %>%
    arrange(Date, Prot_nr, Time_sec) %>% # sort again
    mutate(protocol_minutes = Time_sec / 60)


  #### Use pairing_num to calculate the time in body grooming for each dyad

  # Create a new column showing the time difference between sg and eg for
  # rows with a corresponding pairing_num
  df3_grooming$time_diff <- NA

  # Calculate the difference for each pairing_num directly
  for (pair_num in unique(df3_grooming$pairing_num[df3_grooming$pairing_num != "unmatched"])) {
    # Subset df_prox1 to contain only rows for the current pairing number
    pair_rows <- df3_grooming[df3_grooming$pairing_num == pair_num,]
    # Calculate the difference
    time_diff <- pair_rows$protocol_minutes[pair_rows$Action == "eg"] -
      pair_rows$protocol_minutes[pair_rows$Action == "sg"]
    # Store the difference in the 'time_diff' column for all rows with current pairing number
    df3_grooming$time_diff[df3_grooming$pairing_num == pair_num] <- time_diff
  }

  # Decide on cut-off value for time spent together. If below this value, ap-le
  # won't be included as it could just mean passing by.

  cut_off <- 0.0333 # 2 sec = 0.0333 min

  grooming_summary <- df3_grooming %>%
    distinct(dyad, pairing_num, .keep_all = TRUE) %>%  # Keep only unique dyad-pairing_num combinations
    filter(time_diff > cut_off) %>%
    group_by(dyad) %>%
    summarize(
      sg_eg_time = sum(time_diff, na.rm = TRUE), # proximity sum
      sg_eg_count = as.numeric(n()) # number of times in proximity (considering the cutoff)
    )


  # 5. SP EP UNDIRECTED ---------------------------------------------------------
  # _5.1 Couple sp-ep duos -------------------------------------------------------

  # Filter out sp ep
  df4 <- continuous %>%
    filter(Action %in% c("sp", "ep")) %>% # Only sp and ep
    # Create action_sort so Action is sorted with sp before ep
    mutate(action_sort = factor(Action,
                                levels = c("sp", "ep", setdiff(unique(Action), c("sp", "ep"))))) %>%
    arrange(Date, Prot_nr, dyad, action_sort, Time_sec)


  # Create a unique pairing number
  df4$pairing_num <- NA
  pairing_counter <- 1

  # Iterate over the dataframe to assign pairing numbers
  for (i in seq_len(nrow(df4))) {
    if (df4$Action[i] == "sp" && is.na(df4$pairing_num[i])) {
      # Find the next 'ep' for the same dyad and identifier
      ep_index <- which(df4$dyad == df4$dyad[i] &
                          df4$Action == "ep"
                          &
                          is.na(df4$pairing_num) &
                          df4$identifier == df4$identifier[i])
      if (length(ep_index) > 0) {
        df4$pairing_num[i] <- pairing_counter
        df4$pairing_num[ep_index[1]] <- pairing_counter
        pairing_counter <- pairing_counter + 1
      }
    }
  }

  df4$pairing_num[is.na(df4$pairing_num)] <- "unmatched" # Identify unmatched approaches or leaves


  # _5.2 Calculate time playing ------------------------------------------------

  # Create extra column that shows the time of the protocol in minutes
  # (but working with comma! So does not show seconds)
  df4_play <- df4 %>%
    arrange(Date, Prot_nr, Time_sec) %>% # sort again
    mutate(protocol_minutes = Time_sec / 60)


  #### Use pairing_num to calculate the time in body play for each dyad

  # Create a new column showing the time difference between sp and ep for
  # rows with a corresponding pairing_num
  df4_play$time_diff <- NA

  # Calculate the difference for each pairing_num directly
  for (pair_num in unique(df4_play$pairing_num[df4_play$pairing_num != "unmatched"])) {
    # Subset df_prox1 to contain only rows for the current pairing number
    pair_rows <- df4_play[df4_play$pairing_num == pair_num,]
    # Calculate the difference
    time_diff <- pair_rows$protocol_minutes[pair_rows$Action == "ep"] -
      pair_rows$protocol_minutes[pair_rows$Action == "sp"]
    # Store the difference in the 'time_diff' column for all rows with current pairing number
    df4_play$time_diff[df4_play$pairing_num == pair_num] <- time_diff
  }

  # Decide on cut-off value for time spent together. If below this value, ap-le
  # won't be included as it could just mean passing by.

  cut_off <- 0.0333 # 2 sec = 0.0333 min

  play_summary <- df4_play %>%
    distinct(dyad, pairing_num, .keep_all = TRUE) %>%  # Keep only unique dyad-pairing_num combinations
    filter(time_diff > cut_off) %>%
    group_by(dyad) %>%
    summarize(
      sp_ep_time = sum(time_diff, na.rm = TRUE), # proximity sum
      sp_ep_count = as.numeric(n()) # number of times in proximity (considering the cutoff)
    )


  # 6. Friendly postures --------------------------------------------------------

  friendly_summary <- continuous %>%
    filter(Action %in% c('sf', 'lf', 'ro', 'af')) %>%
    group_by(dyad) %>%
    summarize(friendly_count = as.numeric(n()))

  # 7. Bundle in single data frame ----------------------------------------------

  # Let's get together and feel alright
  all_affiliation <- proximity_summary %>%
    full_join(contact_summary) %>%
    full_join(grooming_summary) %>%
    full_join(play_summary) %>%
    full_join(friendly_summary) %>%
    # make all but 'dyad' numeric for the next function to work
    mutate(across(-dyad, as.numeric)) %>%
    # Replace NAs with 0, all columns except 'dyad'
    mutate(across(-dyad, ~coalesce(., 0))) %>%
    mutate(originaldyad = dyad) %>%
    #split the dyad ids, add pack for each id
    separate(originaldyad, into = c("id1", "id2"), sep = "\\.") %>%
    left_join(id_all %>%
                mutate(id1 = id) %>%
                mutate(pack1 = pack) %>%
                select(id1, pack1), by = "id1") %>%
    left_join(id_all %>%
                mutate(id2 = id) %>%
                mutate(pack2 = pack) %>%
                select(id2, pack2), by = "id2") %>%

    # Adjust pack for market females, depending on time period
    mutate(pack1 = ifelse(id1 %in% c("jaws", "wasm", "lara", "tach"),
                          ifelse(To < "2023-06-15", "nova", "market"), pack1),
           pack2 = ifelse(id2 %in% c("jaws", "wasm", "lara", "tach"),
                          ifelse(To < "2023-06-15", "nova", "market"), pack2)) %>%

    # indicate if same pack
    mutate(samepack = ifelse(!is.na(pack1) &
                               !is.na(pack2)
                               &
                               pack1 == pack2, TRUE, FALSE)) %>%
    # remove individuals without a pack
    filter(!is.na(pack1) & (!is.na(pack2))) %>%
    # add total focal time
    left_join(focal_time %>%
                mutate(id1 = Focal) %>%
                mutate(obs_hours1 = total_obs_time) %>%
                select(id1, obs_hours1), by = 'id1') %>%
    left_join(focal_time %>%
                mutate(id2 = Focal) %>%
                mutate(obs_hours2 = total_obs_time) %>%
                select(id2, obs_hours2), by = 'id2') %>%
    mutate(across(c(obs_hours1, obs_hours2), ~coalesce(., 0)))


  # 7. Calculate CSI

  # _7.1 Correct for number of focal hours -----------------------------------------
  all_affiliation2 <- all_affiliation %>%
    mutate(obs_hours_SUM = rowSums(across(c(obs_hours1, obs_hours2)))) %>%
    # durations and counts devided by total focal hours for dyad
    mutate(ap_le_time_FH = ap_le_time / obs_hours_SUM) %>%
    mutate(ap_le_count_FH = ap_le_count / obs_hours_SUM) %>%
    mutate(sb_eb_time_FH = sb_eb_time / obs_hours_SUM) %>%
    mutate(sb_eb_count_FH = sb_eb_count / obs_hours_SUM) %>%
    mutate(sg_eg_time_FH = sg_eg_time / obs_hours_SUM) %>%
    mutate(sg_eg_count_FH = sg_eg_count / obs_hours_SUM) %>%
    mutate(sp_ep_time_FH = sp_ep_time / obs_hours_SUM) %>%
    mutate(sp_ep_count_FH = sp_ep_count / obs_hours_SUM) %>%
    mutate(friendly_count_FH = friendly_count / obs_hours_SUM)


  # _7.2 Prep for CSIall ---------------------------------------------------------
  all_affiliation3 <- all_affiliation2 %>%
    mutate(ap_le_time_CSI = ap_le_time_FH / mean(ap_le_time_FH, na.rm = TRUE)) %>%
    mutate(ap_le_count_CSI = ap_le_count_FH / mean(ap_le_count_FH, na.rm = TRUE)) %>%
    mutate(sb_eb_time_CSI = sb_eb_time_FH / mean(sb_eb_time_FH, na.rm = TRUE)) %>%
    mutate(sb_eb_count_CSI = sb_eb_count_FH / mean(sb_eb_count_FH, na.rm = TRUE)) %>%
    mutate(sg_eg_time_CSI = sg_eg_time_FH / mean(sg_eg_time_FH, na.rm = TRUE)) %>%
    mutate(sg_eg_count_CSI = sg_eg_count_FH / mean(sg_eg_count_FH, na.rm = TRUE)) %>%
    mutate(sp_ep_time_CSI = sp_ep_time_FH / mean(sp_ep_time_FH, na.rm = TRUE)) %>%
    mutate(sp_ep_count_CSI = sp_ep_count_FH / mean(sp_ep_count_FH, na.rm = TRUE)) %>%
    mutate(friendly_count_CSI = friendly_count_FH / mean(friendly_count_FH, na.rm = TRUE)) %>%
    # show populations averages
    mutate(ap_le_time_avg = mean(ap_le_time_FH, na.rm = TRUE)) %>%
    mutate(ap_le_count_avg = mean(ap_le_count_FH, na.rm = TRUE)) %>%
    mutate(sb_eb_time_avg = mean(sb_eb_time_FH, na.rm = TRUE)) %>%
    mutate(sb_eb_count_avg = mean(sb_eb_count_FH, na.rm = TRUE)) %>%
    mutate(sg_eg_time_avg = mean(sg_eg_time_FH, na.rm = TRUE)) %>%
    mutate(sg_eg_count_avg = mean(sg_eg_count_FH, na.rm = TRUE)) %>%
    mutate(sp_ep_time_avg = mean(sp_ep_time_FH, na.rm = TRUE)) %>%
    mutate(sp_ep_count_avg = mean(sp_ep_count_FH, na.rm = TRUE)) %>%
    mutate(friendly_count_avg = mean(friendly_count_FH, na.rm = TRUE))


  # _7.3 Calculate CSI_all --------------------------------------------------------
  col_name <- paste0("CSI_all_", To)
  col_name_noprox <- paste0("CSI_all_noprox_", To)
  col_name_prox <- paste0("CSI_all_prox_", To)


  CSI_all_df <- all_affiliation3 %>%
    # ALL behaviours
    mutate(!!col_name := rowSums(across(c(ap_le_time_CSI, ap_le_count_CSI,
                                          sb_eb_time_CSI, sb_eb_count_CSI,
                                          sg_eg_time_CSI, sg_eg_count_CSI,
                                          sp_ep_time_CSI, sp_ep_count_CSI,
                                          friendly_count_CSI))) / 9,
           !!col_name_noprox := rowSums(across(c(sb_eb_time_CSI, sb_eb_count_CSI,
                                                 sg_eg_time_CSI, sg_eg_count_CSI,
                                                 sp_ep_time_CSI, sp_ep_count_CSI,
                                                 friendly_count_CSI))) / 7,
           !!col_name_prox := rowSums(across(c(ap_le_time_CSI, ap_le_count_CSI))) / 2)


  # _7.4 Prep for CSI_group ------------------------------------------------------
  all_affiliation4 <- CSI_all_df %>%
    filter(samepack == TRUE) %>% # Only groupmates
    #filter(pack1 == "black&tan") %>%
    group_by(pack1) %>%
    mutate(ap_le_time_CSIgroup = ap_le_time_FH / mean(ap_le_time_FH, na.rm = TRUE)) %>% # Calculate mean within the group
    mutate(ap_le_count_CSIgroup = ap_le_count_FH / mean(ap_le_count_FH, na.rm = TRUE)) %>%
    mutate(sb_eb_time_CSIgroup = sb_eb_time_FH / mean(sb_eb_time_FH, na.rm = TRUE)) %>%
    mutate(sb_eb_count_CSIgroup = sb_eb_count_FH / mean(sb_eb_count_FH, na.rm = TRUE)) %>%
    mutate(sg_eg_time_CSIgroup = sg_eg_time_FH / mean(sg_eg_time_FH, na.rm = TRUE)) %>%
    mutate(sg_eg_count_CSIgroup = sg_eg_count_FH / mean(sg_eg_count_FH, na.rm = TRUE)) %>%
    mutate(sp_ep_time_CSIgroup = sp_ep_time_FH / mean(sp_ep_time_FH, na.rm = TRUE)) %>%
    mutate(sp_ep_count_CSIgroup = sp_ep_count_FH / mean(sp_ep_count_FH, na.rm = TRUE)) %>%
    mutate(friendly_count_CSIgroup = friendly_count_FH / mean(friendly_count_FH, na.rm = TRUE)) %>%
    # Show group averages
    mutate(ap_le_time_avggroup = mean(ap_le_time_FH, na.rm = TRUE)) %>% # Calculate mean within the group
    mutate(ap_le_count_avggroup = mean(ap_le_count_FH, na.rm = TRUE)) %>%
    mutate(sb_eb_time_avggroup = mean(sb_eb_time_FH, na.rm = TRUE)) %>%
    mutate(sb_eb_count_avggroup = mean(sb_eb_count_FH, na.rm = TRUE)) %>%
    mutate(sg_eg_time_avggroup = mean(sg_eg_time_FH, na.rm = TRUE)) %>%
    mutate(sg_eg_count_avggroup = mean(sg_eg_count_FH, na.rm = TRUE)) %>%
    mutate(sp_ep_time_avggroup = mean(sp_ep_time_FH, na.rm = TRUE)) %>%
    mutate(sp_ep_count_avggroup = mean(sp_ep_count_FH, na.rm = TRUE)) %>%
    mutate(friendly_count_avggroup = mean(friendly_count_FH, na.rm = TRUE)) %>%
    ungroup() %>%
    # Turn NaN values into 0 for groups where the average rate of a behaviour was zero
    # !!!!!!!!!!!!! Still check if ok to include like this !!!!!!!!
    mutate(across(everything(), ~ifelse(is.nan(.), 0, .)))

  # _7.5 Calculate CSI_group ----------------------------------------------------
  # Dynamic column name
  col_name <- paste0("CSI_group_", To)
  col_name_noprox <- paste0("CSI_group_noprox_", To)
  col_name_prox <- paste0("CSI_group_prox_", To)

  CSI_group_df <- all_affiliation4 %>%
    mutate(!!col_name := rowSums(across(c(ap_le_time_CSIgroup, ap_le_count_CSIgroup,
                                          sb_eb_time_CSIgroup, sb_eb_count_CSIgroup,
                                          sg_eg_time_CSIgroup, sg_eg_count_CSIgroup,
                                          sp_ep_time_CSIgroup, sp_ep_count_CSIgroup,
                                          friendly_count_CSIgroup))) / 9,
           !!col_name_noprox := rowSums(across(c(sb_eb_time_CSI, sb_eb_count_CSI,
                                                 sg_eg_time_CSI, sg_eg_count_CSI,
                                                 sp_ep_time_CSI, sp_ep_count_CSI,
                                                 friendly_count_CSI))) / 7,
           !!col_name_prox := rowSums(across(c(ap_le_time_CSIgroup, ap_le_count_CSIgroup))) / 2)


  # Buldle needed dataframes together in list
  # Name per date
  df_name <- paste0("CSI_", To)  # e.g., "df_2024_01_31"
  # Create an empty list to store DataFrames
  #CSI_dfs <- list()
  # Assign DataFrame dynamically into the list
  CSI_dfs[[df_name]] <- CSI_group_df

  check <- CSI_group_df %>%
    filter(dyad == "boug.nova")


  # Save the list of DataFrames to an RDS file
  saveRDS(CSI_dfs, file = "CSI_ff_synchrony1.rds")

  return("TODO return value here")
}

# Load it back when needed
#CSI_dfs <- readRDS("CSI_ff_synchrony.rds")


# Now we can make different dataframes for different dates and bundle all columns
# we want so we don't lose them..


# 8. NOTES ---------------------------------------------------------------------

# a) Some group meeasures were not calculating because the group average was 0.
# In this case, I've replaced all NA values with zeros before calculating the CSI.
