# 02.12.2024
# Melissa Vanderheyden

# Load packages
library (tidyverse)
library(readxl)
library(lubridate)
library(here)
library(openxlsx)



# 1. IMPORT DATA ---------------------------------------------------------------
excel_file_path <- here("data", "CSI_FRD_NT_BC_MV_20241202.xlsx")
csi_data <- read_excel(excel_file_path, sheet = "Sheet1")

# Only include known females
csi_samegroup <- csi_data %>%
  filter(CSIgroup != "x") %>%
  filter(CSIgroup != "xxx") %>%
  #filter(dyadsex == "ff") %>%
  # Only include dyads once (undirected)
  filter(Doublecounter == 1) %>%
  filter(SameGroup == "y") %>%
  mutate(CSIgroup = as.numeric(CSIgroup)) %>%
  mutate(CSIall = as.numeric(CSIall))

groompack <- csi_samegroup %>%
  filter(ActGroup == "groom") %>%
  select(dyad, CSIall,CSIgroup)

test <- csi_data %>%
  filter(CSIgroup != "x") %>%
  filter(CSIgroup != "xxx") %>%
  #filter(ActGroup == "groom") %>%
  filter(Doublecounter == 1) %>%
  select(dyad, CSIall,CSIgroup)


write.xlsx(groompack, "groompack_ff_csi.xlsx")
