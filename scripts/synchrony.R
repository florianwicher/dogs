library(readxl)
library(dplyr)

df <- read_excel("output/litterdyads_cutoffmax_focalpackmates_norepeats_20250201_1.xlsx")

for (i in 1:nrow(df_selected)) {
  synchrony(
    df$motherdyad[i],
    ymd(df$From[i]),
    ymd(df$To[i])
  )
}
