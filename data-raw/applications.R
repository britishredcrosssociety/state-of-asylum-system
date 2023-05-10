library(tidyverse)
library(asylum)

applications_summary <- 
  asylum::applications |> 
  group_by(Date, Region, Nationality) |> 
  summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
  ungroup()

# usethis::use_data(DATASET, overwrite = TRUE)
write_csv(applications_summary, "data-raw/applications-summary.csv")
