library(tidyverse)
library(asylum)
library(zoo)

source("functions/rolling-annual-sum.R")

# Prepare asylum applications data by calculating quarterly total numbers of applications
applications_annual <- 
  asylum::applications |> 
  group_by(Date) |> 
  summarise(Applications = sum(Applications, na.rm = TRUE))

# Use the `rolling_annual_sum()` function to calculate the total number of applications
# for the year ending June 2023, June 2022, June 2021 etc.
applications_year_ending_most_recent_quarter <- 
  applications_annual |> 
  rolling_annual_sum(Applications)

View(applications_year_ending_most_recent_quarter)
