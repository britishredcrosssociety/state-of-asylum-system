library(tidyverse)
library(asylum)

# ---- How many people seeking asylum are in receipt of asylum support, and which kind of support? ----
# Trend
asylum::support_received |> 
  group_by(Date, `Support Type`) |> 
  summarise(People = sum(People)) |> 
  pivot_wider(names_from = `Support Type`, values_from = People) |> 
  write_csv("data-raw/flourish/4a - Asylum support/asylum support - longitudinal.csv")

# Most recent stats
asylum::support_received |> 
  filter(Date == max(Date)) |> 
  group_by(`Support Type`, `Accommodation Type`) |> 
  summarise(People = sum(People)) |> 
  write_csv("data-raw/flourish/4a - Asylum support/asylum support - most recent.csv")

# ---- What is the rate of destitution among people seeking asylum in the UK? ----
# - Could this be number receiving support divided by total number of people on the backlog? -
# Total receiving destitution support
receiving_support <- 
  asylum::support_received |> 
  filter(Date == max(Date)) |> 
  mutate(`Accommodation Type` = if_else(`Accommodation Type` == "Subsistence Only", "Receiving asylum support (subsistence only)", "Receiving asylum support (accommodation)")) |> 
  group_by(`Accommodation Type`) |> 
  summarise(People = sum(People))

# Total waiting for a decision
backlog <- 
  asylum::awaiting_decision |> 
  filter(Date == max(Date)) |> 
  summarise(Backlog = sum(Applications)) |> 
  pull(Backlog)

receiving_support |> 
  rename(Category = `Accommodation Type`) |> 
  add_row(
    Category = "Awaiting decision but not receiving asylum support", 
    People = backlog - sum(receiving_support$People)
  ) |> 
  write_csv("data-raw/flourish/4a - Asylum support/destitution rate.csv")

# receiving_support
# backlog
# backlog - receiving_support
scales::percent(sum(receiving_support$People) / backlog)

# ---- How many people deemed inadmissible are in receipt of support? ----
# Not sure data exists

# ---- For those who have been granted refugee status, what is the rate of destitution (move on period)? ----
# Not sure data exists, but could show destitution among people BRC supports

# ---- How many people seeking asylum have been granted the right to work? (related – how many vacancies are there in the UK job market?) ----
# Not sure data exists
