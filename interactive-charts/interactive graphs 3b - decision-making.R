library(tidyverse)
library(asylum)

# ---- Resolved age disputes over the last 12 months ----
age_disputes_nation <- 
  asylum::age_disputes |> 
  # Filter within the last 12 months
  filter(Date >= max(Date) - dmonths(11)) |> 
  filter(`Raised or resolved` == "Resolved") |>   # Don't need to show 'raised'
  group_by(Nationality, `Resolved outcome` = `Raised type / Resolved outcome`) |> 
  summarise(`Age disputes` = sum(`Age disputes`, na.rm = TRUE)) |> 
  ungroup() |> 
  
  mutate(`Resolved outcome` = if_else(`Resolved outcome` == "Less than 18", "Under 18", "18+"))

# Get top ten nationalities by number of disputes resolved
top_ten_resolved <- 
  age_disputes_nation |> 
  
  group_by(Nationality) |> 
  summarise(`Age disputes` = sum(`Age disputes`, na.rm = TRUE)) |> 
  ungroup() |> 
  
  slice_max(`Age disputes`, n = 10) |> 
  pull(Nationality)

age_disputes_nation |> 
  filter(Nationality %in% top_ten_resolved) |> 
  pivot_wider(names_from = `Resolved outcome`, values_from = `Age disputes`) |> 
  relocate(`18+`, .after = last_col()) |> 
  
  write_csv("data-raw/flourish/3b - Decision-making/age disputes Sep 23.csv")

# - CAPTION -
# What proportion of all resolved age disputes found that people were children?
age_disputes_nation |> 
  ungroup() |> 
  group_by(`Resolved outcome`) |> 
  summarise(`Age disputes` = sum(`Age disputes`)) |> 
  ungroup() |> 
  mutate(Proportion = `Age disputes` / sum(`Age disputes`))
