library(tidyverse)
library(asylum)

# ---- What is the asylum grant rate at initial decision vs appeal? ----
# Calculate initial and final grant rates from Outcomes data
grant_rates_initial_final <- 
asylum::outcomes |>
  drop_na() |> 
  select(`Year of application`, `Granted asylum`:Refused, `Allowed appeals`:`Dismissed appeals`, `Final outcome: Grants of asylum`:`Final outcomes: Refused asylum or HP or DL or other leave`) |> 
  group_by(`Year of application`) |> 
  summarise(across(everything(), sum)) |> 
  ungroup() |> 
  
  mutate(
    `Initial grant rate` = (`Granted asylum` + `Granted HP/DL` + `Other grants`) / (`Granted asylum` + `Granted HP/DL` + `Other grants` + Refused),
    # `Appeal grant rate` = `Allowed appeals` / (`Allowed appeals` + `Dismissed appeals`),
    `Final grant rate` = (`Final outcome: Grants of asylum` + `Final outcomes: Grants of HP/DL and other`) / (`Final outcome: Grants of asylum` + `Final outcomes: Grants of HP/DL and other` + `Final outcomes: Refused asylum or HP or DL or other leave`)
  )
  
grant_rates_initial_final |> 
  select(`Year of application`, contains("rate")) |> 
  write_csv("data-raw/flourish/3b - Decision-making/grant rates - initial and final.csv")

# ---- How many people have been age disputed in the last 12 months by nationality and gender and what was the outcome of the dispute? ----
# Data doesn't contain gender/sex
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
  
  write_csv("data-raw/flourish/3b - Decision-making/age disputes.csv")

# - Caption -
# What proportion of all resolved age disputes found that people were children?
age_disputes_nation |> 
  ungroup() |> 
  group_by(`Resolved outcome`) |> 
  summarise(`Age disputes` = sum(`Age disputes`)) |> 
  ungroup() |> 
  mutate(Proportion = `Age disputes` / sum(`Age disputes`))
