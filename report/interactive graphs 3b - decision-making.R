library(tidyverse)
library(asylum)

# ---- What is the asylum grant rate at initial decision vs appeal? ----
grant_rates_at_appeal <- 
  asylum::appeals_determined |> 
  group_by(Year, Nationality, Region, Outcome) |> 
  summarise(`Appeals determined` = sum(`Appeals determined`, na.rm = TRUE)) |> 
  ungroup() |> 
  
  pivot_wider(names_from = Outcome, values_from = `Appeals determined`) |> 
  
  mutate(
    Allowed = replace_na(Allowed, 0),
    Dismissed = replace_na(Dismissed, 0),
    Withdrawn = replace_na(Withdrawn, 0)
  ) |> 
  
  # Calculate grant rate - don't include withdrawn cases, since they're not
  # included in calculations of initial grant rates
  mutate(`Grant rate at appeal` = Allowed / (Allowed + Dismissed))

# Overall grant rates at appeal
grant_rates_at_appeal_overall <- 
  grant_rates_at_appeal |> 
  group_by(Year) |> 
  summarise(
    Allowed = sum(Allowed, na.rm = TRUE),
    Dismissed = sum(Dismissed, na.rm = TRUE)
  ) |> 
  mutate(`Grant rate at appeal` = Allowed / (Allowed + Dismissed)) |> 
  select(Year, `Grant rate at appeal`)

grant_rates_at_appeal_overall |> 
  write_csv("data-raw/flourish/3b - Decision-making/grant rates at appeal.csv")

# - Combine initial and appeal grant rates into a single dataframe/file
# Initial grant rates
grant_rates_initial_annual |>
  group_by(Year) |>
  summarise(
    Grant = sum(Grant, na.rm = TRUE),
    Refused = sum(Refused, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(`Initial grant rate` = Grant / (Grant + Refused)) |>
  select(Year, `Initial grant rate`) |> 
  
  # Merge grant rates at appeal
  left_join(grant_rates_at_appeal_overall) |> 
  drop_na() |> 
  write_csv("data-raw/flourish/3b - Decision-making/initial and appeal grant rates.csv")

# - Grant rates by nationality -
# Top five nations, by number of grants and grant rate in the most recent year
top_five_nations <- 
  grant_rates_at_appeal |> 
  filter(Year == max(Year)) |> 
  arrange(desc(`Grant rate at appeal`), desc(Allowed)) |> 
  slice(1:5) |> 
  pull(Nationality)

# Make a wider version of initial grant rates quarterly data for testing in a Flourish Studio chart
grant_rates_at_appeal |> 
  select(Year, Nationality, `Grant rate at appeal`) |> 
  pivot_wider(names_from = Nationality, values_from = `Grant rate at appeal`) |> 
  
  # Move the five nations with the highest number of grants and highest grant rates to the left, so they get shown on the chart by default
  relocate(Year, any_of(top_five_nations)) |> 
  
  write_csv("data-raw/flourish/3b - Decision-making/grant rates at appeal - by nationality.csv")

# ---- How many people have been age disputed in the last 12 months by nationality and gender and what was the outcome of the dispute? ----
# - Data doesn't contain gender/sex ----
age_disputes_nation <- 
  asylum::age_disputes |> 
  # Filter within the last 12 months
  filter(Date >= today() - dmonths(12)) |> 
  group_by(Nationality, `Raised or resolved`, `Raised type / Resolved outcome`) |> 
  summarise(`Age disputes` = sum(`Age disputes`, na.rm = TRUE)) |> 
  ungroup()

# Get top five nations for 'raised' and for 'resolved'
top_five_raised <- 
  age_disputes_nation |> 
  filter(`Raised or resolved` == "Raised") |> 
  arrange(desc(`Age disputes`)) |> 
  slice(1:10) |> 
  pull(Nationality)
  
top_five_resolved <- 
  age_disputes_nation |> 
  filter(`Raised or resolved` == "Resolved") |> 
  arrange(desc(`Age disputes`)) |> 
  slice(1:10) |> 
  pull(Nationality)

age_disputes_nation |> 
  filter(Nationality %in% c(top_five_raised, top_five_resolved)) |> 
  #   (`Raised or resolved` == "Raised" & Nationality %in% top_five_raised) |
  #     (`Raised or resolved` == "Resolved" & Nationality %in% top_five_resolved)
  # ) |> 
  pivot_wider(names_from = `Raised type / Resolved outcome`, values_from = `Age disputes`) |> 
  
  write_csv("data-raw/flourish/3b - Decision-making/age disputes.csv")
