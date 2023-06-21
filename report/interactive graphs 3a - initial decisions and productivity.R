library(tidyverse)
library(asylum)

# ---- What is the number of people waiting for an initial decision on their asylum claim (and what is their nationality, age, gender)? ----
# - Total -
asylum::awaiting_decision |> 
  filter(Date == max(Date)) |> 
  summarise(sum(Applications))

# - Nationality -
asylum::awaiting_decision |> 
  filter(Date == max(Date)) |>
  group_by(Nationality, Duration) |> 
  summarise(Applications = sum(Applications)) |> 
  arrange(desc(Applications)) |> 
  pivot_wider(names_from = Duration, values_from = Applications) |> 
  mutate(
    `6 months or less` = replace_na(`6 months or less`, 0),
    `More than 6 months` = replace_na(`More than 6 months`, 0),
  ) |> 
  write_csv("data-raw/flourish/3a - Initial decisions and productivity/waiting - by nationality.csv")

# - Data by age and sex doesn't exist -

# ---- What is the impact of the streamlined asylum process (SAP) and what has the impact of this policy been on the backlog? ----
# Not sure we can answer this from Home Office data...

# ---- What is the productivity of the Home Office asylum decision making (using Home Office quarterly transparency data)? ----
asylum::asylum_costs_and_productivity |> 
  select(`Financial Year`, Productivity) |> 
  drop_na() |> 
  write_csv("data-raw/flourish/3a - Initial decisions and productivity/Home Office productivity.csv")

# ---- What is the current backlog for decisions on family reunion cases? ----
# Not sure this data exists...
# ... thought possibly in VSI_03 worksheet in https://www.gov.uk/government/publications/visas-and-citizenship-data-q1-2023

# ---- How long are families currently waiting to be reunited? ----
# Not sure this data exists
