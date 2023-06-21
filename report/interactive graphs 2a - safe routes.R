library(tidyverse)
library(asylum)

# ---- How many people have been granted protection in the UK having arrived through a safe route? ----
asylum::decisions_resettlement |> 
  filter(`Case type` == "Resettlement Case" & `Case outcome group` == "Grant of Protection") |> 
  group_by(Date) |> 
  summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
  write_csv("data-raw/flourish/2a - Safe routes/2a - resettlement - total.csv")

# ---- What safe routes have been available in the last 12 months and how many people (and age, nationality and gender) have arrived through each? ----
bind_rows(
  asylum::decisions_resettlement |> 
    filter(`Case type` == "Resettlement Case") |> 
    # Filter applications within the last 12 months
    filter(Date >= today() - dmonths(12)) |> 
    group_by(Nationality) |> 
    summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
    arrange(desc(Decisions)) |> 
    slice(1:10) |> 
    rename(Category = Nationality, Nationality = Decisions) |> 
    mutate(Type = "Nationality"),
  
  asylum::decisions_resettlement |> 
    filter(`Case type` == "Resettlement Case") |> 
    # Filter applications within the last 12 months
    filter(Date >= today() - dmonths(12)) |> 
    group_by(Age) |> 
    summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
    filter(Age != "Unknown") |> 
    rename(Category = Age, Age = Decisions) |> 
    mutate(Type = "Age"),
  
  asylum::decisions_resettlement |> 
    filter(`Case type` == "Resettlement Case") |> 
    # Filter applications within the last 12 months
    filter(Date >= today() - dmonths(12)) |> 
    group_by(Sex) |> 
    summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
    filter(Sex != "Unknown Sex") |> 
    rename(Category = Sex, Sex = Decisions) |> 
    mutate(Type = "Sex")
) |> 
  write_csv("data-raw/flourish/2a - Safe routes/2a - What safe routes have been available in the last 12 months - by category.csv")

# ---- How many people have arrived from Ukraine through a safe route in the last 12 months? ----
source("https://github.com/britishredcrosssociety/ukraine-analyses/raw/main/R/load%20Ukraine%20visa%20data%20-%20scraped.R")

visas_scraped |> 
  ungroup() |> 
  filter(str_detect(Stage, "arrival")) |> 
  select(Date, Scheme, Arrivals = Visas_imputed) |> 
  pivot_wider(names_from = Scheme, values_from = Arrivals) |> 
  write_csv("data-raw/flourish/2a - Safe routes/2a - Arrivals from Ukraine.csv")

# ---- How many people have crossed the channel in a small boat and other ‘irregular entry’ by nationality, age and gender and what are the trends e.g. month by month/by quarter? ----
# Top five nations, by number of returns in the most recent year
top_five_nations <- 
  asylum::irregular_migration |> 
  filter(Year == max(Year)) |> 
  group_by(Year, Nationality) |> 
  summarise(`Number of detections` = sum(`Number of detections`, na.rm = TRUE)) |> 
  arrange(desc(`Number of detections`)) |> 
  slice(1:5) |> 
  pull(Nationality)

# By nationality
asylum::irregular_migration |> 
  select(Date, Nationality, `Number of detections`) |> 
  
  group_by(Date, Nationality) |> 
  summarise(`Number of detections` = sum(`Number of detections`, na.rm = TRUE)) |> 
  
  pivot_wider(names_from = Nationality, values_from = `Number of detections`) |> 
  
  # Move the top five nations to the left, so they get shown on the chart by default
  relocate(Date, any_of(top_five_nations)) |> 
  
  write_csv("data-raw/flourish/2a - Safe routes/2a - Irregular migration - by nationality.csv")

# By age/sex
asylum::irregular_migration |> 
  group_by(Date, `Age Group`, Sex) |> 
  summarise(`Number of detections` = sum(`Number of detections`, na.rm = TRUE)) |> 
  write_csv("data-raw/flourish/2a - Safe routes/2a - Irregular migration - by age and sex.csv")

# ---- How many people have arrived in the UK in the last 12 months through family reunion pathways? ----
asylum::family_reunion |> 
  # Filter visas within the last 12 months
  filter(Date >= today() - dmonths(12)) |> 
  summarise(`Visas granted` = sum(`Visas granted`, na.rm = TRUE))

asylum::family_reunion |> 
  group_by(Date) |> 
  summarise(`Visas granted` = sum(`Visas granted`, na.rm = TRUE)) |> 
  write_csv("data-raw/flourish/2a - Safe routes/2a - Family reunion.csv")
