library(tidyverse)
library(asylum)
library(zoo)

resettlement_grants_without_ARAP <- 
  asylum::decisions_resettlement |> 
  filter(`Case type` == "Resettlement Case" & `Case outcome group` == "Grant of Protection") |> 
  filter(!str_detect(`Case outcome`, "Relocation - ARAP"))

# ---- How many people have been granted protection in the UK having arrived through a safe route? ----
resettlement_grants_without_ARAP |> 
  group_by(Date) |> 
  summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
  ungroup() |> 
  
  # Manually add missing rows - no resettlement during lockdowns
  add_row(Date = ymd("2020-07-01"), Decisions = 0) |>
  add_row(Date = ymd("2020-04-01"), Decisions = 0) |>
  
  arrange(Date) |> 
  
  write_csv("data-raw/flourish/2a - Safe routes/2a - resettlement - total.csv")

# Check the spike in decisions in Q2 2021
resettlement_grants_without_ARAP |> 
  filter(Date == ymd("2021-07-01")) |> 
  
  group_by(`Case outcome`, Nationality) |> 
  summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
  ungroup() |> 
  
  arrange(desc(Decisions))

# - How do current levels of resettlement compare to historical levels? -
resettlement_by_quarter <- 
  resettlement_grants_without_ARAP |> 
  
  group_by(Date) |> 
  summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
  ungroup() |> 
  
  # Manually add missing rows - no resettlement during lockdowns
  add_row(Date = ymd("2020-07-01"), Decisions = 0) |>
  add_row(Date = ymd("2020-04-01"), Decisions = 0) |>
  
  arrange(desc(Date))
  
# Calculate the annual sum over each set of 12 months, starting from the most recently available quarter
resettlement_by_quarter |> 
  # Calculate the annual rolling sum, starting with the most recent quarter
  mutate(RollSum = rollsum(Decisions, k = 4, na.pad = TRUE)) |> 
  
  # Keep the 2nd row (rolling sum for the past 12 months) and every 4th row after that
  slice(seq(2, n(), by = 4))

# ---- What safe routes have been available in the last 12 months and how many people (and age, nationality and gender) have arrived through each? ----
bind_rows(
  resettlement_grants_without_ARAP |> 
    # Filter applications within the last 12 months
    filter(Date >= today() - dmonths(12)) |> 
    group_by(Nationality) |> 
    summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
    arrange(desc(Decisions)) |> 
    slice(1:10) |> 
    rename(Category = Nationality, Nationality = Decisions) |> 
    mutate(Type = "Nationality"),
  
  resettlement_grants_without_ARAP |> 
    # Filter applications within the last 12 months
    filter(Date >= today() - dmonths(12)) |> 
    group_by(Age) |> 
    summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
    filter(Age != "Unknown") |> 
    rename(Category = Age, Age = Decisions) |> 
    mutate(Type = "Age"),
  
  resettlement_grants_without_ARAP |> 
    # Filter applications within the last 12 months
    filter(Date >= today() - dmonths(12)) |> 
    group_by(Sex) |> 
    summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
    filter(Sex != "Unknown") |> 
    rename(Category = Sex, Sex = Decisions) |> 
    mutate(Type = "Sex")
) |> 
  write_csv("data-raw/flourish/2a - Safe routes/2a - What safe routes have been available in the last 12 months - by category.csv")

# resettlement_by_nationality
resettlement_grants_without_ARAP |> 
  # Filter applications within the last 12 months
  filter(Date >= today() - dmonths(12)) |> 
  group_by(Nationality) |> 
  summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
  ungroup() |> 
  arrange(desc(Decisions)) |> 
  mutate(Prop = Decisions / sum(Decisions))

resettlement_by_age <- 
  resettlement_grants_without_ARAP |> 
  # Filter applications within the last 12 months
  filter(Date >= today() - dmonths(12)) |> 
  group_by(Age) |> 
  summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
  filter(Age != "Unknown") |> 
  rename(Category = Age, Age = Decisions) |> 
  mutate(Type = "Resettled") |> 
  relocate(Type) |> 
  mutate(Age = Age / sum(Age)) |> 
  pivot_wider(names_from = Category, values_from = Age) |> 
  relocate(`Under 18`, .after = Type)

resettlement_by_age

resettlement_by_age |> 
  write_csv("data-raw/flourish/2a - Safe routes/Resettlement - by age.csv")

resettlement_by_sex <- 
  resettlement_grants_without_ARAP |> 
  # Filter applications within the last 12 months
  filter(Date >= today() - dmonths(12)) |> 
  group_by(Sex) |> 
  summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
  filter(Sex != "Unknown") |> 
  rename(Category = Sex, Sex = Decisions) |> 
  mutate(Type = "Resettled") |> 
  relocate(Type) |> 
  mutate(Sex = Sex / sum(Sex)) |> 
  pivot_wider(names_from = Category, values_from = Sex)

resettlement_by_sex

resettlement_by_sex |> 
  write_csv("data-raw/flourish/2a - Safe routes/Resettlement - by sex.csv")

# ---- How many people have arrived from Ukraine through a safe route in the last 12 months? ----
source("https://github.com/britishredcrosssociety/ukraine-analyses/raw/main/R/load%20Ukraine%20visa%20data%20-%20scraped.R")

visas_scraped |> 
  ungroup() |> 
  filter(str_detect(Stage, "arrival")) |> 
  select(Date, Scheme, Arrivals = Visas_imputed) |> 
  pivot_wider(names_from = Scheme, values_from = Arrivals) |> 
  write_csv("data-raw/flourish/2a - Safe routes/2a - Arrivals from Ukraine.csv")

# ---- How many people have crossed the channel in a small boat and other ‘irregular entry’ by nationality, age and gender and what are the trends e.g. month by month/by quarter? ----
# Cumulative arrivals
asylum::irregular_migration |> 
  group_by(Year, `Method of entry`) |> 
  summarise(`Number of detections` = sum(`Number of detections`, na.rm = TRUE)) |> 
  ungroup() |> 
  pivot_wider(names_from = `Method of entry`, values_from = `Number of detections`) |> 
  mutate(across(-Year, cumsum)) |> 
  write_csv("data-raw/flourish/2a - Safe routes/2a - Irregular migration - trend.csv")

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

# Age/sex pyramid
asylum::family_reunion |> 
  # Filter applications within the last 12 months
  filter(Date >= today() - dmonths(12)) |> 
  group_by(Age, Sex) |> 
  summarise(`Visas granted` = sum(`Visas granted`, na.rm = TRUE)) |> 
  
  filter(Age != "Unknown") |> 
  filter(Sex != "Unknown Sex") |> 
  
  pivot_wider(names_from = Sex, values_from = `Visas granted`) |> 
  mutate(Female = Female * -1) |> 
  
  write_csv("data-raw/flourish/2a - Safe routes/2a - Family reunion - by age and sex.csv")
