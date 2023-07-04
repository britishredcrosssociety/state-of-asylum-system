library(tidyverse)
library(asylum)

# ---- Migration comparison/proportions ----
# Total applications in 2022
migration_asylum <- 
  asylum::applications |> 
  filter(Year == 2022) |> 
  summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
  pull(Applications)

# People arriving on small boats who claimed asylum
migration_small_boats <- 
  asylum::small_boat_asylum_applications |> 
  filter(Year == 2022 & `Asylum application` == "Asylum application raised") |> 
  summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
  pull(Applications)

# asylum::irregular_migration |> 
#   filter(Year == 2022 & str_detect(`Method of entry`, "boat")) |>   
#   summarise(Boats = sum(`Number of detections`, na.rm = TRUE)) |> 
#   pull(Boats)

resettlement <- 
  asylum::decisions_resettlement |> 
  filter(Year == 2022 & str_detect(`Case type`, "Resettlement")) |> 
  summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
  pull(Decisions)

# These migration figures are taken from ONS
# Source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/internationalmigration/bulletins/longterminternationalmigrationprovisional/yearendingdecember2022
net_migration <- 606000 
immigration_2022 <- 1163000
ukraine <- 114000
bno <- 52000  # British Nationals Overseas - BN(O)

tribble(
  ~Category, ~`Sub-category`, ~`Migration type`, ~`Number of people`,
  "Immigration", "Non-asylum migration", "Non-asylum migration", (immigration_2022 - ukraine - bno - migration_asylum - resettlement),
  "Immigration", "Non-asylum migration", "Ukraine visas", ukraine,
  "Immigration", "Non-asylum migration", "British Nationals Overseas", bno,
  "Immigration", "Asylum claims", "Asylum claims (not via small boats)", (migration_asylum - migration_small_boats),
  "Immigration", "Asylum claims", "Small boat arrivals claiming asylum", migration_small_boats,
  "Immigration", "Non-asylum migration", "Resettlement", resettlement
) |> 
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/immigration.csv")

# ---- Graph 1: Total annual applications over time ----
asylum::applications |> 
  group_by(Date) |> 
  summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/applications - total.csv")

# Check historical numbers of applications for the same quarter as the most recently published stats
current_quarter <- quarter(max(asylum::applications$Date))

asylum::applications |> 
  filter(quarter(Date) == current_quarter) |> 
  group_by(Date) |> 
  summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
  arrange(desc(Applications))

# ---- Graph 2: Breakdown of applications within last 12 months ----
# asylum::applications |> 
#   # Filter applications within the last 12 months
#   filter(Date >= today() - dmonths(12)) |> 
#   group_by(Nationality, Age, Sex, UASC) |> 
#   summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
#   write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/applications - by category.csv")
  
bind_rows(
  asylum::applications |> 
    # Filter applications within the last 12 months
    filter(Date >= today() - dmonths(12)) |> 
    group_by(Nationality) |> 
    summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
    arrange(desc(Applications)) |> 
    slice(1:10) |> 
    rename(Category = Nationality, Nationality = Applications) |> 
    mutate(Type = "Nationality"),
  
  asylum::applications |> 
    # Filter applications within the last 12 months
    filter(Date >= today() - dmonths(12)) |> 
    group_by(Age) |> 
    summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
    filter(Age != "Unknown") |> 
    rename(Category = Age, Age = Applications) |> 
    mutate(Type = "Age"),
  
  asylum::applications |> 
    # Filter applications within the last 12 months
    filter(Date >= today() - dmonths(12)) |> 
    group_by(Sex) |> 
    summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
    filter(Sex != "Unknown Sex") |> 
    rename(Category = Sex, Sex = Applications) |> 
    mutate(Type = "Sex"),
  
  asylum::applications |> 
    # Filter applications within the last 12 months
    filter(Date >= today() - dmonths(12)) |> 
    group_by(UASC) |> 
    summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
    rename(Category = UASC, UASC = Applications) |> 
    mutate(Type = "UASC")
) |> 
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/applications - by category.csv")

# Age pyramid
asylum::applications |> 
  # Filter applications within the last 12 months
  filter(Date >= today() - dmonths(12)) |> 
  group_by(Age, Sex) |> 
  summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
  
  filter(Age != "Unknown") |> 
  filter(Sex != "Unknown Sex") |> 

  pivot_wider(names_from = Sex, values_from = Applications) |> 
  mutate(Female = Female * -1) |> 
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/applications - age pyramid.csv")

# Top five countries applying for asylum over time
asylum::applications |> 
  group_by(Year, Region, Nationality) |> 
  summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
  ungroup() |> 
  
  group_by(Year) |> 
  slice_max(Applications, n = 5) |> 
  ungroup() |> 
  
  mutate(Applications = as.character(Applications)) |> 
  # mutate(Applications = replace_na(Applications, "")) |> 
  
  pivot_wider(names_from = Year, values_from = Applications) |> 
  mutate(across(-(Region:Nationality), ~ replace_na(.x, ""))) |> 
  
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/applications - top five nations.csv")

# Asylum over time, by nationality
asylum::applications |> 
  group_by(Year, Nationality) |> 
  summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
  ungroup() |> 
  
  pivot_wider(names_from = Nationality, values_from = Applications) |> 
  mutate(across(-(Year), as.character)) |> 
  mutate(across(-(Year), ~ replace_na(.x, ""))) |> 
  
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/applications - by nation.csv")

# ---- Initial grant rates ----
# Top ten nations, by number of grants and grant rate in the most recent year
top_ten_nations <- 
  grant_rates_initial_annual |> 
  filter(Year == max(Year)) |> 
  arrange(desc(Grant), desc(`Initial grant rate`)) |> 
  slice(1:10) |> 
  pull(Nationality)

grant_rates_initial_annual |> 
  filter(Year >= max(Year) - 1) |> 
  filter(Nationality %in% top_ten_nations) |> 
  select(Nationality, Year, `Initial grant rate`, `Number of grants` = Grant) |> 
  sort(desc(`Initial grant rate`)) |> 
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/initial-grant-rates-annual-recent.csv")

grant_rates_initial_annual |> 
  group_by(Year) |> 
  summarise(
    Grant = sum(Grant, na.rm = TRUE),
    Refused = sum(Refused, na.rm = TRUE)
  ) |> 
  ungroup() |>
  mutate(`Initial grant rate` = Grant / (Grant + Refused)) |> 
  select(Year, `Initial grant rate`) |> 
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/initial-grant-rates-annual-total.csv")

# Make a wider version of initial grant rates quarterly data for testing in a Flourish Studio chart
grant_rates_initial_quarterly |> 
  select(Date, Quarter, Nationality, `Initial grant rate`) |> 
  pivot_wider(names_from = Nationality, values_from = `Initial grant rate`) |> 
  
  # Move the ten nations with the highest number of grants and highest grant rates to the left, so they get shown on the chart by default
  relocate(Date, Quarter, any_of(top_ten_nations)) |> 
  
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/initial-grant-rates-quarterly-wide.csv")

# ---- Graph 3: Returns ----
# How many and who have been returned
asylum::returns |> 
  group_by(Year, `Return type group`) |> 
  summarise(`Number of returns` = sum(`Number of returns`, na.rm = TRUE)) |> 
  
  pivot_wider(names_from = `Return type group`, values_from = `Number of returns`) |> 
  
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/returns - overall.csv")

asylum::returns |> 
  group_by(Year, Age) |> 
  summarise(`Number of returns` = sum(`Number of returns`, na.rm = TRUE)) |> 
  filter(Age != "Unknown") |> 
  pivot_wider(names_from = Age, values_from = `Number of returns`) |> 
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/returns - by age.csv")

asylum::returns |> 
  group_by(Year, Sex) |> 
  summarise(`Number of returns` = sum(`Number of returns`, na.rm = TRUE)) |> 
  filter(!str_detect(Sex, "Unknown")) |> 
  pivot_wider(names_from = Sex, values_from = `Number of returns`) |> 
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/returns - by sex.csv")

# Top five nations, by number of returns in the most recent year
top_five_nations <- 
  returns_by_destination |> 
  filter(Year == max(Year)) |> 
  group_by(Year, `Return destination`) |> 
  summarise(`Number of returns` = sum(`Number of returns`, na.rm = TRUE)) |> 
  arrange(desc(`Number of returns`)) |> 
  slice(1:5) |> 
  pull(`Return destination`)

returns_by_destination |> 
  group_by(`Return destination`) |> 
  summarise(`Number of returns` = sum(`Number of returns`, na.rm = TRUE)) |> 
  ungroup() |> 
  arrange(desc(`Number of returns`))

# Make a wider version of returns quarterly data
asylum::returns_by_destination |> 
  select(Date, `Return destination`, `Number of returns`) |> 
  
  group_by(Date, `Return destination`) |> 
  summarise(`Number of returns` = sum(`Number of returns`, na.rm = TRUE)) |> 
  
  pivot_wider(names_from = `Return destination`, values_from = `Number of returns`) |> 
  
  # Move the ten nations with the highest number of grants and highest grant rates to the left, so they get shown on the chart by default
  relocate(Date, any_of(top_five_nations)) |> 
  
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/returns - by destination.csv")

# Returns by whether the person was seeking asylum or not
asylum::returns_asylum |> 
  relocate(`Voluntary returns`, .after = Nationality) |>  # Reorder so voluntary returns comes first in the stacked bars
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/returns - by asylum.csv")

asylum::returns_asylum |> 
  filter(Category == "Asylum-related") |> 
  relocate(`Voluntary returns`, .after = Nationality) |>  # Reorder so voluntary returns comes first in the stacked bars
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/returns - by asylum only.csv")

asylum::returns_asylum |>
  mutate(Total = `Enforced returns` + `Voluntary returns` + `Refused entry at port and subsequently departed`) |> 
  
  group_by(Category) |> 
  summarise(Total = sum(Total)) |> 
  ungroup() |> 
  
  mutate(prop = scales::percent(Total / sum(Total)))

# ---- Inadmissibility ----
unique(asylum::inadmissibility_cases_considered$Stage)

asylum::inadmissibility_cases_considered |> 
  group_by(Stage) |> 
  summarise(Cases = sum(Cases)) |> 
  ungroup()

asylum::notices_of_intent |> 
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/inadmissibility - notices of intent.csv")
