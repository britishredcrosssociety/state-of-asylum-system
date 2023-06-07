library(tidyverse)
library(asylum)

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
asylum::applications |> 
  # Filter applications within the last 12 months
  filter(Date >= today() - dmonths(12)) |> 
  group_by(Nationality, Age, Sex, UASC) |> 
  summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/applications - by category.csv")
  
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

# ---- Inadmissibility ----
unique(asylum::inadmissibility_cases_considered$Stage)

asylum::inadmissibility_cases_considered |> 
  group_by(Year, Stage) |> 
  summarise(Cases = sum(Cases)) |> 
  ungroup()



