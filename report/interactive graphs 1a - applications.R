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
