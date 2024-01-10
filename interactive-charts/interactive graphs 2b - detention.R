library(tidyverse)
library(asylum)

# ---- Flourish- Section 2, Slide 10: People in detention, by age and sex ----
people_in_detention_by_age_sex <- 
  asylum::people_in_detention |> 
  filter(Date == max(Date)) |> 
  group_by(Age, Sex) |> 
  summarise(`People in detention` = sum(People)) |> 
  
  filter(Age != "Unknown") |> 
  filter(Age != "Not available") |> 
  filter(Sex != "Unknown Sex") |> 
  
  pivot_wider(names_from = Sex, values_from = `People in detention`) |> 
  mutate(Female = Female * -1)

people_in_detention_by_age_sex |> 
  write_csv("data-raw/flourish/2b - Detention/2b - detention - by age and sex spt 23.csv")

# Pregnant women in detention not updated in R asylum package. 

# From transparency data published for Sept 2023, there are no women in detention for this quarter. 

# How many pregnant women are currently in detention?
asylum::detention_pregnant_women |> 
  filter(Date == max(Date))

# - CAPTION -
people_in_detention_by_age_sex |> 
  ungroup() |> 
  mutate(Female = Female * -1) |> 
  summarise(
    Female = sum(Female),
    Male = sum(Male),
    Total = sum(Female) + sum(Male)
  ) |> 
  mutate(Proportion = Male / (Male + Female))

# ---- Flourish- Section 2, Slide 11: Length of detention ----
detention_length <- 
  asylum::people_leaving_detention |>
  filter(Date >= max(Date) - dmonths(11)) |> 
  
  # Remove initial letter and colon
  mutate(`Length of detention` = str_remove(`Length of detention`, "^[A-Z]:\\s")) |> 

  # Group into fewer categories
  mutate(`Length of detention` = case_match(
    `Length of detention`,
    c("3 days or less", "4 to 7 days", "8 to 14 days") ~ "A: Up to two weeks",
    c("15 to 28 days") ~ "B: 15 to 28 days",
    .default = "C: 29 days or more"
  )) |> 
  
  group_by(`Length of detention`) |> 
  summarise(People = sum(Leaving, na.rm = TRUE)) |> 
  
  # Remove initial letter and colon
  mutate(`Length of detention` = str_remove(`Length of detention`, "^[A-Z]:\\s"))

detention_length |> 
  write_csv("data-raw/flourish/2b - Detention/2b - detention - by length sep 23.csv")

# Average cost per day for immigration detention not updated in R asylum package for September 2023 stats. 

# Updated for September only: 
library(readxl)

IE_Q3_2023_Background_Published <- read_excel("~/GitHub/state-of-asylum-system/data-raw/data source/IE__Q3_2023_Background_Published.xlsx", 
                                              +     sheet = "DT_02", skip = 3)

Detention_cost_per_day_0923 <- IE_Q3_2023_Background_Published |>
  select_if(~ !any(is.na(.)))

Detention_cost_per_day_0923 |>
  select(Quarter, Cost) |>
  write_csv("data-raw/flourish/2b - Detention/2b - detention - cost per day Sept 23.csv")

# ---- Flourish- Section 2, Slide 12: Average cost per day to hold a person in immigration detention ----
asylum::detention_cost_per_day |> 
  select(Date, Cost) |> 
  write_csv("data-raw/flourish/2b - Detention/2b - detention - cost per day.csv")

# - CAPTION -
# calculate % increase since April 2021
asylum::detention_cost_per_day |> 
  select(Date, Cost) |> 
  filter(Date %in% c(ymd("2021-06-30"), max(Date))) |> 
  mutate(Cost = as.numeric(Cost)) |> 
  # arrange(desc(Date)) |> 
  mutate(Percent_change = (Cost - lag(Cost)) / lag(Cost))
