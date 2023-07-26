library(tidyverse)
library(asylum)

# ---- How many people have been and are in detention in the last 12 months? ----
leaving_detention <- 
  asylum::people_leaving_detention |> 
  # Filter visas within the last 12 months
  filter(Date >= max(Date) - dmonths(11)) |> 
  group_by(`Reason for leaving detention`) |> 
  summarise(`Number of people` = sum(Leaving, na.rm = TRUE)) |> 
  
  mutate(`Reason for leaving detention` = case_when(
    `Reason for leaving detention` == "Bailed (IJ)" ~ "Bailed (First-tier Tribunal)",
    `Reason for leaving detention` == "Bailed (SoS)" ~ "Bailed (Secretary of State)",
    `Reason for leaving detention` == "Granted LTE/LTR" ~ "Granted leave to enter/remain",
    `Reason for leaving detention` == "Other" ~ "Other",
    `Reason for leaving detention` == "Returned" ~ "Removed"
  )) |> 
  
  mutate(`Detention status` = "Left detention in last 12 months") |> 
  relocate(`Detention status`)

in_detention <- 
  asylum::people_in_detention |> 
  filter(Date == max(Date)) |> 
  summarise(`Number of people` = sum(People, na.rm = TRUE)) |> 
  
  mutate(
    `Detention status` = "In detention, as of March 2023",
    `Reason for leaving detention` = "In detention",
  ) |> 
  relocate(`Detention status`, `Reason for leaving detention`)

bind_rows(leaving_detention, in_detention) |> 
  # mutate(`Reason for leaving detention` = replace_na(`Reason for leaving detention`, "")) |> 
  arrange(desc(`Number of people`)) |> 
  write_csv("data-raw/flourish/2b - Detention/2b - detention - totals.csv")

# - Caption -
leaving_detention |> 
  mutate(Proportion = `Number of people` / sum(`Number of people`)) |> 
  mutate(Proportion_cumulative = cumsum(Proportion))

# sum(leaving_detention$`People leaving detention`)
# sum(in_detention$`People in detention`)

# ---- Who is in immigration detention (nationality, age, gender w/ focus on number of women in detention- and pregnant women) ----
# - Nationality -
people_in_detention_by_nationality <- 
  asylum::people_in_detention |> 
  filter(Date == max(Date)) |> 
  group_by(Nationality) |> 
  summarise(`People in detention` = sum(People)) |> 
  ungroup() |> 
  arrange(desc(`People in detention`))

people_in_detention_by_nationality |>
  slice(1:20) |> 
  write_csv("data-raw/flourish/2b - Detention/2b - detention - by nationality.csv")

# - Caption -
people_in_detention_by_nationality |> 
  mutate(Proportion = `People in detention` / sum(`People in detention`)) |> 
  mutate(Proportion_cumulative = cumsum(Proportion))
round(438/1591 * 100, 0)

# ---- People in detention, by age and sex ----
people_in_detention_by_age_sex <- 
  asylum::people_in_detention |> 
  filter(Date == max(Date)) |> 
  group_by(Age, Sex) |> 
  summarise(`People in detention` = sum(People)) |> 
  
  filter(Age != "Unknown") |> 
  filter(Sex != "Unknown Sex") |> 
  
  pivot_wider(names_from = Sex, values_from = `People in detention`) |> 
  mutate(Female = Female * -1)

people_in_detention_by_age_sex |> 
  write_csv("data-raw/flourish/2b - Detention/2b - detention - by age and sex.csv")

# How many pregnant women are currently in detention?
asylum::detention_pregnant_women |> 
  filter(Date == max(Date))

# - Caption -
people_in_detention_by_age_sex |> 
  ungroup() |> 
  mutate(Female = Female * -1) |> 
  summarise(
    Female = sum(Female),
    Male = sum(Male),
    Total = sum(Female) + sum(Male)
  ) |> 
  mutate(Proportion = Male / (Male + Female))

# ---- How long have people been in immigration detention? How many detained for more than 28 days? ----
detention_length <- 
  asylum::people_leaving_detention |>
  filter(Date == max(Date)) |> 
  
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
  write_csv("data-raw/flourish/2b - Detention/2b - detention - by length.csv")

# Number detained for longer than 28 days
detention_length |> 
  filter(!`Length of detention` %in% c("3 days or less", "4 to 7 days", "8 to 14 days", "15 to 28 days")) |> 
  summarise(sum(People))
  
# ---- How many people are in immigration detention and what is the size of immigration detention estate? Baseline from previous years ----
asylum::detention_cost_per_day |> 
  select(Date, Cost) |> 
  write_csv("data-raw/flourish/2b - Detention/2b - detention - cost per day.csv")

# calculate % increase since April 2021
asylum::detention_cost_per_day |> 
  select(Date, Cost) |> 
  filter(Date %in% c(ymd("2021-04-01"), max(Date))) |> 
  # arrange(desc(Date)) |> 
  mutate(Percent_change = (Cost - lag(Cost)) / lag(Cost))
