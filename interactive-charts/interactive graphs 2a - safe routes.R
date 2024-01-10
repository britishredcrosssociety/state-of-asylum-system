library(tidyverse)
library(asylum)
library(zoo)

resettlement_grants_without_evacuation <- 
  asylum::decisions_resettlement |> 
  filter(`Case type` == "Resettlement Case" & `Case outcome group` == "Grant of Protection") |> 
  filter(!str_detect(`Case outcome`, "Relocation - ARAP")) |> 
  filter(!str_detect(`Case outcome`, "Resettlement - ACRS Pathway 1")) |> 
  filter(!str_detect(`Case outcome`, "Resettlement - ACRS Pathway 3")) 

# ---- Flourish- Section 2, Slide 4: People arriving in the UK through resettlement ----
resettlement_by_quarter <- 
  resettlement_grants_without_evacuation |> 
  group_by(Date) |> 
  summarise(Arrivals = sum(Decisions, na.rm = TRUE)) |> 
  ungroup() |> 
  
  # Manually add missing rows - no resettlement during lockdowns
  add_row(Date = ymd("2020-06-30"), Arrivals = 0) |>
  add_row(Date = ymd("2020-09-30"), Arrivals = 0) |>
  
  arrange(Date)

resettlement_by_quarter |> 
  write_csv("data-raw/flourish/2a - Safe routes/2a - resettlement - total Sept 23.csv")

# - CAPTION -
# Number of people resettled over last 12 months
resettlement_grants_without_evacuation |> 
  filter(Date >= max(Date) - dmonths(11)) |> 
  summarise(Decisions = sum(Decisions, na.rm = TRUE))

# How do current levels of resettlement compare to historical levels?
# Calculate the annual sum over each set of 12 months, starting from the most recently available quarter
resettlement_by_quarter |>
  arrange(desc(Date)) |> 

  # Calculate the annual rolling sum, starting with the most recent quarter
  mutate(RollSum = rollsum(Arrivals, k = 4, na.pad = TRUE)) |> 
  
  # Keep the 2nd row (rolling sum for the past 12 months) and every 4th row after that
  slice(seq(1, n(), by = 4))

# ---- Flourish- Section 2, Slide 5: Resettlement over the last 12 months ----
bind_rows(
  resettlement_grants_without_evacuation |> 
    # Filter applications within the last 12 months
    filter(Date >= max(Date) - dmonths(11)) |> 
    group_by(Nationality) |> 
    summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
    ungroup() |> 
    slice_max(Decisions, n = 10) |> 
    mutate(Nationality = if_else(Nationality == "Sudan (South)", "S. Sudan", Nationality)) |> 
    rename(Category = Nationality, Nationality = Decisions) |> 
    mutate(Type = "Nationality"),
  
  resettlement_grants_without_evacuation |> 
    # Filter applications within the last 12 months
    filter(Date >= max(Date) - dmonths(11)) |> 
    group_by(Age) |> 
    summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
    filter(Age != "Unknown") |> 
    rename(Category = Age, Age = Decisions) |> 
    arrange(match(Category, c("Under 18", "18-29", "30-49", "50-69", "70+"))) |> 
    mutate(Type = "Age"),
  
  resettlement_grants_without_evacuation |> 
    # Filter applications within the last 12 months
    filter(Date >= max(Date) - dmonths(11)) |> 
    group_by(Sex) |> 
    summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
    filter(Sex != "Unknown") |> 
    rename(Category = Sex, Sex = Decisions) |> 
    mutate(Type = "Sex")
) |> 
  write_csv("data-raw/flourish/2a - Safe routes/2a - resettlement - by category Sept 23.csv")

# - CAPTION -
# Proportions of resettlement cases by nationality
resettlement_grants_without_evacuation |> 
  # Filter applications within the last 12 months
  filter(Date >= max(Date) - dmonths(11)) |> 
  group_by(Nationality) |> 
  summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
  ungroup() |> 
  arrange(desc(Decisions)) |> 
  mutate(Prop = Decisions / sum(Decisions))

# Proportion of resettlement cases by age
resettlement_grants_without_evacuation |> 
  # Filter applications within the last 12 months
  filter(Date >= max(Date) - dmonths(11)) |> 
  group_by(Age) |> 
  summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
  filter(Age != "Unknown") |> 
  rename(Category = Age, Age = Decisions) |> 
  mutate(Type = "Resettled") |> 
  relocate(Type) |> 
  mutate(Age = Age / sum(Age)) |> 
  pivot_wider(names_from = Category, values_from = Age) |> 
  relocate(`Under 18`, .after = Type)

# Proportion of resettlement cases by sex
resettlement_grants_without_evacuation |> 
  # Filter applications within the last 12 months
  filter(Date >= max(Date) - dmonths(11)) |> 
  group_by(Sex) |> 
  summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
  filter(Sex != "Unknown") |> 
  rename(Category = Sex, Sex = Decisions) |> 
  mutate(Type = "Resettled") |> 
  relocate(Type) |> 
  mutate(Sex = Sex / sum(Sex)) |> 
  pivot_wider(names_from = Category, values_from = Sex)

# ---- Flourish- Section 2, Slide 6: Arrivals from Ukraine via the UK Ukraine Visa Schemes ----
source("https://github.com/britishredcrosssociety/ukraine-analyses/raw/main/R/load%20Ukraine%20visa%20data%20-%20scraped.R")

ukraine <- 
  visas_scraped |> 
  ungroup() |> 
  filter(str_detect(Stage, "arrival")) |> 
  select(Date, Scheme, Arrivals = Visas_imputed) |> 
  pivot_wider(names_from = Scheme, values_from = Arrivals) |> 
  relocate(`Ukraine Sponsorship Scheme`, .before = `Ukraine Family Scheme`)

ukraine |> 
  write_csv("data-raw/flourish/2a - Safe routes/2a - Arrivals from Ukraine Sept 23.csv")

# - CAPTION -
ukraine |> 
  filter(Date == max(Date)) |> 
  mutate(Total = `Ukraine Sponsorship Scheme` + `Ukraine Family Scheme`) |> 
  mutate(Proportion_Sponsorship = `Ukraine Sponsorship Scheme` / (`Ukraine Sponsorship Scheme` + `Ukraine Family Scheme`))

# ---- Flourish- Section 2, Slide 7: Arrivals through resettlement routes and other routes ----
# Stacked bar chart by nationality, showing the proportion of people who arrived 
# through resettlement routes and proportion of those who arrived outside of them. 
# For the last 12 months.
# The nationalities we select can be the 10 most common when considering both means of arrival?

# People arriving through resettlement routes
arrivals_resettlement <- 
  resettlement_grants_without_evacuation |> 
  filter(Date >= max(Date) - dmonths(11)) |> 
  group_by(Nationality) |> 
  summarise(`People resettled` = sum(Decisions, na.rm = TRUE)) |> 
  ungroup()

# People arriving through family reunion
arrivals_family_reunion <- 
  family_reunion |> 
  filter(Date >= max(Date) - dmonths(11)) |> 
  group_by(Nationality) |> 
  summarise(`Family reunion visas granted` = sum(`Visas granted`, na.rm = TRUE)) |> 
  ungroup()

# People arriving on small boats who claimed asylum
arrivals_small_boats <- 
  asylum::small_boat_asylum_applications |> 
  filter(Date >= max(Date) - dmonths(11)) |> view()
  filter(`Asylum application` == "Asylum application raised") |> 
  group_by(Nationality) |> 
  summarise(`People arriving via small boat and claiming asylum` = sum(Applications, na.rm = TRUE)) |> 
  ungroup()

# - September 2023 irregular migration stats not updated to asylum package in r. 

# - Downloaded and imported irregular migration stats for September 2023 via following link.. 

# source: https://www.gov.uk/government/statistical-data-sets/irregular-migration-detailed-dataset-and-summary-tables

# Revised code with updated September 2023 irregular migration: 

library(readxl)

irregular_migration_to_the_UK_detailed_dataset_year_ending_september_2023_1_ <- read_excel("~/GitHub/state-of-asylum-system/data-raw/data source/irregular-migration-to-the-UK-detailed-dataset-year-ending-september-2023 (1).xlsx", 
                                                                                               +     sheet = "Data - Irr_D02", skip = 1)

small_boat_asylum_0923 <- irregular_migration_to_the_UK_detailed_dataset_year_ending_september_2023_1_

small_boat_asylum_0923 <- small_boat_asylum_0923 [-3970,]

# small_boat_asylum_0923 does not have dates, therefore,   filter(Date >= max(Date) - dmonths(11)) |> cannot work for 
# this dataset. Following code is to check when filtered by quarter, the output matches the code above. It does. 

small_boat_asylum_0923 |>
  filter(Quarter > "2022 Q2") |> 
  filter(Quarter < "2023 Q3") |> 
  filter(`Asylum application` == "Asylum application raised") |> 
  group_by(Nationality) |> 
  summarise(`People arriving via small boat and claiming asylum` = sum(Applications, na.rm = TRUE)) |> view()

arrivals_small_boats <- 
small_boat_asylum_0923 |>
  filter(Quarter > "2022 Q3") |>
  filter(`Asylum application` == "Asylum application raised") |> 
  group_by(Nationality) |> 
  summarise(`People arriving via small boat and claiming asylum` = sum(Applications, na.rm = TRUE)) |> 
  ungroup()
  
# People arriving to claim asylum (including small boats)
arrivals_asylum <- 
  asylum::applications |> 
  filter(Date >= max(Date) - dmonths(11)) |> 
  group_by(Nationality) |> 
  summarise(`People applying for asylum` = sum(Applications, na.rm = TRUE)) |> 
  ungroup()

# Combine datasets
arrivals_all <- 
  arrivals_asylum |> 
  left_join(arrivals_small_boats) |> 
  left_join(arrivals_family_reunion) |> 
  left_join(arrivals_resettlement) |>
  
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) |> 
  
  # Calculate people applying for asylum who didn't arrive via small boats
  mutate(`People arriving via other routes and claiming asylum` = `People applying for asylum` - `People arriving via small boat and claiming asylum`) |> 
  select(-`People applying for asylum`) |> 
  
  mutate(Total = `People arriving via other routes and claiming asylum` + `People arriving via small boat and claiming asylum` + `People resettled` + `Family reunion visas granted`) |> 
  relocate(`People arriving via other routes and claiming asylum`, .after = `People arriving via small boat and claiming asylum`)

# Save top ten nationalities by total numbers of people arriving
arrivals_all |> 
  slice_max(Total, n = 10) |> 
  select(-Total) |> 
  write_csv("data-raw/flourish/2a - Safe routes/arrival routes Sept 23.csv")

# - CAPTION -
# What % of arrivals were through resettlement routes?
arrivals_all |> 
  mutate(Total = `People arriving via small boat and claiming asylum` + `People arriving via other routes and claiming asylum` + `Family reunion visas granted` + `People resettled`) |> 
  summarise(across(where(is.numeric), sum)) |> 
  mutate(Proportion = (`Family reunion visas granted` + `People resettled`) / Total) |> 
  pull(Proportion) |> 
  scales::percent()  

# ---- Flourish- Section 2, Slide 8 & 9: Family reunion visas granted ----
asylum::family_reunion |> 
  group_by(Date) |> 
  summarise(`Visas granted` = sum(`Visas granted`, na.rm = TRUE)) |> 
  write_csv("data-raw/flourish/2a - Safe routes/2a - Family reunion Sept 23.csv")

# Family reunion under 18
family_reunion |>
  filter(Age == "Under 18") |>
  group_by(Date) |>
  summarise(`Visas granted` = sum(`Visas granted`, na.rm = TRUE)) |> 
  write_csv("data-raw/flourish/2a - Safe routes/2a - Family reunion Under 18 sept 23.csv")

# - CAPTION -
asylum::family_reunion |> 
  # Filter visas within the last 12 months
  filter(Date >= max(Date) - dmonths(11)) |> 
  summarise(`Visas granted` = sum(`Visas granted`, na.rm = TRUE))

# - How do current levels of visa grants compare to historical levels? -
family_reunion_rolling_sum <- 
  asylum::family_reunion |> 
  group_by(Date) |> 
  summarise(`Visas granted` = sum(`Visas granted`, na.rm = TRUE)) |> 
  ungroup() |>
  
  arrange(desc(Date)) |> 

  # Calculate the annual sum over each set of 12 months, starting from the most recently available quarter
  mutate(RollSum = rollsum(`Visas granted`, k = 4, na.pad = TRUE)) |> 
  
  # Keep the 2nd row (rolling sum for the past 12 months) and every 4th row after that
  slice(seq(1, n(), by = 4))

family_reunion_rolling_sum 

view(family_reunion_rolling_sum)

# Check trend in rolling sum
# ggplot(family_reunion_rolling_sum, aes(x = Date, y = RollSum)) +
#   geom_line() +
#   scale_y_continuous(limits = c(0, NA))
