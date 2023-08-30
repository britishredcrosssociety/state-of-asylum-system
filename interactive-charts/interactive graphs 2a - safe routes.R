library(tidyverse)
library(asylum)
library(zoo)

resettlement_grants_without_evacuation <- 
  asylum::decisions_resettlement |> 
  filter(`Case type` == "Resettlement Case" & `Case outcome group` == "Grant of Protection") |> 
  filter(!str_detect(`Case outcome`, "Relocation - ARAP")) |> 
  filter(!str_detect(`Case outcome`, "Resettlement - ACRS Pathway 1")) |> 
  filter(!str_detect(`Case outcome`, "Resettlement - ACRS Pathway 3"))

# ---- How many people have been granted protection in the UK having arrived through a safe route? ----
resettlement_grants_without_evacuation |> 
  group_by(Date) |> 
  summarise(Arrivals = sum(Decisions, na.rm = TRUE)) |> 
  ungroup() |> 
  
  # Manually add missing rows - no resettlement during lockdowns
  add_row(Date = ymd("2020-07-01"), Arrivals = 0) |>
  add_row(Date = ymd("2020-04-01"), Arrivals = 0) |>
  
  arrange(Date) |> 
  
  write_csv("data-raw/flourish/2a - Safe routes/2a - resettlement - total.csv")

# Check the spike in decisions in Q2 2021
# resettlement_grants_without_evacuation |> 
#   filter(Date == ymd("2021-07-01")) |> 
#   
#   group_by(`Case outcome`, Nationality) |> 
#   summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
#   ungroup() |> 
#   
#   arrange(desc(Decisions)) |> 
#   
#   # separate_wider_delim(`Case outcome`, delim = " - ", names = c("Resettlement", "Scheme", "Accommodation"))
#   mutate(ACRS = if_else(str_detect(`Case outcome`, "ACRS"), "ACRS", "Other")) |> 
#   
#   group_by(ACRS) |> 
#   summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
#   ungroup() |> 
#   mutate(Proportion = Decisions / sum(Decisions))

# - CAPTION -
# Number of people resettled over last 12 months
resettlement_grants_without_evacuation |> 
  filter(Date >= max(Date) - dmonths(11)) |> 
  summarise(Decisions = sum(Decisions, na.rm = TRUE))

# How do current levels of resettlement compare to historical levels?
resettlement_by_quarter <- 
  resettlement_grants_without_evacuation |> 
  
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
  resettlement_grants_without_evacuation |> 
    # Filter applications within the last 12 months
    filter(Date >= max(Date) - dmonths(11)) |> 
    group_by(Nationality) |> 
    summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
    arrange(desc(Decisions)) |> 
    slice(1:10) |> 
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
  write_csv("data-raw/flourish/2a - Safe routes/2a - resettlement - by category.csv")

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

# resettlement_by_age
# 
# resettlement_by_age |> 
#   write_csv("data-raw/flourish/2a - Safe routes/Resettlement - by age.csv")

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

# resettlement_by_sex
# 
# resettlement_by_sex |> 
#   write_csv("data-raw/flourish/2a - Safe routes/Resettlement - by sex.csv")

# ---- How many people have arrived from Ukraine through a safe route in the last 12 months? ----
source("https://github.com/britishredcrosssociety/ukraine-analyses/raw/main/R/load%20Ukraine%20visa%20data%20-%20scraped.R")

ukraine <- 
  visas_scraped |> 
  ungroup() |> 
  filter(str_detect(Stage, "arrival")) |> 
  select(Date, Scheme, Arrivals = Visas_imputed) |> 
  pivot_wider(names_from = Scheme, values_from = Arrivals) |> 
  relocate(`Ukraine Sponsorship Scheme`, .before = `Ukraine Family Scheme`)

ukraine |> 
  write_csv("data-raw/flourish/2a - Safe routes/2a - Arrivals from Ukraine.csv")

# - Caption -
ukraine |> 
  filter(Date == max(Date)) |> 
  mutate(Total = `Ukraine Sponsorship Scheme` + `Ukraine Family Scheme`) |> 
  mutate(Proportion_Sponsorship = `Ukraine Sponsorship Scheme` / (`Ukraine Sponsorship Scheme` + `Ukraine Family Scheme`))

# ---- How many people have crossed the channel in a small boat and other ‘irregular entry’ ----
# Cumulative arrivals
asylum::irregular_migration |> 
  group_by(Year, `Method of entry`) |> 
  summarise(`Number of detections` = sum(`Number of detections`, na.rm = TRUE)) |> 
  ungroup() |> 
  pivot_wider(names_from = `Method of entry`, values_from = `Number of detections`) |> 
  mutate(across(-Year, cumsum)) |> 
  select(Year, `Small boat arrivals`, `Recorded detections in the UK`, `Inadequately documented air arrivals`, `Recorded detections at UK ports`) |> 
  write_csv("data-raw/flourish/2a - Safe routes/2a - Irregular migration - trend.csv")

# - Caption -
irregular_migration_last_12_months <- 
  asylum::irregular_migration |> 
  filter(Date >= max(Date) - dmonths(11)) |> 
  group_by(`Method of entry`) |> 
  summarise(`Number of detections` = sum(`Number of detections`, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(Proportion = `Number of detections` / sum(`Number of detections`))

irregular_migration_last_12_months
sum(irregular_migration_last_12_months$`Number of detections`)

# ---- 'Irregular' migration by nationality ----
irregular_migration_by_nationality <- 
  asylum::irregular_migration |> 
  filter(Date >= max(Date) - dmonths(11)) |> 
  group_by(Nationality) |> 
  summarise(`Number of detections` = sum(`Number of detections`, na.rm = TRUE)) |> 
  arrange(desc(`Number of detections`))

# Top five nations, by number of returns in the most recent year
top_five_nations <- 
  irregular_migration_by_nationality |> 
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

# - Caption -
irregular_migration_by_nationality |> 
  mutate(Proportion = `Number of detections` / sum(`Number of detections`)) |> 
  mutate(Proportion_cumulative = cumsum(Proportion))

# ---- 'Irregular' migration by age/sex ----
# asylum::irregular_migration |> 
#   group_by(Date, `Age Group`, Sex) |> 
#   summarise(`Number of detections` = sum(`Number of detections`, na.rm = TRUE)) |> 
#   write_csv("data-raw/flourish/2a - Safe routes/2a - Irregular migration - by age and sex.csv")

# ---- How many people have arrived in the UK in the last 12 months through family reunion pathways? ----
asylum::family_reunion |> 
  group_by(Date) |> 
  summarise(`Visas granted` = sum(`Visas granted`, na.rm = TRUE)) |> 
  write_csv("data-raw/flourish/2a - Safe routes/2a - Family reunion.csv")

# - Caption -
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
  slice(seq(2, n(), by = 4))

family_reunion_rolling_sum

# Check trend in rolling sum
ggplot(family_reunion_rolling_sum, aes(x = Date, y = RollSum)) +
  geom_line() +
  scale_y_continuous(limits = c(0, NA))

# ---- Arrivals through resettlement routes and other routes ----
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
  filter(Date >= max(Date) - dmonths(11)) |> 
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
  write_csv("data-raw/flourish/2a - Safe routes/arrival routes.csv")
