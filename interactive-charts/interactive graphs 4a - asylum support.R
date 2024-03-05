library(tidyverse)
library(asylum)

# ---- Flourish- Section 4, Slide 3: Asylum backlog ----
backlog <- 
  asylum::awaiting_decision |> 
  mutate(Stage = case_when(
    Duration == "More than 6 months" ~ "Pending initial decision (more than 6 months)",
    Duration == "6 months or less" ~ "Pending initial decision (6 months or less)",
    Duration == "N/A - Further review" ~ "Pending further review"
  )) |> 
  group_by(Date, Stage) |> 
  summarise(Backlog = sum(Applications))

backlog |> 
  pivot_wider(names_from = Stage, values_from = Backlog) |> 
  select(Date, `Pending initial decision (more than 6 months)`, `Pending initial decision (6 months or less)`, `Pending further review`) |> 
  write_csv("data-raw/flourish/4a - Asylum support/backlog trend Dec 23.csv")

# - CAPTION -
backlog |> 
  ungroup() |>
  filter(Date == max(Date)) |> 
  summarise(Backlog = sum(Backlog))

# ---- Flourish- Section 4, Slide 4: Asylum support payments ----
asylum::support_received |> 
  group_by(Date, `Support Type`) |> 
  summarise(People = sum(People)) |> 
  pivot_wider(names_from = `Support Type`, values_from = People) |> 
  write_csv("data-raw/flourish/4a - Asylum support/asylum support - longitudinal Dec 23.csv")

# ---- Per cent of people awaiting a decision receiving asylum support ----
# Total receiving destitution support
receiving_support <- 
  asylum::support_received |> 
  filter(Date == max(Date)) |> 
  filter(`Support Type` != "Section 4") |> 
  mutate(`Accommodation Type` = if_else(`Accommodation Type` == "Subsistence Only", "Receiving asylum support (subsistence only)", "Receiving asylum support (accommodation)")) |> 
  group_by(`Accommodation Type`) |> 
  summarise(People = sum(People))

# Total waiting for a decision
backlog <- 
  asylum::awaiting_decision |> 
  filter(Date == max(Date)) |> 
  summarise(Backlog = sum(Applications)) |> 
  pull(Backlog)

receiving_support |> 
  rename(Category = `Accommodation Type`) |> 
  add_row(
    Category = "Awaiting decision but not receiving asylum support", 
    People = backlog - sum(receiving_support$People)
  ) |> 
  write_csv("data-raw/flourish/4a - Asylum support/destitution rate Dec 23.csv")

# - CHART TITLE AND CAPTION -
# What percentage are receiving asylum support?
scales::percent(sum(receiving_support$People) / backlog)

# ---- Flourish- Section 4, Slide 5: Asylum support payments ----
support_received_recently <- 
  asylum::support_received |> 
  filter(Date == max(Date)) |> 
  
  group_by(`Support Type`, `Accommodation Type`) |> 
  summarise(People = sum(People)) |> 
  ungroup() |> 
  arrange(`Support Type`, desc(People))

# Add definitions for each support type
support_received_recently |> 
  mutate(Definition = case_match(
    `Support Type`,
    "Section 4" ~ "Support provided to people whose asylum claim was unsuccessful and they have no more right to appeal.",
    "Section 95" ~ "Support provided to people seeking asylum to prevent destitution while their asylum application is considered.",
    "Section 98" ~ "Emergency support for people seeking asylum while an application for section 95 support is being considered."
  )) |> 
  write_csv("data-raw/flourish/4a - Asylum support/asylum support - most recent Dec 23.csv")

# - CAPTION -
# What proportion of people receiving asylum support are in hotels?
support_received_recently |> 
  # filter(`Support Type` == "Section 98") |> 
  filter(!str_detect(tolower(`Accommodation Type`), "subsistence")) |> 
  mutate(Hotels = if_else(str_detect(tolower(`Accommodation Type`), "hotel"), "Hotel", "Other accommodation")) |> 
  
  group_by(Hotels) |> 
  summarise(People = sum(People)) |> 
  ungroup() |> 
  
  mutate(Proportion = People / sum(People))
