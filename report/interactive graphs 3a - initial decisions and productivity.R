library(tidyverse)
library(asylum)

# ---- Backlog over time, by nationality ----
backlog_total <- 
  asylum::awaiting_decision |> 
  mutate(Stage = case_when(
    Duration == "More than 6 months" ~ "Pending initial decision (more than 6 months)",
    Duration == "6 months or less" ~ "Pending initial decision (6 months or less)",
    Duration == "N/A - Further review" ~ "Pending further review"
  )) |> 
  group_by(Date, Stage) |> 
  summarise(Backlog = sum(Applications)) |> 
  mutate(Nationality = "Total")

backlog_nationality <- 
  asylum::awaiting_decision |> 
  mutate(Stage = case_when(
    Duration == "More than 6 months" ~ "Pending initial decision (more than 6 months)",
    Duration == "6 months or less" ~ "Pending initial decision (6 months or less)",
    Duration == "N/A - Further review" ~ "Pending further review"
  )) |> 
  group_by(Date, Stage, Nationality) |> 
  summarise(Backlog = sum(Applications))

bind_rows(backlog_total, backlog_nationality) |> 
  pivot_wider(names_from = Stage, values_from = Backlog) |> 
  # Turns NAs into blank spaces
  mutate(across(`Pending further review`:`Pending initial decision (more than 6 months)`, as.character)) |> 
  replace_na(list(`Pending further review` = "",
                  `Pending initial decision (6 months or less)` = "",
                  `Pending initial decision (more than 6 months)` = "")) |> 
  select(Date, Nationality, `Pending initial decision (more than 6 months)`, `Pending initial decision (6 months or less)`, `Pending further review`) |> 
  write_csv("data-raw/flourish/3a - Initial decisions and productivity/backlog trends - by nationality.csv")

# - Caption -
# Proportion of people currently waiting for a decision who have been waiting more than three months
backlog_total |> 
  ungroup() |>
  filter(Date == max(Date)) |> 
  mutate(Proportion = Backlog / sum(Backlog))

backlog_total |> 
  ungroup() |>
  filter(Date == max(Date)) |> 
  summarise(Total = sum(Backlog))

# ---- What is the number of people waiting for an initial decision on their asylum claim (and what is their nationality, age, gender)? ----
# - Nationality -
awaiting_decision_by_nationality <- 
  asylum::awaiting_decision |> 
  filter(Date == max(Date)) |>
  group_by(Nationality, Duration) |> 
  summarise(Applications = sum(Applications)) |> 
  arrange(desc(Applications)) |> 
  pivot_wider(names_from = Duration, values_from = Applications) |> 
  mutate(
    `6 months or less` = replace_na(`6 months or less`, 0),
    `More than 6 months` = replace_na(`More than 6 months`, 0),
  )

awaiting_decision_by_nationality |> 
  write_csv("data-raw/flourish/3a - Initial decisions and productivity/waiting - by nationality.csv")

# - Caption -
# Calculate total waiting as of most recent quarter
asylum::awaiting_decision |> 
  filter(Date == max(Date)) |> 
  summarise(sum(Applications))

awaiting_decision_by_nationality |> 
  ungroup() |> 
  mutate(
    Proportion_waiting = (`More than 6 months` + `6 months or less`) / (sum(`More than 6 months`) + sum(`6 months or less`)),
    Proportion_more_than_6_months = `More than 6 months` / sum(`More than 6 months`)
  ) |> 
  mutate(
    Proportion_waiting_cumulative = cumsum(Proportion_waiting)
  )

# - Data by age and sex doesn't exist -

# ---- What is the impact of the streamlined asylum process (SAP) and what has the impact of this policy been on the backlog? ----
# Not sure we can answer this from Home Office data...

# ---- What is the productivity of the Home Office asylum decision making (using Home Office quarterly transparency data)? ----
asylum::asylum_costs_and_productivity |> 
  select(`Financial Year`, Productivity) |> 
  drop_na() |> 
  write_csv("data-raw/flourish/3a - Initial decisions and productivity/Home Office productivity.csv")

# - Caption -
# Plot asylum caseworking staff and principal stages completed side by side to check trends
asylum::asylum_costs_and_productivity |> 
  select(`Financial Year`, `Asylum Caseworking Staff`, `Average Principal Stages Completed Per Month`, Productivity) |> 
  na.omit() |> 
  
  # Check that productivity is calculated this way:
  mutate(Productivity2 = `Average Principal Stages Completed Per Month` / `Asylum Caseworking Staff`) |> 
  #--> It is.
  
  # Plot asylum caseworkers and principal stages completed side by side
  select(`Financial Year`, `Asylum Caseworking Staff`, `Average Principal Stages Completed Per Month`) |> 
  pivot_longer(cols = -`Financial Year`) |> 
  
  ggplot(aes(x = `Financial Year`, y = value, group = name)) +
  geom_line() +
  facet_wrap(~name, scales = "free") +
  labs(
    title = "More asylum caseworking staff are completing fewer principal stages per month, on average"
  )

asylum::asylum_costs_and_productivity |> 
  select(`Financial Year`, `Asylum Caseworking Staff`, `Average Principal Stages Completed Per Month`) |> 
  na.omit() |> 
  
  # Calculate growth rates
  mutate(
    `Asylum caseworking staff (% change year on year)` = (`Asylum Caseworking Staff` - lag(`Asylum Caseworking Staff`)) / lag(`Asylum Caseworking Staff`),
    `Average Principal Stages Completed Per Month (% change year on year)` = (`Average Principal Stages Completed Per Month` - lag(`Average Principal Stages Completed Per Month`)) / lag(`Average Principal Stages Completed Per Month`)
  )

# Calculate % changes in caseworking staff and principal stages completed since 2015
asylum::asylum_costs_and_productivity |> 
  select(`Financial Year`, `Asylum Caseworking Staff`, `Average Principal Stages Completed Per Month`) |> 
  filter(`Financial Year` == max(`Financial Year`) | `Financial Year` == "2015/16") |> 
  
  # Calculate growth rates
  mutate(
    `Asylum caseworking staff (% change year on year)` = (`Asylum Caseworking Staff` - lag(`Asylum Caseworking Staff`)) / lag(`Asylum Caseworking Staff`),
    `Average Principal Stages Completed Per Month (% change year on year)` = (`Average Principal Stages Completed Per Month` - lag(`Average Principal Stages Completed Per Month`)) / lag(`Average Principal Stages Completed Per Month`)
  )


# ---- What is the current backlog for decisions on family reunion cases? ----
# Not sure this data exists...
# ... thought possibly in VSI_03 worksheet in https://www.gov.uk/government/publications/visas-and-citizenship-data-q1-2023

# ---- How long are families currently waiting to be reunited? ----
# Not sure this data exists
