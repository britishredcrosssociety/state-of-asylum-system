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
  ungroup() |> 
  mutate(Nationality = "Total")

backlog_nationality <- 
  asylum::awaiting_decision |> 
  mutate(Stage = case_when(
    Duration == "More than 6 months" ~ "Pending initial decision (more than 6 months)",
    Duration == "6 months or less" ~ "Pending initial decision (6 months or less)",
    Duration == "N/A - Further review" ~ "Pending further review"
  )) |> 
  group_by(Date, Stage, Nationality) |> 
  summarise(Backlog = sum(Applications)) |> 
  ungroup() |> 
  arrange(Nationality)

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

# ---- What is the number of people waiting for an initial decision by nationality ----
awaiting_decision_by_nationality <- 
  asylum::awaiting_decision |> 
  filter(Date == max(Date)) |>
  group_by(Nationality, Duration) |> 
  summarise(Applications = sum(Applications)) |> 
  ungroup() |> 
  arrange(desc(Applications)) |> 
  pivot_wider(names_from = Duration, values_from = Applications) |> 
  mutate(
    `6 months or less` = replace_na(`6 months or less`, 0),
    `More than 6 months` = replace_na(`More than 6 months`, 0),
  ) |> 
  
  mutate(Total = `6 months or less` + `More than 6 months`) |> 
  slice_max(Total, n = 20) |> 
  select(-Total)
  
awaiting_decision_by_nationality |> 
  write_csv("data-raw/flourish/3a - Initial decisions and productivity/waiting - by nationality.csv")

# - Caption -
# Which nationalities have a grant rate > 80%?
high_grant_nationalities <- 
  asylum::grant_rates_initial_annual |> 
  filter(Year == max(Year)) |> 
  filter(`Initial grant rate` > 0.8) |> 
  pull(Nationality)

# How many and what proportion of people from high-grant-rate nationalities are waiting for a decision?
asylum::awaiting_decision |> 
  filter(Date == max(Date)) |> 
  mutate(high_grant = if_else(Nationality %in% high_grant_nationalities, "High", "Low")) |> 
  group_by(high_grant) |> 
  summarise(Applications = sum(Applications)) |> 
  ungroup() |> 
  mutate(Proportion = Applications / sum(Applications))

# # Calculate total waiting as of most recent quarter
# asylum::awaiting_decision |> 
#   filter(Date == max(Date)) |> 
#   summarise(sum(Applications))
# 
# # Proportion waiting - use the `Proportion_waiting_cumulative` column to judge which nationalities to include in the caption
# awaiting_decision_by_nationality |> 
#   ungroup() |> 
#   mutate(
#     Proportion_waiting = (`More than 6 months` + `6 months or less`) / (sum(`More than 6 months`) + sum(`6 months or less`)),
#     Proportion_more_than_6_months = `More than 6 months` / sum(`More than 6 months`)
#   ) |> 
#   mutate(
#     Proportion_waiting_cumulative = cumsum(Proportion_waiting)
#   )

# ---- Grants, refusals, and withdrawn claims over time ----
asylum::decisions_resettlement |> 
  filter(`Case type` == "Asylum Case") |> 
  mutate(`Case outcome group` = if_else(str_detect(`Case outcome group`, "Grant"), "Granted", `Case outcome group`)) |> 
  
  group_by(Date, `Case outcome group`) |> 
  summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
  
  pivot_wider(names_from = `Case outcome group`, values_from = Decisions) |> 
  
  write_csv("data-raw/flourish/3a - Initial decisions and productivity/granted, refused, withdrawn.csv")

# - Caption -
library(zoo)

# Proportion of claims withdrawn over last 12 months
asylum::decisions_resettlement |> 
  filter(Date >= max(Date) - dmonths(11)) |>  # Filter applications within the last 12 months
  
  filter(`Case type` == "Asylum Case") |> 
  mutate(`Case outcome group` = if_else(str_detect(`Case outcome group`, "Grant"), "Granted", `Case outcome group`)) |> 
  
  group_by(`Case outcome group`) |> 
  summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
  ungroup() |> 
  
  mutate(Proportion = Decisions / sum(Decisions))

# Rolling sums of withdrawals - to check highest on record
asylum::decisions_resettlement |> 
  # filter(Date >= max(Date) - dmonths(11)) |>  # Filter applications within the last 12 months
  
  filter(`Case type` == "Asylum Case") |> 
  mutate(`Case outcome group` = if_else(str_detect(`Case outcome group`, "Grant"), "Granted", `Case outcome group`)) |> 
  
  group_by(Date, `Case outcome group`) |> 
  summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
  ungroup() |> 
  
  pivot_wider(names_from = `Case outcome group`, values_from = Decisions) |> 
  
  arrange(desc(Date)) |> 
  
  # Calculate the annual rolling sum, starting with the most recent quarter
  mutate(
    Granted = rollsum(Granted, k = 4, na.pad = TRUE),
    Refused = rollsum(Refused, k = 4, na.pad = TRUE),
    Withdrawn = rollsum(Withdrawn, k = 4, na.pad = TRUE)
  ) |> 
  
  # Keep the 2nd row (rolling sum for the past 12 months) and every 4th row after that
  slice(seq(2, n(), by = 4)) |> 
  arrange(desc(Withdrawn))

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
