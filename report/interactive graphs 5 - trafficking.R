library(tidyverse)
library(asylum)

# ---- How many people have been referred into the NRM in the last 12 months by nationality, age and setting of referral? ----
nrm_referrals <- 
  bind_rows(
  asylum::nrm_referrals_2023_q1,
  asylum::nrm_referrals_2022_q4,
  asylum::nrm_referrals_2022_q3,
  asylum::nrm_referrals_2022_q2
)

# By first responder
nrm_referrals |> 
  filter(`Exploitation type` == "Total" & `Age at exploitation` == "Total" & Gender == "Total" & Nationality == "Total") |> 
  filter(
    (`First responder type` == "Government agency" & `First responder` != "Government agency total") |
    (`First responder type` != "Government agency" & str_detect(`First responder`, "total"))
  ) |> 
  
  mutate(`First responder type` = if_else(`First responder type` == "Government agency", `First responder`, `First responder type`)) |> 
  
  group_by(`First responder type`) |> 
  summarise(People = sum(People)) |> 
  
  write_csv("data-raw/flourish/5 - Trafficking/NRM referrals by first responder.csv")

# By age and sex
nrm_referrals |> 
  filter(`Age at exploitation` != "Total" & Nationality == "Total") |> 
  filter(Gender != "Total") |> 
  
  group_by(`Age at exploitation`, Gender) |> 
  summarise(People = sum(People)) |> 
  
  pivot_wider(names_from = Gender, values_from = People) |> 
  
  write_csv("data-raw/flourish/5 - Trafficking/NRM referrals by age and gender.csv")

# By nationality
nrm_referrals |> 
  filter(Nationality != "Total") |> 
  filter(str_detect(`First responder`, "total")) |> 
  
  group_by(Nationality) |> 
  summarise(People = sum(People)) |> 
  ungroup() |> 
  
  arrange(desc(People)) |> 
  
  write_csv("data-raw/flourish/5 - Trafficking/NRM referrals by nationality.csv")

# ---- How many people received a positive reasonable grounds decision by age, gender, nationality and setting of referral? ----
nrm_reasonable_grounds |> 
  filter(Quarter == "Total") |> 
  select(-Quarter) |> 
  
  # Turn each separate "Age - Decision" column into a single column, and remove Totals
  pivot_longer(cols = -Year) |> 
  filter(name != "Total") |> 
  
  # Split Age and Decision into separate columns, and remove totals
  separate_wider_delim(name, delim = " - ", names = c("Age", "Decision")) |> 
  filter(Decision != "Total") |> 
  
  # Turn Age back into separate columns
  pivot_wider(names_from = Decision, values_from = value) |> 
  
  write_csv("data-raw/flourish/5 - Trafficking/reasonable grounds by age.csv")

# ---- How many people received positive conclusive grounds decision by age, gender, nationality and competent authority? ----
nrm_conclusive_grounds |> 
  filter(Quarter == "Total") |> 
  select(-Quarter) |> 
  
  # Turn each separate "Age - Decision" column into a single column, and remove Totals
  pivot_longer(cols = -Year) |> 
  filter(name != "Total") |> 
  
  # Split Age and Decision into separate columns, and remove totals
  separate_wider_delim(name, delim = " - ", names = c("Age", "Decision")) |> 
  filter(Decision != "Total") |> 
  
  # Turn Age back into separate columns
  pivot_wider(names_from = Decision, values_from = value) |> 
  
  write_csv("data-raw/flourish/5 - Trafficking/conclusive grounds by age.csv")

# ---- How many people were the Home Office notified through the Duty to Notify (DtN) process by nationality? ----
dtn <- 
  bind_rows(
  nrm_duty_to_notify_2022_q2,
  nrm_duty_to_notify_2022_q3,
  nrm_duty_to_notify_2022_q4,
  nrm_duty_to_notify_2023_q1
) |> 
  group_by(Nationality) |> 
  summarise(Total = sum(Total)) |> 
  arrange(desc(Total)) |> 
  filter(Nationality != "Total")

dtn |> 
  write_csv("data-raw/flourish/5 - Trafficking/duty to notify.csv")

# What proportion of DtNs are from the top five countries?
scales::percent(
  dtn |> slice(1:4) |> summarise(Total = sum(Total)) |> pull(Total) / dtn |> summarise(Total = sum(Total)) |> pull(Total)
)

# ---- How many people were detained for removal having consented to enter the NRM? ----
