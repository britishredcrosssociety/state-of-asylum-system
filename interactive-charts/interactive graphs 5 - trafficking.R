library(tidyverse)
library(asylum)
library(zoo)

# ---- NRM referrals over time ----
nrm_referrals_longitudinal |> 
  mutate(Date = paste0(Year, " ", Quarter)) |> 
  relocate(Date) |> 
  pivot_wider(names_from = `Location of exploitation`, values_from = `NRM referrals`) |> 
  filter(!str_detect(`Age at exploitation`, "unknown")) |> 
  select(-Total, -Year, -Quarter) |> 
  write_csv("data-raw/flourish/5 - Trafficking/NRM referrals over time.csv")

# - CAPTION -
nrm_referrals_longitudinal |> 
  filter(`Location of exploitation` == "Total") |> 
  mutate(Date = yq(paste0(Year, " ", Quarter))) |> 
  filter(Date >= max(Date) - dmonths(11)) |>  # Filter last 12 months
  summarise(`Total NRM referrals` = sum(`NRM referrals`))

# ---- How many people have been referred into the NRM in the last 12 months by nationality, age and setting of referral? ----
nrm_referrals <- 
  bind_rows(
    asylum::nrm_referrals_2023_q2,
    asylum::nrm_referrals_2023_q1,
    asylum::nrm_referrals_2022_q4,
    asylum::nrm_referrals_2022_q3
  )

# ---- By first responder ----
nrm_referrals_first_responder <- 
  nrm_referrals |> 
  filter(`Exploitation type` == "Total" & `Age at exploitation` == "Total" & Gender == "Total" & Nationality != "Total") |> 
  filter(
    (`First responder type` == "Government agency" & `First responder` != "Government agency total") |
    (`First responder type` != "Government agency" & str_detect(`First responder`, "total"))
  ) |> 
  
  mutate(`First responder type` = if_else(`First responder type` == "Government agency", `First responder`, `First responder type`)) |> 
  mutate(`UK national?` = if_else(str_detect(Nationality, "UK"), "UK national", "Other nationality")) |> 
  
  group_by(`First responder type`, `UK national?`) |> 
  summarise(People = sum(People)) |> 
  
  pivot_wider(names_from = `UK national?`, values_from = People)

nrm_referrals_first_responder |> 
  write_csv("data-raw/flourish/5 - Trafficking/NRM referrals by first responder.csv")

# - CAPTION -
# Proportin of referrals over last 12 months that were from Home Office
nrm_referrals_first_responder |> 
  mutate(Total = `Other nationality` + `UK national`) |> 
  mutate(`First responder type` = if_else(str_detect(`First responder type`, "Home Office"), "Home Office", "Other")) |> 
  group_by(`First responder type`) |> 
  summarise(Total = sum(Total)) |> 
  ungroup() |> 
  mutate(Proportion = Total / sum(Total))

# ---- By nationality and first responder (top 40 only) ----
nrm_referrals |> 
  filter(Nationality != "Total") |> 
  #filter(str_detect(`First responder`, "total")) |> 
  
  group_by(Nationality, `First responder type`) |> 
  summarise(People = sum(People)) |> 
  ungroup() |> 
  
  pivot_wider(names_from = `First responder type`, values_from = People) |> 
  
  mutate(across(where(is.double), ~replace_na(.x, 0))) |> 
  
  rowwise() |> 
  mutate(Total = sum(c_across(`Government agency`:Police))) |> 
  ungroup() |> 
  arrange(desc(Total)) |> 
  select(-Total) |> 
  
  slice(1:40) |> 

  write_csv("data-raw/flourish/5 - Trafficking/NRM referrals by nationality.csv")

# ---- By age and sex ----
nrm_referrals |> 
  filter(`Age at exploitation` != "Total" & Nationality == "Total") |> 
  filter(Gender %in% c("Female", "Male")) |> 
  
  group_by(`Age at exploitation`, Gender) |> 
  summarise(People = sum(People)) |> 
  
  filter(!str_detect(`Age at exploitation`, "unknown")) |> 
  
  pivot_wider(names_from = Gender, values_from = People) |> 
  mutate(Female = Female * -1) |> 
  
  write_csv("data-raw/flourish/5 - Trafficking/NRM referrals by age and sex.csv")

# ---- How many people received a positive reasonable grounds decision ----
nrm_reasonable_grounds |> 
  filter(Quarter != "Total") |> 
  
  mutate(Date = paste(Year, Quarter, sep = " ")) |> 
  mutate(Date = as.Date(as.yearqtr(Date, format = "%Y Q%q"), frac = 1)) |>
  
  relocate(Date) |> 
  select(-Year, -Quarter) |> 
  
  # Turn each separate "Age - Decision" column into a single column, and remove Totals
  pivot_longer(cols = -Date) |> 
  filter(name != "Total") |> 
  
  # Split Age and Decision into separate columns, and remove totals
  separate_wider_delim(name, delim = " - ", names = c("Age", "Decision")) |> 
  filter(Decision != "Total") |> 
  
  # Turn Age back into separate columns
  pivot_wider(names_from = Decision, values_from = value) |> 
  
  filter(!str_detect(Age, "unknown")) |> 
  
  # Change order of columns
  select(Date, Age, `Positive reasonable grounds`, `Negative reasonable grounds`) |> 
  
  write_csv("data-raw/flourish/5 - Trafficking/reasonable grounds by age.csv")

# ---- How many people received positive conclusive grounds decision by age, gender, nationality and competent authority? ----
nrm_conclusive_grounds |> 
  filter(Quarter != "Total") |> 
  
  mutate(Date = paste(Year, Quarter, sep = " ")) |> 
  mutate(Date = as.Date(as.yearqtr(Date, format = "%Y Q%q"), frac = 1)) |>
  
  relocate(Date) |> 
  select(-Year, -Quarter) |> 
  
  # Turn each separate "Age - Decision" column into a single column, and remove Totals
  pivot_longer(cols = -Date) |> 
  filter(name != "Total") |> 
  
  # Split Age and Decision into separate columns, and remove totals
  separate_wider_delim(name, delim = " - ", names = c("Age", "Decision")) |> 
  filter(Decision != "Total") |> 
  
  # Turn Age back into separate columns
  pivot_wider(names_from = Decision, values_from = value) |> 
  
  filter(!str_detect(Age, "unknown")) |> 
  
  # Change order of columns
  select(Date, Age, `Positive conclusive grounds`, `Negative conclusive grounds`) |> 
  
  write_csv("data-raw/flourish/5 - Trafficking/conclusive grounds by age.csv")

# ---- Referrals via Duty to Notify over time ----
nrm_duty_to_notify_longitudinal |> 
  filter(Quarter != "Total") |> 
  # mutate(Date = yq(paste(Year, Quarter, sep = "-"))) |> 
  mutate(Date = zoo::as.Date(as.yearqtr(paste(Year, Quarter), format = "%Y Q%q"), frac = 1)) |>
  select(Date, `Referrals through Duty to Notify process` = Total) |> 
  write_csv("data-raw/flourish/5 - Trafficking/duty to notify - longitudinal.csv")

# ---- How many people were the Home Office notified through the Duty to Notify (DtN) process by nationality? ----
dtn <- 
  bind_rows(
  nrm_duty_to_notify_2022_q3,
  nrm_duty_to_notify_2022_q4,
  nrm_duty_to_notify_2023_q1,
  nrm_duty_to_notify_2023_q2
) |> 
  group_by(Nationality) |> 
  summarise(Total = sum(Total)) |> 
  ungroup() |> 
  arrange(desc(Total)) |> 
  filter(Nationality != "Total")

dtn |> 
  slice(1:20) |> 
  write_csv("data-raw/flourish/5 - Trafficking/duty to notify - nationality.csv")

# - CAPTION -
# What proportion of DtNs are from the top four nationalities?
scales::percent(
  dtn |> slice(1:4) |> summarise(Total = sum(Total)) |> pull(Total) / dtn |> summarise(Total = sum(Total)) |> pull(Total)
)

# Which are the top four nationalities?
dtn |> slice(1:4)
