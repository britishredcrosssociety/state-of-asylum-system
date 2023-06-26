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

# ---- How many people received positive conclusive grounds decision by age, gender, nationality and competent authority? ----

# ---- How many people were the HomeOffice notified through the Duty to Notify (DtN) process by nationality? ----

# ---- How many people were detained for removal having consented to enter the NRM? ----
