library(tidyverse)
library(asylum)
source("report/brc_colours.R")
source("report/theme_brc.R")

# ---- QUESTION: What is the scale of human trafficking in the UK and what is the provision of support to survivors? ----

# ---- Referral to NRM: ----
nrm_referrals <- 
  bind_rows(
    asylum::nrm_referrals_2023_q2,
    asylum::nrm_referrals_2023_q1,
    asylum::nrm_referrals_2022_q4,
    asylum::nrm_referrals_2022_q3
  )

view(nrm_referrals |>
  group_by(Nationality) |>
  summarise(Total = sum(People)))

NRMTotal <- nrm_referrals |>
  group_by(`Age at exploitation`, Gender, Nationality) |>
  summarise(Total = sum(People))

NRMTotal <- NRMTotal |>
  filter(`Age at exploitation` != "Total") |>
  filter(Gender != "Total") |>
  filter(Nationality != "Total")

# "Total" was removed from each category as it was grouped for other analyses.

# ---- 1. NRM by Top Nationalities ---- 
NRMTotal |>
  group_by(Nationality) |>
  summarise(TotalNat = sum(Total)) |> view()

# Total number of people = 13763

# Total M Under 18 = 4841/13763

# 70% of those referred to NRM is from Albania, the UK, Eritrea, Sudan, Vietnam and Iran. 


NRMTotal |>
  group_by(Nationality) |>
  summarise(TotalN = sum(Total)) |>
  slice_max(TotalN, n = 10) |>
  ggplot(aes(x = reorder(Nationality, desc(TotalN)), y = TotalN)) +
  geom_col(fill = brc_colours$red_dunant) +
  geom_text(aes(label = scales::comma(TotalN)), show.legend = FALSE, size = rel(3),  position = position_dodge(width=1), vjust=-0.25, colour = brc_colours$black_shadow) +
  theme_brc() +
  labs(title = str_wrap("Nationalities with the largest number of referrals to the National Referral Mechanism (NRM) processed for year ending June 2023"),
       subtitle =  "People from Albania, the United Kingdom, Eritrea, Sudan, Vietnam and Iran made up 70% of all NRM referrals",
       x = "Nationality", 
       y = 'Number of people', 
       caption = 'British Red Cross analysis of Home Office data, June 2022 to June 2023') +
  scale_y_continuous(labels = scales::comma, limits = c(0, 5000), expand = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5))

# ---- 2. NRM referral by Sex and Age ----
NRMTotal %>%
  filter(Gender != "Not Recorded") %>%
  filter(Gender != "Other") %>%
  filter(`Age at exploitation` != "Not specified or unknown") %>%
  group_by(`Age at exploitation`, Gender) %>%
  summarise(TotalSum = sum(Total)) %>%
  ggplot(aes(fill = `Age at exploitation`, x =  Gender,y =  TotalSum)) +
  geom_bar(position = "stack", stat ="identity") +
  geom_text(aes(label = scales::comma(TotalSum)), position = position_stack(vjust = .5),size = rel(3)) +
  theme_brc() +
  labs(title = strwrap("Number of referrals to the National Referral Mechanism (NRM) by age and sex for year ending June 2023"), 
       x = "Sex", 
       y = "Number of Referrals",
       caption = "British Red Cross analysis of Home Office data, June 2022 to June 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 12000), expand = c(0, NA)) +
  scale_fill_manual(values = c(brc_colours$red_light,
                               brc_colours$red_dunant))
 
# ---- 3. NRM Duty to Notify ----
view(nrm_duty_to_notify_2022_q2)

nrm_duty_to_notify <- bind_rows(
  asylum::nrm_duty_to_notify_2023_q2,
  asylum::nrm_duty_to_notify_2023_q1,
  asylum::nrm_duty_to_notify_2022_q4,
  asylum::nrm_duty_to_notify_2022_q3
)

DTNTotal <- nrm_duty_to_notify |>
  group_by(Nationality) Z>
  filter(Nationality != "Total")

DTNTotal <- DTNTotal |>
  group_by(Nationality) |>
  summarise(Total2 = sum(Total))

DTNTotal |>
  slice_max(Total2, n = 10) |>
  ggplot(aes(x = reorder(Nationality, desc(Total2)), y = Total2)) +
  geom_col(fill = brc_colours$red_dunant, colour = brc_colours$red_dunant) +
  geom_text(aes(label = scales::comma(Total2)), show.legend = FALSE, size = rel(3),  position = position_dodge(width=1), vjust=-0.25, colour = brc_colours$black_shadow) +
  theme_brc() +
  labs(title = str_wrap("Nationalities with the largest number of reports via the Duty to Notify process for year ending June 2023"),
       # subtitle = "406 individuals from the United Kingdom were referred to Duty to Notify from April 2022 to January 2023",
       x = "Nationalities", 
       y = "Number of reports", 
       caption = "British Red Cross analysis of Home Office data, June 2022 to June 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 1000), expand = c(0,NA))

# ---- 4. NRM Reasonable Grounds ---- 
view(nrm_reasonable_grounds)

nrm_reasonable_grounds <- nrm_reasonable_grounds |>
  filter(Quarter == "Total") |> 
  pivot_longer(cols = `Adult (18 or over) - Negative reasonable grounds`:`Age not specified or unknown - Total`, names_to = "Age group", values_to = "People") 

nrm_reasonable_grounds <- nrm_reasonable_grounds |>
  select(Year, `Age group`,People)

nrm_reasonable_grounds <- nrm_reasonable_grounds |>
  filter(`Age group` != "Adult (18 or over) - Total", 
         `Age group` != "Child (17 or under) - Total",
         `Age group` != "Age not specified or unknown - Total",
         `Age group` != "Age not specified or unknown - Negative reasonable grounds",
         `Age group` != "Age not specified or unknown - Positive reasonable grounds")

nrm_reasonable_grounds |> 
  mutate(`Age group` = 
           case_match(
             `Age group`,
             c("Adult (18 or over) - Negative reasonable grounds" ,
                "Child (17 or under) - Negative reasonable grounds") ~ "Negative reasonable grounds",
             c("Adult (18 or over) - Positive reasonable grounds" ,
              "Child (17 or under) - Positive reasonable grounds") ~ "Positive reasonable grounds")) |>
  ggplot(aes(x = Year, y = People, fill = `Age group`)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_brc() +
  labs(title = "Proportion of people who received positive reasonable grounds by age from 2014 to 2023", 
       x = "Year", 
       y = "Number of people",
       fill = "Reasonable grounds decision",
       caption = "British Red Cross analysis of Home Office data, March 2014 to June 2023") +
  scale_x_continuous(breaks = c(2014:2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 20000), expand = c(0,NA)) +
  scale_fill_manual(values = c(brc_colours$red_light,
                               brc_colours$red_mercer))


# ---- 5. Positive Conclusive Grounds ---- 
nrm_conclusive_grounds <- nrm_conclusive_grounds |>
  filter(Quarter == "Total") |> 
  pivot_longer(cols = `Adult (18 or over) - Negative conclusive grounds`:`Age not specified or unknown - Total`, names_to = "Age group", values_to = "People") 

nrm_conclusive_grounds <- nrm_conclusive_grounds |>
  select(Year, `Age group`,People)

nrm_conclusive_grounds |>
  filter(`Age group` != "Adult (18 or over) - Total", 
         `Age group` != "Child (17 or under) - Total",
         `Age group` != "Age not specified or unknown - Total",
         `Age group` != "Age not specified or unknown - Negative conclusive grounds",
         `Age group` != "Age not specified or unknown - Positive conclusive grounds") |>
  mutate(`Age group` = 
           case_match(
             `Age group`,
             c("Adult (18 or over) - Negative conclusive grounds" ,
               "Child (17 or under) - Negative conclusive grounds") ~ "Negative conclusive grounds",
             c("Adult (18 or over) - Positive conclusive grounds" ,
               "Child (17 or under) - Positive conclusive grounds") ~ "Positive conclusive grounds")) |>
  ggplot(aes(x = Year, y = People, fill = `Age group`)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_brc() +
  labs(title = "Proportion of people who received positive conclusive grounds by age from 2014 to 2023", 
       x = "Year", 
       y = "Number of people",
       caption = "British Red Cross analysis of Home Office data, March 2014 to June 2023") +
  scale_x_continuous(breaks = c(2014:2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 7000), expand = c(0,NA)) +
  scale_fill_manual(values = c(brc_colours$red_light,
                               brc_colours$red_mercer))
