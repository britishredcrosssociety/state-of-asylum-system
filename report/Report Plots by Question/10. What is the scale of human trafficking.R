library(tidyverse)
library(asylum)
source("report/brc_colours.R")
source("report/theme_brc.R")

# ---- QUESTION: What is the scale of human trafficking in the UK and what is the provision of support to survivors? ----

# ---- Referral to NRM: ----
nrm_referrals <- 
  bind_rows(
    asylum::nrm_referrals_2023_q1,
    asylum::nrm_referrals_2022_q4,
    asylum::nrm_referrals_2022_q3,
    asylum::nrm_referrals_2022_q2
  )

view(nrm_referrals %>%
  group_by(Nationality) %>%
  summarise(Total = sum(People)))

NRMTotal <- nrm_referrals %>%
  group_by(`Age at exploitation`, Gender, Nationality) %>%
  summarise(Total = sum(People))

NRMTotal <- NRMTotal %>%
  filter(`Age at exploitation` != "Total") %>%
  filter(Gender != "Total") %>%
  filter(Nationality != "Total")

# "Total" was removed from each category as it was grouped for other analyses.

# ---- NRM referral by Sex and Age ----
NRMTotal %>%
  filter(Gender != "Not Recorded") %>%
  filter(Gender != "Other") %>%
  filter(`Age at exploitation` != "Not specified or unknown") %>%
  group_by(`Age at exploitation`, Gender) %>%
  summarise(TotalSum = sum(Total)) %>%
  ggplot(aes(fill = `Age at exploitation`, x =  Gender,y =  TotalSum)) +
  geom_bar(position = "stack", stat ="identity") +
  geom_text(aes(label = TotalSum), position = position_stack(vjust = .5),size = rel(2)) +
  theme_brc() +
  labs(title = "Number of referrals to the National Referral Mechanism (NRM) by age and sex year ending March 2023", 
       x = "Sex", 
       y = "Number of Referrals",
       caption = "British Red Cross analysis of Home Office data, March 2022 to March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 12000), expand = c(0, NA)) +
  scale_fill_manual(values = c(brc_colours$red_dunant,
                               brc_colours$red_earth))
 
# ---- NRM by Top Nationalities ---- 
NRMTotal %>%
  filter(Nationality == "UK") %>%
  group_by(Nationality) %>%
  summarise(UK = sum(Total))

# 1775 male children/ 2469 were referred to NRM- mostly male children from UK.

NRMTotal |>
  group_by(Nationality) %>%
  summarise(TotalN = sum(Total)) %>%
  filter(TotalN > 300) %>%
  ggplot(aes(x = reorder(Nationality, desc(TotalN)), y = TotalN)) +
  geom_col(fill = brc_colours$red_dunant) +
  geom_text(aes(label = scales::comma(TotalN)), show.legend = FALSE, size = rel(3),  position = position_dodge(width=1), vjust=-0.25, colour = brc_colours$black_shadow) +
  theme_brc() +
  labs(title = "Nationalities with the largest number of referrals to the National Referral Mechanism (NRM) processed year ending March 2023",
       subtitle =  "People from the United Kingdom, Albania, Eritrea, Iran, Sudan and Vietnam made up 70% of all NRM referrals",
       x = "Nationality", 
       y = 'Number of People', 
       caption = 'British Red Cross analysis of Home Office data, March 2022 to March 2023') +
  scale_y_continuous(labels = scales::comma, limits = c(0, 5000), expand = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5))


# ---- Positive Conclusive Grounds ---- 
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
  ggplot(aes(x = Year, y = People, fill = `Age group`)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_brc() +
  labs(title = "Proportion of people who recieved positive conclusive grounds by age from 2014 to 2023", 
       x = "Year", 
       y = "Number of People",
       caption = "British Red Cross analysis of Home Office data, March 2014 until March 2023") +
  scale_x_continuous(breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 6000), expand = c(0,NA)) +
  scale_fill_manual(values = c(brc_colours$red_light,
                               brc_colours$red_dunant,
                               brc_colours$steel,
                               brc_colours$teal))

# ---- NRM Reasonable Grounds ---- 
view(nrm_reasonable_grounds)

nrm_reasonable_grounds <- nrm_reasonable_grounds |>
  filter(Quarter == "Total") |> 
  pivot_longer(cols = `Adult (18 or over) - Negative reasonable grounds`:`Age not specified or unknown - Total`, names_to = "Age group", values_to = "People") 

nrm_reasonable_grounds <- nrm_reasonable_grounds |>
  select(Year, `Age group`,People)

nrm_reasonable_grounds |>
  filter(`Age group` != "Adult (18 or over) - Total", 
         `Age group` != "Child (17 or under) - Total",
         `Age group` != "Age not specified or unknown - Total",
         `Age group` != "Age not specified or unknown - Negative reasonable grounds",
         `Age group` != "Age not specified or unknown - Positive reasonable grounds") |>
  ggplot(aes(x = Year, y = People, fill = `Age group`)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_brc() +
  labs(title = "Proportion of people who recieved positive reasonable grounds by age from 2014 to 2023", 
       x = "Year", 
       y = "Number of People",
       caption = "British Red Cross analysis of Home Office data, January 2014 until January 2023") +
  scale_x_continuous(breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 20000), expand = c(0,NA)) +
  scale_fill_manual(values = c(brc_colours$red_light,
                               brc_colours$red_dunant,
                               brc_colours$steel,
                               brc_colours$teal))

# ---- # NRM Duty to Notify ----
view(nrm_duty_to_notify_2022_q2)

nrm_duty_to_notify <- bind_rows(
  asylum::nrm_duty_to_notify_2023_q1,
  asylum::nrm_duty_to_notify_2022_q4,
  asylum::nrm_duty_to_notify_2022_q3,
  asylum::nrm_duty_to_notify_2022_q2
)

DTNTotal <- nrm_duty_to_notify %>%
  group_by(Nationality) %>%
  filter(Nationality != "Total")

DTNTotal <- DTNTotal %>%
  group_by(Nationality) %>%
  summarise(Total2 = sum(Total))

DTNTotal %>%
  filter(Total2 > 200) %>%
  ggplot(aes(x = reorder(Nationality, desc(Total2)), y = Total2)) +
  geom_col(fill = brc_colours$red_dunant, colour = brc_colours$red_dunant) +
  geom_text(aes(label = scales::comma(Total2)), show.legend = FALSE, size = rel(3),  position = position_dodge(width=1), vjust=-0.25, colour = brc_colours$black_shadow) +
  theme_brc() +
  labs(title = "Nationalities with the largest number of reports via the Duty to Notify process year ending March 2023",
       # subtitle = "406 individuals from the United Kingdom were referred to Duty to Notify from April 2022 to January 2023",
       x = "Nationalities", 
       y = "Number of reports", 
       caption = "British Red Cross analysis of Home Office data, March 2022 - March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA), expand = c(0,NA))
