----#QUESTION: What is the scale of human trafficking in the UK and what is the provision of support to survivors?#----

----#Referral to NRM:----
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

#"Total" was removed from each category as it was grouped for other analyses.

#NRM referral by Sex and Age#

NRMTotal %>%
  filter(Gender != "Not Recorded") %>%
  filter(Gender != "Other") %>%
  group_by(`Age at exploitation`, Gender) %>%
  summarise(TotalSum = sum(Total)) %>%
  ggplot(aes(fill = `Age at exploitation`, x =  Gender,y =  TotalSum)) +
  geom_bar(position = "stack", stat ="identity") +
  theme_classic() +
  labs(title = "Referrals to the National Referral Mechanism (NRM) by age and sex, 2022/23", 
       x = "Sex", 
       y = "Number of Referrals",
       caption = "British Red Cross analysis of Home Office data, April 2022 to January 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 10000)) +
  scale_fill_manual(values = c(brc_colours$red_light,
                               brc_colours$red_dunant,
                               brc_colours$red_deep))
 
#NRM by Top Nationalities 

NRMTotal %>%
  filter(Nationality == "UK") %>%
  group_by(Nationality) %>%
  summarise(UK = sum(Total))

#1775 male children/ 2469 were referred to NRM- mostly male children from UK.

NRMTotal %>%
  group_by(Nationality) %>%
  summarise(TotalNat = sum(Total)) %>%
  filter(Nationality != "UK") %>%
  filter(TotalNat > 300) %>%
  ggplot(aes(x = Nationality, y = TotalNat)) +
  geom_col(fill = brc_colours$red_dunant, colour = brc_colours$red_dunant) +
  geom_text(aes(label = scales::comma(TotalNat)), show.legend = FALSE, size = rel(3)) +
  theme_classic() +
  labs(title = "Nationalities aside from the United Kingdom with the highest referrals to the National Referral Mechanism (NRM) in 2022/23",
       subtitle = "2462 individuals from the United Kingdom were referred to the NRM",
       x = "Nationality",
       y = "Number of Referrals",
       caption = "British Red Cross analysis of Home Office data, April 2022 to January 2023") +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 5000))

#Positive Conclusive Grounds 
PositiveConclusive <- nrm_conclusive_grounds %>%
  select(Year, Quarter, `Adult (18 or over) - Positive conclusive grounds`, `Child (17 or under) - Positive conclusive grounds`, `Age not specified or unknown - Positive conclusive grounds`) 

PositiveConclusive %>%
  filter(Quarter == "Total") |>
  ggplot(aes(x = Year)) +
  geom_line(aes(x = Year, y = `Adult (18 or over) - Positive conclusive grounds`), colour = brc_colours$red_dunant) +
  geom_line(aes(x = Year, y = `Child (17 or under) - Positive conclusive grounds`), colour = brc_colours$teal) +
  geom_line(aes(x = Year, y = `Age not specified or unknown - Positive conclusive grounds`), colour = brc_colours$black_shadow) +
  theme_classic() +
  labs(title = "Number of people who recieved positive conclusive grounds by age",
       subtitle = "Adults shown in red, children in teal and unknown in black",
       x = "Year", 
       y = "Number of People",
       colour = "Age",
       caption = "British Red Cross analysis of Home Office data, January 2014 until January 2023") +
  scale_x_continuous(breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) 

#To discuss with Matt why the legend is coming up with colour inside aes() but does not apply when inside, but legend shows up.


#NRM Reasonable Grounds# 

view(nrm_reasonable_grounds)

ReasonableGrounds <- nrm_reasonable_grounds %>%
  select(Year, Quarter, `Adult (18 or over) - Positive reasonable grounds`, `Child (17 or under) - Positive reasonable grounds`, `Age not specified or unknown - Positive reasonable grounds`) 

ReasonableGrounds %>%
  filter(Quarter == "Total") |>
  ggplot(aes(x = Year)) +
  geom_line(aes(x = Year, y = `Adult (18 or over) - Positive reasonable grounds`), colour = brc_colours$red_dunant) +
  geom_line(aes(x = Year, y = `Child (17 or under) - Positive reasonable grounds`), colour = brc_colours$teal) +
  geom_line(aes(x = Year, y = `Age not specified or unknown - Positive reasonable grounds`), colour = brc_colours$black_shadow) +
  theme_classic() +
  labs(title = "Number of people who recieved positive reasonable grounds by age", 
       subtitle = "Adults shown in red, children in teal and unknown in black",
       x = "Year", 
       y = "Number of People",
       caption = "British Red Cross analysis of Home Office data, January 2014 until January 2023") +
  scale_x_continuous(breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) 

#Fix the legend

----##NRM Duty to Notify#----

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
  filter(Nationality != "UK") %>%
  filter(Total2 > 200) %>%
  ggplot(aes(Nationality, Total2)) +
  geom_col(fill = brc_colours$red_dunant, colour = brc_colours$red_dunant) +
  geom_text(aes(label = scales::comma(Total2)), show.legend = FALSE, size = rel(3)) +
  theme_classic() +
  labs(title = "Nationaities with the highest number of individuals referred to Duty to Notitify aside from the United Kingdom",
       subtitle = "406 individuals from the United Kingdom were referred to Duty to Notify from April 2022 to Janaury 2023",
       x = "Nationalities", 
       y = "Number of People", 
       caption = "British Red Cross analysis of Home Office data, April 2022 - January 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))
