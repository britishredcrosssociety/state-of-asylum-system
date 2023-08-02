library(tidyverse)
library(asylum)
source("report/brc_colours.R")
source("report/theme_brc.R")

# ---- QUESTION: HOW MANY PEOPLE ARE IN RECIEPT OF ASYLYUM SUPPORT (S4, S98, S95)
  
# ---- Section 95 Support ----
# ---- Support Applications ----
# support_applications is for the number of applications to section 95.# 
Section95Apps <- support_applications %>%
  select(Year, Nationality, `Support type granted`, `Group type`, Applications) %>%
  group_by(Year, Nationality, `Support type granted`, `Group type`) %>%
  summarise(Total = sum(Applications))

TotalSection95Apps <- Section95Apps %>%
  group_by(Year, `Support type granted`,`Group type`) %>%
  summarise(Total95 = sum(Total))

# Applications per Support Type Applied for#  
TotalSection95Apps |>
  ggplot(aes(fill = `Support type granted`, y = Total95, x = Year)) +
  geom_bar(position ="stack", stat="identity") +
  theme_classic() +
  labs(title = "People Applying for Section 95 by Support Type", 
       subtitle = "Other as defined as cases that are deemed invalid, not assessed, or awaiting dispersal into accomodation",
       x = NULL, 
       y = "Number of Applications", 
       caption = "British Red Cross Analyses of Home Office Data, year ending 2022") +
  scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) + 
  scale_fill_manual(values = c(brc_colours$red_light,
                    brc_colours$red_mercer,
                    brc_colours$red_deep))

# ---- Section 95 by Nationality
Nationalities95 <- Section95Apps %>%
  group_by(Year, Nationality) %>%
  summarise(TotalNat = sum(Total)) 

# In 2020 and 2022, of all the Section95 applications made, Unknown was the highest group in Nationality. Does this even fit with the narrative? 

# ---- Section 95 Received ----
view(support_received)

SupportRecieved <- support_received %>%
  select(Date, `Support Type`, `Accommodation Type`, "UK Region" , People) 

SupportRecieved <- SupportRecieved %>%
  mutate(Year = lubridate::year(Date))

TotalSupportRecieved <- SupportRecieved %>%
  group_by(Date) %>%
  summarise(Total = sum(People))

TotalSupportRecieved |>
  ggplot(aes(Date, Total)) +
  geom_line(aes(colour = "red"), show.legend = FALSE) +
  geom_point(aes(colour = "red", size = Total, alpha = 0.5), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(2)) +
  theme_classic() +
  labs(title = "Total Number of People Recieving Section 4, 98 and Section 95 per Year", 
       x = NULL, 
       y = "Number of People", 
       caption = "British Red Cross Analyses of Home Office Data, year ending March 2023") +
  # scale_x_continuous(breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))

# ---- Support Type ----
SupportType <- SupportRecieved %>%
  group_by(Date,`Support Type`,`Accommodation Type`) %>%
  summarise(Total = sum(People))  

SupportType |>
  ggplot(aes(fill = `Support Type`, y = Total, x = Date)) +
  geom_bar(position ="stack", stat="identity") +
  theme_classic() +
  labs(title = "Number of people in receipt of asylum support under section 4, 95 and 98 of the Immigration and Asylum Act 1999, March 2014 to March 2023", 
       x = "Year", 
       y = "Number of People", 
       caption = "British Red Cross analysis of Home Office data, March 2014 to March 2023") +
  # scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 120000)) +
  scale_fill_manual(values = c(brc_colours$red_light,
                               brc_colours$red_dunant,
                               brc_colours$red_deep))

# ---- 2023 Section and Accommodation Type ---- 
SupportType %>%
  filter(Date == "2023-03-31") %>%
  filter(`Accommodation Type` != "N/A - Section 98 (pre-2023)") %>%
  filter(`Accommodation Type`!= "Subsistence only") %>%
  filter(`Accommodation Type`!= "Subsistence Only") %>%
  ggplot(aes(fill = `Accommodation Type`, y = Total, x = `Support Type`)) +
  geom_bar(position ="stack", stat="identity") +
  theme_classic() +
  labs(title = "Number of people in reciept of support by accomodation type, March 2023", 
       x = "Asylum Support", 
       y = "Number of People", 
       caption = "British Red Cross analysis of Home Office data, year ending March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 60000)) +
  guides(color = guide_legend(override.aes = list(size = 0.5))) +
  scale_fill_manual(values = c(brc_colours$steel,
                               brc_colours$teal,
                               brc_colours$red_dunant,
                               brc_colours$red_mercer))

# ---- Destitution ----
Destitution <- support_received %>%
  select(Date, `Support Type`, `Accommodation Type`, "UK Region" , People) 

Destitution <- Destitution %>%
  mutate(Year = lubridate::year(Date))

TotalDestitute <- Destitution %>%
  group_by(Year) %>%
  summarise(Total = sum(People))

TotalDestitute |>
  ggplot(aes(x = Year,
             y = Total, 
             fill = brc_colours$red_mercer, show.legend = FALSE)) +
  geom_area() +
  theme_classic() +
  labs(title = "Total Number of Persons Destitute",
       subtitle = "Individuals on Section 4, 95 and 98 are destitute",
       x = NULL,
       y = "Number of People",
       caption = "British Red Cross Analyses of Home Office Data, year ending March, 2023") +
  scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))
