library(tidyverse)
library(asylum)
source("report/brc_colours.R")
source("report/theme_brc.R")

# ---- QUESTION: HOW MANY PEOPLE ARE IN RECIEPT OF ASYLYUM SUPPORT (S4, S98, S95)
  
# ---- Section 95 Support ----
# ---- Section 95 Received ---- 

SupportRecieved <- support_received %>%
  select(Date, `Support Type`, `Accommodation Type`, "UK Region" , People)

# SupportRecieved <- SupportRecieved %>%
#   mutate(Year = lubridate::year(Date))
# 
# TotalSupportRecieved <- SupportRecieved %>%
#   group_by(Date) %>%
#   summarise(Total = sum(People))
# 
# TotalSupportRecieved |>
#   ggplot(aes(Date, Total)) +
#   geom_line(aes(colour = "red"), show.legend = FALSE) +
#   geom_point(aes(colour = "red", size = Total, alpha = 0.5), show.legend = FALSE) +
#   geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(2)) +
#   theme_classic() +
#   labs(title = "Total Number of People Recieving Section 4, 98 and Section 95 per Year", 
#        x = NULL, 
#        y = "Number of People", 
#        caption = "British Red Cross Analyses of Home Office Data, year ending March 2023") +
#   # scale_x_continuous(breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
#   scale_y_continuous(labels = scales::comma, limits = c(0, NA))

# ---- Support Type ----
SupportType <- SupportRecieved |>
  group_by(Date,`Support Type`,`Accommodation Type`) |>
  summarise(Total = sum(People))  

SupportType |>
  ggplot(aes(fill = `Support Type`, y = Total, x = Date)) +
  geom_bar(position ="stack", stat="identity") +
  theme_brc() +
  labs(title = str_wrap("Number of people in receipt of asylum support from 2014 to 2023"),
       subtitle = "Asylum support under section 4, 95 and 98 of the Immigration and Asylum Act 1999",
       x = "Year", 
       y = "Number of people", 
       caption = "British Red Cross analysis of Home Office data, March 2014 to June 2023") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 120000), expand = c(0, NA)) +
  scale_fill_manual(values = c(brc_colours$red_deep,
                               brc_colours$red_mercer,
                               brc_colours$red_light))

# ---- 2023 Section and Accommodation Type ---- 
SupportType %>%
  filter(Date > "2022-12-31") |>
  filter(`Accommodation Type` != "N/A - Section 98 (pre-2023)") |>
  filter(`Accommodation Type`!= "Subsistence only") |>
  filter(`Accommodation Type`!= "Subsistence Only") |> 
  ggplot(aes(fill = `Accommodation Type`, y = Total, x = `Support Type`)) +
  geom_bar(position ="stack", stat="identity") +
  #geom_text(aes(label = scales::comma(Total)),
            #position = position_stack(vjust = .5), size = 2) +
  theme_brc() +
  labs(title = "Number of people in receipt of support by accomodation type as of June 2023", 
       x = "Type of asylum support", 
       y = "Number of people", 
       caption = "British Red Cross analysis of Home Office data, year ending June 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA), expand = c(0, NA)) +
  guides(color = guide_legend(override.aes = list(size = 0.5))) +
  scale_fill_manual(values = c(brc_colours$steel,
                               brc_colours$teal,
                               brc_colours$red_dunant,
                               brc_colours$red_earth))

#To discuss with Alice and Tamara on how to plot and viz destitution.#
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
