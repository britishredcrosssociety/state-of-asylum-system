library(tidyverse)
library(asylum)
source("report/brc_colours.R")
source("report/theme_brc.R")

# ---- QUESTION: WHAT IS THE STATE OF DETENTION?

# ---- 1. Women in Detention ----
people_in_detention$Age <- factor(people_in_detention$Age, levels=c('70 and over',
                                                                    '50 to 69',
                                                                    '30 to 49',
                                                                    '18 to 29',
                                                                    '17 and under'))

people_in_detention %>%
  filter(Sex == "Female") %>%
  group_by(Age) %>%
  ggplot(aes(fill = Age, x = Year, y = People)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_brc() +
  scale_fill_manual(values = c(brc_colours$teal,
                               brc_colours$steel,
                               brc_colours$red_light,
                               brc_colours$red_mercer,
                               brc_colours$red_deep)) +
  labs(title = "Number of women in immigration detention from 2010 to 2023",
       x = "Year",
       y = "Number of women",
       caption = "British Red Cross analysis of Home Office data, March 2010 to March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 1500)) +
  scale_x_continuous(breaks = c(2010:2023))

# ---- 2.  Children in Detention ----
#ChildrenDetention <- people_in_detention |>
#filter(Age == "17 and under") |>
#group_by(Year) |>
#summarise(Total = sum(People)) 

#ChildrenDetention |>
#ggplot(aes(Year, Total)) +
#geom_col(colour = brc_colours$red_dunant, fill = brc_colours$red_dunant)
# Check with team about children IN and children entering detention and why these numbers are so different?!


# using children entering detention as a more accurate depiction rather than in detention as the data is not recorded as well? 
view(children_entering_detention)

children_entering_detention |>
  ggplot(aes(`Date of entry to detention`, `Total children`)) +
  geom_col(aes(fill = brc_colours$red_dunant, colour = brc_colours$red_dunant), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(`Total children`)), show.legend = FALSE, size = rel(3), position = position_dodge(width=1), vjust=-0.25) +
  theme_brc() +
  labs(title = "Number of children entering immigration detention from 2010 to 2022",
       x = "Year",
       y = "Number of children",
       caption = "British Red Cross analysis of Home office data, March 2010 to March 2022") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 600), expand = c(0,NA)) +
  scale_x_continuous(breaks = c(2010:2022)) 

# ---- 3. Leaving Detention and Reason ---- 

people_leaving_detention |> 
  mutate(`Reason for leaving detention` = 
           case_match(
             `Reason for leaving detention`,
             (c("Bailed (IJ)",
                "Bailed (SoS)") ~ "Bailed"),
             .default = `Reason for leaving detention`)
  ) %>%
  group_by(Year, `Reason for leaving detention`) %>%
  summarise(Total = sum(Leaving)) %>%
  ggplot(aes(fill = `Reason for leaving detention`, x = Year, y = Total)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_brc() +
  labs(title = "Number of people leaving detention by reason from 2010 to 2023",
       subtitle = str_wrap("Bailed includes bailed by immigration judge and Secretary of State. Other is defined as people returned to criminal detention, released unconditionally, those sectioned under the Mental Health Act, deaths, absconds and reasons unavailable when leaving detention"),
       x = "Year",
       y = "Number of people",
       caption = "British Red Cross analysis of Home Office data, March 2010 to March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 40000), expand = c(0,NA)) +
  scale_x_continuous(breaks = c(2010:2023)) +
  scale_fill_manual(values = c(brc_colours$steel,
                               brc_colours$teal,
                               brc_colours$red_earth,
                               brc_colours$red_dunant))

# ---- 4. Detention Pregnant People ----
detention_pregnant_women |>
  select(Year, `Number of pregnant women detained in the immigration detention estate`) |>
  group_by(Year) |>
  summarise(Total = sum(`Number of pregnant women detained in the immigration detention estate`)) |>
  ggplot(aes(Year, Total)) +
  geom_line(aes(colour = brc_colours$red_dunant), show.legend = FALSE) +
  geom_point(aes(colour = brc_colours$red_dunant), size= 5, alpha = 0.5, show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(3)) +
  theme_brc() +
  labs(title = "Number of pregnant people in immigration detention from 2016 to 2023",
       x = "Year", 
       y = "Number of pregnant people", 
       caption = "British Red Cross analysis of Home Office data, July 2016 to March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 60), expand = c(0,NA)) +
  scale_x_continuous(breaks = c(2016:2023)) 

# ---- 5. Detention: Age ---- 
AgeSexDetention <- people_in_detention |>
  select(Year, Age, Sex, People) |>
  group_by(Year, Age, Sex) |>
  summarise(AgeTotal = sum(People))

AgeSexDetention$Age <- factor(AgeSexDetention$Age, levels=c('70 and over',
                                                            '50 to 69',
                                                            '30 to 49',
                                                            '18 to 29',
                                                            '17 and under'))

AgeSexDetention |>
  filter(Age != "NA") |>
  ggplot(aes(fill = Age, Year, AgeTotal)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_brc() +
  labs(title = "Age of people in immigration detention from 2010 to 2023",
       x = "Year",
       y = "Number of people",
       caption = "British Red Cross analysis of Home Office data, March 2010 to March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 15000)) +
  scale_x_continuous(breaks = c(2010:2023)) +
  scale_fill_manual(values = c(brc_colours$teal,
                               brc_colours$steel,
                               brc_colours$red_light,
                               brc_colours$red_mercer,
                               brc_colours$red_deep))

# ---- 6. People in Detention ----
OverallinDetention <- people_in_detention |>
  select(Year, Age, Sex, `Length of detention`, People) |>
  group_by(Year) |>
  summarise(TotalinDetention = sum(People))

OverallinDetention |>
  ggplot(aes(Year, TotalinDetention)) +
  geom_line(aes(colour = brc_colours$red_dunant), show.legend = FALSE) +
  geom_point(aes(colour = brc_colours$red_dunant, alpha = 0.5, size = 4), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(TotalinDetention)), show.legend = FALSE, size = rel(3)) +
  theme_brc() +
  labs(title = "Number of people in immigration detention from 2010 to 2023",
       x = "Year",
       y = "Number of people", 
       caption = "British Red Cross Analysis of Home Office data, March 2010 to March 2023") +
  scale_x_continuous(breaks = c(2010 : 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 15000), expand = c(0,NA))

# ---- 7. Length of Time in Detention ----
detention_length <- 
  asylum::people_leaving_detention |>
  
  #  Remove initial letter and colon
  mutate(`Length of detention` = str_remove(`Length of detention`, "^[A-Z]:\\s")) |> 
  
  #  Group into fewer categories
  mutate(`Length of detention` = case_match(
    `Length of detention`,
    c("3 days or less", "4 to 7 days", "8 to 14 days") ~ "A: Up to two weeks",
    c("15 to 28 days") ~ "B: 15 to 28 days",
    .default = "C: 29 days or more"
  )) |> 
  
  group_by(Year, `Length of detention`) |> 
  summarise(People = sum(Leaving, na.rm = TRUE)) |> 
  
  #  Remove initial letter and colon
  mutate(`Length of detention` = str_remove(`Length of detention`, "^[A-Z]:\\s"))
  
detention_length$`Length of detention` <- factor(detention_length$`Length of detention`, 
                                                           levels=c("29 days or more",
                                                                    "15 to 28 days",
                                                                    "Up to two weeks"))

detention_length |> 
  ggplot(aes(fill =`Length of detention`, x = Year, y = People)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_brc() +
  labs(title = "Length of time people have spent in immigration detention from 2010 to 2023",
       x = "Year",
       y = "Number of people",
       caption = "British Red Cross analysis of Home Office data, March 2010 to March 2023") +
  scale_x_continuous(breaks = c(2010:2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 40000), expand = c(0,NA)) +
  scale_fill_manual(values = c(brc_colours$red_light,
                               brc_colours$red_mercer,
                               brc_colours$red_deep))


# ---- How many people are in immigration detention and what is the size of immigration detention estate? Baseline from previous years ----

# calculate % increase since April 2021
asylum::detention_cost_per_day |> 
  select(Date, Cost) |> 
  filter(Date %in% c(ymd("2021-07-01"), max(Date))) |> 
  # arrange(desc(Date)) |> 
  mutate(Percent_change = (Cost - lag(Cost)) / lag(Cost))

# Cost of Detention 
                               
detention_cost_per_day |>
  ggplot(aes(Date, Cost)) +
  geom_line(colour = brc_colours$red_dunant) +
  #geom_text(aes(label = scales::comma(Cost)), show.legend = FALSE, size = rel(3)) +
  theme_brc() +
  labs(title = "Cost of immigration detention per day from 2013 to 2023",
       subtitle = "Average cost (£) per day to hold a person in immigration detention",
       x = "Year",
       y = "Average cost (£)",
       caption = "British Red Cross analysis of Home Office data, March 2013 to March 2023") +
  #scale_x_continuous(breaks = c(2013:2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))
