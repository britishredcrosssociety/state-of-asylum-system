library(tidyverse)
library(asylum)
source("report/brc_colours.R")
source("report/theme_brc.R")

# ---- QUESTION: HOW MANY PEOPLE HAVE AGE DISPUTES? 
  
# ---- Age Dispute Raised or Resolved ----
AgeDispute <- age_disputes |>
  group_by(Year, `Raised or resolved`) |>
  summarise(Total = sum(`Age disputes`)) 

AgeDispute |>
  ggplot(aes(fill = `Raised or resolved`, y = Total, x = Year)) + 
  geom_bar(position="stack", stat="identity") +
  #geom_text(aes(label = Total),
            #position = position_stack(vjust = .5), size = 2) +
  theme_brc() +
  labs(title = "Number of age dispute cases raised and resolved from 2010 to 2023",
       x = "Year",
       y = "Number of cases",
       fill = "Age dispute raised or resolved",
       caption = "British Red Cross analysis of Home Office data, March 2010 to June 2023") +
  scale_x_continuous(breaks = c(2010:2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 8000), expand = c(0, NA)) +
  scale_fill_manual(values = c(brc_colours$red_dunant,
                               brc_colours$red_deep))
  
# ---- Age Dispute by Resolved Reason ---- 
AgeDispute <- age_disputes |>
  group_by(Year, Nationality, `Raised type / Resolved outcome`, `Raised or resolved`) |>
  summarise(Total = sum(`Age disputes`))

ByOutcome <- AgeDispute %>%
  filter(`Raised type / Resolved outcome` != 'Existing asylum application') |>
  filter(`Raised type / Resolved outcome` != 'Asylum application raised in quarter') |>
  group_by(Year,`Raised type / Resolved outcome`) |>
  summarise(TotalDispute = sum(Total))
ggplot(ByOutcome, aes(fill = `Raised type / Resolved outcome`, y = TotalDispute, x = Year)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = scales::comma(TotalDispute)),
            position = position_stack(vjust = .5), size = 3) +
  theme_brc() +
  labs(title = "Outcome of age dispute cases from 2010 to 2023", 
       x = "Year", 
       y = "Total cases",
       fill = 'Outcome of age dispute',
       caption = "British Red Cross analysis of Home Office data, March 2010 to June 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 3500), expand = c(0, NA)) +
  scale_x_continuous(breaks = c(2010:2023)) +
  scale_fill_manual(values = c(brc_colours$red_dunant,
                               brc_colours$red_earth))

# Age Dispute Nationality for Raised- REVISE by asking the team if they want any specific nationality highlighted. 
age_disputes |>
  filter(`Raised or resolved` != "Raised") |>
  filter(Year > 2021) |>
  group_by(Year, Nationality) |>
  summarise(Total = sum(`Age disputes`)) |> 
  ggplot(aes(Year, Total)) +
  geom_point(aes(colour = Nationality, size = Total), alpha = 0.5) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(3)) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  theme_classic() +
  guides(color = guide_legend(override.aes = list(size = 0.5)))
