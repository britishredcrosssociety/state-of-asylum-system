library(tidyverse)
library(asylum)
source("report/brc_colours.R")
source("report/theme_brc.R")

# ---- QUESTION: HOW MANY PEOPLE HAVE COME THROUGH FAMILY REUINION PATHWAYS 
# ---- Family Reunion ----
view(family_reunion)

Totalfamreunion <- family_reunion %>%
  # select(Year, `Visas granted`) %>%
  group_by(Year) %>%
  summarise(Total = sum(`Visas granted`)) %>%
  ggplot(aes(Year, Total)) +
  geom_line(aes(colour = "red"), show.legend = FALSE) +
  geom_point(aes(colour = "red", alpha = 0.5, size = Total), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(3)) +
  theme_classic() +
  labs(title = "Number of family reunion visas granted from 2010 to 2023",
       x = NULL,
       y = "Number of Visas Granted", 
       caption = "British Red Cross analysis of Home Office data, March 2010 to March 2023")

Totalfamreunion + 
scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
scale_y_continuous(labels = scales::comma, limits = c(0, 8000))

# ---- Family Reunion Visas by Sex and Age ----
FamilyReunion <- family_reunion %>% 
  group_by(Year, Nationality, Sex, Age) %>%
  summarise(Total = sum(`Visas granted`))

FamReuinion22 <- FamilyReunion %>%
  filter(Year == 2022) %>%
  group_by(Sex, Age) %>%
  summarise(TotalbyAge = sum(Total)) 

FamReuinion22$Age <- factor(FamReuinion22$Age, levels=c('70+', '50-69', '30-49', '18-29', 'Under 18'))

FamReuinion22 |>
  ggplot(aes(fill = Age, x = Sex, y = TotalbyAge)) +
  geom_bar(position = "stack", stat = "identity") +
  ggrepel::geom_text_repel(aes(label = scales::number(TotalbyAge, big.mark = ',', accuracy = 1)), size = 3, 
                           position = position_stack(vjust = 0.5), direction = "y", 
                           box.padding = unit(0.01, "lines")) +
  # geom_text(aes(label = scales::comma(TotalbyAge)), show.legend = FALSE, size = rel(2), position = position_stack(vjust = .5)) +
  theme_classic() +
  labs(title = "Family reunion visas granted by sex and age for year ending December 2022", 
       x = "Sex", 
       y = "Total Visas Granted", 
       caption = "British Red Cross analysis of Home Office data, year ending December 2022") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 3000)) +
  scale_fill_manual(values = c(brc_colours$red_deep,
                                     brc_colours$red_light,
                                     brc_colours$red_dunant,
                                     brc_colours$red_mercer,
                                     brc_colours$red_earth))
