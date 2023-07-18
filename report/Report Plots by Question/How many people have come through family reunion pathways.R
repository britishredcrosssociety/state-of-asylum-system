----#QUESTION: HOW MANY PEOPLE HAVE COME THROUGH FAMILY REUINION PATHWAYS 
----#Family Reunification#----

view(family_reunion)

Totalfamreunion <- family_reunion %>%
  #select(Year, `Visas granted`) %>%
  group_by(Year) %>%
  summarise(Total = sum(`Visas granted`)) %>%
  ggplot(aes(Year, Total)) +
  geom_line(aes(colour = "red"), show.legend = FALSE) +
  geom_point(aes(colour = "red", alpha = 0.5, size = Total), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(2)) +
  theme_classic() +
  labs(title = "Number of Family Reunion Visas Granted",
       x = NULL,
       y = "Number of Visas Granted", 
       caption = "British Red Cross Analysis of HO Data, March 2023")

Totalfamreunion + 
scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
scale_y_continuous(labels = scales::comma, limits = c(0, NA))

#Family Reunion by Quarter Across All Years#

FamilyRunionbyQ <- family_reunion %>%
  group_by(Year, Quarter) %>%
  summarise(Total = sum(`Visas granted`)) %>%
  ggplot(aes(fill = Quarter, x = Year, y = Total)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  labs(title = "Number of Family Reunion Visas Granted per Quarter",
       x = NULL,
       y = "Number of Visas Granted", 
       caption = "British Red Cross Analysis of HO Data, March 2023") +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))
  
  
FamilyRunionbyQ

#To change colours#

#Family Reunion Visas by Sex and Age 

FamilyReunion <- family_reunion %>% 
  group_by(Year, Nationality, Sex, Age) %>%
  summarise(Total = sum(`Visas granted`))

FamReuinion22 <- FamilyReunion %>%
  filter(Year == 2022) %>%
  group_by(Sex, Age) %>%
  summarise(TotalbyAge = sum(Total)) 

arrange(FamReuinion22)

FamReuinion22$Age <- factor(FamReuinion22$Age, levels=c('70+', '50-69', '30-49', '18-29', 'Under 18'))

FamAS <- (FamReuinion22 |>
  ggplot(aes(fill = Age, x = Sex, y = TotalbyAge)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  labs(title = "Family Reunion Visas Granted by Sex and Age Group, 2022", 
       x = "Sex", 
       y = "Total Visas Granted", 
       caption = "British Red Cross Analyses of Home Office Data, March 2023"))


FamAS + scale_fill_manual(values = c(brc_colours$red_deep,
                                     brc_colours$red_light,
                                     brc_colours$red_dunant,
                                     brc_colours$red_mercer,
                                     brc_colours$red_earth))