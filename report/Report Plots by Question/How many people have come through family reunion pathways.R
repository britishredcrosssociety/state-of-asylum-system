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

#Family Reunion Visas by Sex and Age 

FamilyReunion <- family_reunion %>% 
  group_by(Year, Nationality, Sex, Age) %>%
  summarise(Total = sum(`Visas granted`))

#For reference, review of trends of Fam Reunion by Sex over last decade 
FamilyReunion %>%
  group_by(Year, Sex) %>%
  summarise(TotalbySex = sum(Total)) |>
  ggplot(aes(Year, TotalbySex)) +
  geom_line(aes(colour = Sex))

FamReuinion22 <- FamilyReunion %>%
  filter(Year == 2022) %>%
  group_by(Sex, Age) %>%
  summarise(TotalbyAge = sum(Total)) 

arrange(FamReuinion22)

FambyAS <- (FamReuinion22 %>% 
  arrange(factor(Age, levels = c('Under 18', '18-29', '30-49', '50-69', '70+'))) %>%
  ggplot(aes(fill = Age, x = Sex, y = TotalbyAge)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  labs(title = "Family Reunion Visas Granted by Sex and Age Group, 2022", 
       x = "Sex", 
       y = "Total Visas Granted", 
       caption = "BRC Analyses of HO Data, March 2023"))

FambyAS + scale_fill_manual(values = c("#9d1f21",
                                       "#651713",
                                       "#e95351",
                                       "#d0011b",
                                       "#ee2a24"))