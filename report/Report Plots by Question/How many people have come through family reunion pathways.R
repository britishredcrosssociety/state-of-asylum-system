----#QUESTION: HOW MANY PEOPLE HAVE COME THROUGH FAMILY REUINION PATHWAYS 
----#Family Reunification#----

view(family_reunion)

Totalfamreunion <- family_reunion %>%
  select(Year, `Visas granted`) %>%
  group_by(Year) %>%
  summarise(Total = sum(`Visas granted`)) %>%
  ggplot(aes(Year, Total)) +
  geom_line(aes(colour = "red"), show.legend = FALSE) +
  geom_point(aes(colour = "red", alpha = 0.5, size = Total), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(2)) +
  theme_classic() +
  labs(title = "Number of Family Reunion Visas Granted",
       x = "Year",
       y = "Number of Visas Granted")

Totalfamreunion + scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023.1))
