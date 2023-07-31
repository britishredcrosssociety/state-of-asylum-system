----#QUESTION:HOW MANY PEOPLE HAVE ARRIVED THROUGH "IRREGULAR" ENTRY?  

  smallboat <- irregular_migration %>%
    filter(`Method of entry`== "Small boat arrivals") 
  
----#Nationalities Small Boat#----
SmallBoatNat <- smallboat %>%
  filter(Year > 2021) %>%
  select(Year, Nationality, `Number of detections`) %>%
  group_by(Nationality) %>%
  summarise(Total = sum(`Number of detections`))

SmallBoatNat$Nationality <- factor(SmallBoatNat$Nationality, levels = SmallBoatNat$Nationality[order(SmallBoatNat$Total, decreasing = TRUE)])

SmallBoatNat %>%
  filter(Total > 1200) %>%
  filter(Nationality != "Not currently recorded") |>
  ggplot(aes(Nationality, Total)) +
  geom_col(aes(fill = brc_colours$red_dunant), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(3), position = position_dodge(width=1), vjust=-0.25) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.5)) +
  labs(title = "Number of people detected crossing the channel in small boats from March 2022 to March 2023",
       subtitle = "Top 10 nationalities detected crossing the channel",
       x = "Nationalities", 
       y = "Number of People Detected", 
       caption = "British Red Cross analysis of Home Office data, March 2022 to March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 15000)) 

view(smallboat)

----#Age and Sex 2022#----

smallboat$`Age Group` <- factor(smallboat$`Age Group`, levels=c('40 and over', '25 to 39', '18 to 24', '17 and under'))

smallboat %>%
  filter(`Age Group` != "Not currently recorded") %>%
  filter(Sex != "Unknown") %>%
  filter(Year == 2022) %>%
  select(Date, Year, Sex, `Age Group`, Nationality, `Number of detections`) %>%
  group_by(Year, `Age Group`, Sex) %>%
  summarise(Total = sum(`Number of detections`)) |>
  ggplot(aes(fill = `Age Group`, x = Sex, y = Total)) +
  geom_bar(position = "stack", stat = "identity") +
  #geom_text(aes(label = scales::comma(TotalAge)), show.legend = FALSE, size = rel(3)) +
  theme_classic() +
  labs(title = "Number of people crossing the channel by small boats by age and sex, 2018 to 2022",
       x = "Year", 
       y = "Number of People Detected",
       caption = "British Red Cross analysis of Home Office data, March 2018 to March 2022") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_fill_manual(values = c(brc_colours$red_deep,
                               brc_colours$red_earth,
                               brc_colours$red_dunant,
                               brc_colours$red_light))

----#Small Boat x Quarter#----
Smallboatbyquarter <- smallboat %>% 
  select(Year, Quarter, `Number of detections`) %>%
  group_by(Year, Quarter) %>%
  summarise(TotalQ = sum(`Number of detections`))

view(Smallboatbyquarter)

Smallboatbyquarter |>
  ggplot(aes(fill = factor(Quarter), x = Year, y = TotalQ)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  labs(title = "Number of people crossing the channel by quarter from March 2018 to March 2022", 
       subtitle = "The greatest number of detections are seen in the third quarter",
       x = "Year", 
       y = "Number of People Detected",
       fill = "Quarter",
       caption = "British Red Cross analysis of Home Office data, March 2018 to March 2022") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 50000)) +
  scale_fill_manual(values = c(brc_colours$red_deep,
                               brc_colours$red_earth,
                               brc_colours$red_dunant,
                               brc_colours$red_light))


----#Small Boat x Asylum Applications#----

SmallboatAsylum <- small_boat_asylum_applications %>%
  select(Year, `Age Group`, Sex, Region, Applications, `Asylum application`) %>%
  filter("Asylum application" != "No asylum application raised")

SmallboatAsylum %>%
  select(Year, `Age Group`, Applications) %>%
  group_by(Year) %>%
  summarise(Total = sum(Applications)) |>
  ggplot(aes(Year, Total)) +
  geom_point(aes(colour = brc_colours$red_dunant, size = Total), alpha = 0.5, show.legend = FALSE) +
  geom_line(aes(colour = brc_colours$red_dunant), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(3)) +
  theme_classic() +
  labs(title = "Number of asylum applications from people crossing the channel in small boats from March 2018 to March 2023",
       x = "Year", 
       y = "Number of Applications", 
       caption = "British Red Cross analysis of Home Office data, March 2018 to March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 50000)) 


----#Irregular Migration by Method of Entry#----  
irregular_migration %>%
  select(Year, `Method of entry`, `Number of detections`) %>%
  group_by(Year, `Method of entry`) %>%
  summarise(TotalbyMethod = sum(`Number of detections`)) %>%
  ggplot(aes(Year, TotalbyMethod), group = `Method of entry`) +
  geom_point(aes(group = `Method of entry`, size = TotalbyMethod, alpha = 0.5, colour = `Method of entry`), show.legend = FALSE) +
  geom_line(aes(group = `Method of entry`, colour = `Method of entry`)) +
  geom_text(aes(label = scales::comma(TotalbyMethod)), show.legend = FALSE, size = rel(2)) +
  theme_classic() +
  labs(title =  "Number of people detected by irregular methods of entry, from March 2018 to March 2023",
       x = NULL,
       y = "Number of People", 
       caption = "British Red Cross analysis of Home Office data, March 2018 to March, 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 50000)) +
  scale_colour_manual(values = c(brc_colours$red_deep,
                                          brc_colours$red_earth,
                                          brc_colours$red_light,
                                          brc_colours$red_dunant))