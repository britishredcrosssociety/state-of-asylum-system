----#QUESTION:HOW MANY PEOPLE HAVE ARRIVED THROUGH "IRREGULAR" ENTRY?  
  
  ----#Irregular Migration#----

----##Small Boat##----
smallboat <- irregular_migration %>%
  filter(`Method of entry`== "Small boat arrivals") 

view(smallboat)

----#Small Boat x Gender#----
smallboat %>%
  filter(Sex != "Not currently recorded") %>%
  filter(Sex != "Unknown") %>%
  select(Year, Sex, `Age Group`, Nationality, `Number of detections`)%>%
  group_by(Year, Sex) %>%
  summarise(TotalSex = sum(`Number of detections`))  |>
  ggplot(aes(Year, TotalSex)) +
  geom_point(aes(size = TotalSex, colour = Sex), alpha = 0.5, show.legend = FALSE) +
  geom_line(aes(colour = Sex)) +
  geom_text(aes(label = scales::comma(TotalSex)), show.legend = FALSE, size = rel(3)) +
  theme_classic() +
  labs(title = "Small Boat Crossing by Gender", 
       x = NULL, 
       y = "Number of Detections", 
       caption = "British Red Cross Analyses of Home Office Data, year ending 2022") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_x_continuous(breaks = c(2018, 2019, 2020, 2021, 2022, 2023))

----#Small Boat x Age Group#----
smallboat %>%
  filter(`Age Group` != "Not currently recorded") %>%
  select(Date, Year, Sex, `Age Group`, Nationality, `Number of detections`) %>%
  group_by(Year, `Age Group`) %>%
  summarise(TotalAge = sum(`Number of detections`)) |>
  ggplot(aes(fill = `Age Group`, x = Year, y = TotalAge)) +
  geom_bar(position = "stack", stat = "identity") +
  #geom_text(aes(label = scales::comma(TotalAge)), show.legend = FALSE, size = rel(3)) +
  theme_classic() +
  labs(title = "Small Boat Crossing by Age",
       x = NULL, 
       y = "Number of Detections",
       caption = "British Red Cross Analyses of Home Office Data, year ending 2022") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))


#Age and Sex 2022#
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
  labs(title = "Irregular Crossings by Small Boat by Age and Sex in 2022",
       x = NULL, 
       y = "Number of Detections",
       caption = "British Red Cross Analyses of Home Office Data, year ending 2022") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))  

----#Small Boat x Quarter#----
Smallboatbyquarter <- smallboat %>% 
  select(Year, Quarter, `Number of detections`) %>%
  group_by(Year, Quarter) %>%
  summarise(TotalQ = sum(`Number of detections`))

view(Smallboatbyquarter)

Smallboatbyquarter |>
  ggplot(aes(fill = Quarter, x = Year, y = TotalQ)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  labs(title = "Irregular Crossings by Small Boat by Quarter", 
       x = NULL, 
       y = "Number of Detections",
       caption = "British Red Cross Analyses of Home Office Data, year ending 2022") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) 

#BRC colours need to be added#

----#Small Boat Detections by Nationality#----

SmallBoatNat <- smallboat %>%
  filter(Year == 2022) %>%
  select(Year, Nationality, `Number of detections`) %>%
  group_by(Nationality) %>%
  summarise(Total = sum(`Number of detections`))

SmallBoatNat %>%
  filter(Total > 1000) %>%
  ggplot(aes(Nationality, Total)) +
  geom_col(aes(fill = brc_colours$red_mercer), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(3)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.5)) +
  labs(title = "Top 10 Nationalities Detected on Small Boat Crossings in 2022", 
       x = "Nationalities", 
       y = "Number of Detections", 
       caption = "British Red Cross Analyses of Home Office Data, 2022")
  

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
  labs(title = "Asylum Applications from Small Boat Arrivals",
       x = "Year", 
       y = "Number of Applications", 
       caption = "British Red Cross Analyses of Home Office Data, year ending March, 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) 


----#Irregular Migration by Method of Entry#----  
irregular_migration %>%
  select(Year, `Method of entry`, `Number of detections`) %>%
  group_by(Year, `Method of entry`) %>%
  summarise(TotalbyMethod = sum(`Number of detections`)) %>%
  ggplot(aes(Year, TotalbyMethod)) +
  geom_line(aes(colour = `Method of entry`)) +
  theme_classic() +
  labs(title =  "Irregular Crossings by Method of Entry",
       x = NULL,
       y = "Number of Detections", 
       caption = "British Red Cross Analyses of Home Office Data, year ending March, 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) 