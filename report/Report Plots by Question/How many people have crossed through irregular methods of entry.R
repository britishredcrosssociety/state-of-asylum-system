----#QUESTION:HOW MANY PEOPLE HAVE ARRIVED THROUGH "IRREGULAR" ENTRY?  
  
  ----#Irregular Migration#----

----##Small Boat##----
smallboat <- irregular_migration %>%
  filter(`Method of entry`== "Small boat arrivals") 

view(smallboat)


----#Small Boat x Gender#----
smallboat %>%
  select(Date, Sex, `Age Group`, Nationality, `Number of detections`)%>%
  group_by(Date, Sex) %>%
  summarise(TotalSex = sum(`Number of detections`)) |>
  ggplot(aes(Date, TotalSex)) +
  geom_line(aes(colour = Sex)) +
  theme_classic() +
  labs(title = "Small Boat Crossing by Gender", x = "Date", y = "Number of Detections")

----#Small Boat x Age Group#----
smallboat %>%
  select(Date, Sex, `Age Group`, Nationality, `Number of detections`) %>%
  group_by(Date, `Age Group`) %>%
  summarise(TotalAge = sum(`Number of detections`)) |>
  ggplot(aes(Date, TotalAge)) +
  geom_line(aes(colour = `Age Group`)) +
  theme_classic() +
  labs(title = "Small Boat Crossing by Age Group",
       x = "Date", 
       y = "Number of Detections")


----#Small Boat x Quarter#----
Smallboatbyquarter <- smallboat %>% 
  select(Date, Quarter, `Number of detections`) %>%
  group_by(Date, Quarter) %>%
  summarise(TotalQ = sum(`Number of detections`))

view(Smallboatbyquarter)

Smallboatbyquarter |>
  ggplot(aes(Date, TotalQ, group = Quarter)) +
  geom_line(aes(color = Quarter)) +
  geom_point(aes(colour = Quarter)) +
  theme_classic() +
  labs(title = "Small Boat Arrivals by Quarter", 
       x = "Year", 
       y = "Number of Small Boat Detections")

#colour need to be edited for this graph.#

----#Small Boat x Asylum Applications#----

SmallboatAsylum <- small_boat_asylum_applications %>%
  select(Year, `Age Group`, Sex, Region, Applications, `Asylum application`)
filter("Asylum application" != "No asylum application raised")

SmallboatAsylum %>%
  select(Year, `Age Group`, Applications) %>%
  group_by(Year, `Age Group`) %>%
  summarise(SBAsyTotal = sum(Applications)) |>
  ggplot(aes(Year, SBAsyTotal)) +
  geom_line(aes(colour = `Age Group`)) +
  theme_classic() +
  labs(title = "Asylum Applications from Small Boat Arrivals",
       x = "Year", 
       y = "Number of Applications")





----###Small Boat -To revise Nationalities###---- 
smallboat %>%
  filter(Date > "2021-10-01")
select(Date, Nationality, `Number of detections`) %>%
  group_by(Date, Nationality) %>%
  summarise(TotalNationality = sum(`Number of detections`)) %>%
  ggplot(aes(Date, TotalNationality)) +
  geom_point(aes(colour = Nationality)) +
  theme_classic() +
  labs(title = "Small Boat Crossing by Nationality",
       x = "Date", 
       y = "Number of Detections")


----#Irregular Migration by Method of Entry#----  
irregular_migration %>%
  select(Date, `Method of entry`, `Number of detections`) %>%
  group_by(Date, `Method of entry`) %>%
  summarise(TotalbyMethod = sum(`Number of detections`)) %>%
  ggplot(aes(Date, TotalbyMethod)) +
  geom_line(aes(colour = `Method of entry`)) +
  theme_classic() +
  labs(title =  "Irregular Migration by Method of Entry",
       x = "Date",
       y = "Number of Detections")