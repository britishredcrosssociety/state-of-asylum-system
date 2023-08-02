----#QUESTION: WHAT IS THE STATE OF DETENTION?

----#People in Detention#----
OverallinDetention <- people_in_detention %>%
  select(Year, Age, Sex, `Length of detention`, People) %>%
  group_by(Year) %>%
  summarise(TotalinDetention = sum(People))

OverallinDetention |>
  ggplot(aes(Year, TotalinDetention)) +
  geom_line(aes(colour = "red"), show.legend = FALSE) +
  geom_point(aes(colour = "red", alpha = 0.5), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(TotalinDetention)), show.legend = FALSE, size = rel(2)) +
  theme_classic() +
  labs(title = "Number of people in immigration detention from 2010 to 2023",
       x = "Year",
       y = "Number of People", 
       caption = "British Red Cross Analysis of Home Office data, March 2010 to March 2023") +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 15000))


----#Length of Time in Detention#----

detention_length <- 
  asylum::people_leaving_detention |>
  
  # Remove initial letter and colon
  mutate(`Length of detention` = str_remove(`Length of detention`, "^[A-Z]:\\s")) |> 
  
  # Group into fewer categories
  mutate(`Length of detention` = case_match(
    `Length of detention`,
    c("3 days or less", "4 to 7 days", "8 to 14 days") ~ "A: Up to two weeks",
    c("15 to 28 days") ~ "B: 15 to 28 days",
    .default = "C: 29 days or more"
  )) |> 
  
  group_by(Year, `Length of detention`) |> 
  summarise(People = sum(Leaving, na.rm = TRUE)) |> 
  
  # Remove initial letter and colon
  mutate(`Length of detention` = str_remove(`Length of detention`, "^[A-Z]:\\s"))

detention_length |> 
  ggplot(aes(fill =`Length of detention`, x = Year, y = People)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  labs(title = "Length of time people have spent in immigration detention from 2010 - 2023",
       x = "Year",
       y = "Number of People",
       caption = "British Red Cross analysis of Home Office data, March 2010 to 2023") +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 40000)) +
  scale_fill_manual(values = c(brc_colours$red_light,
                               brc_colours$red_dunant,
                               brc_colours$red_deep))

----#Detention: Age and Sex#---- 

AgeSexDetention <- people_in_detention %>%
  select(Year, Age, Sex, People) %>%
  group_by(Year, Age, Sex) %>%
  summarise(AgeTotal = sum(People))

AgeSexDetention$Age <- factor(AgeSexDetention$Age, levels=c('70 and over',
                                                            '50 to 69',
                                                            '30 to 49',
                                                            '18 to 29',
                                                            '17 and under'))
                                                                
AgeSexDetention %>%
  filter(Age != "NA") %>%
  ggplot(aes(fill = Age, Year, AgeTotal)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  labs(title = "Age of people in immigration detention from 2010 to 2023",
       x = "Year",
       y = "Number of People",
       caption = "British Red Cross analysis of Home Office data, March 2010 to March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 15000)) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_fill_manual(values = c(brc_colours$red_deep,
                               brc_colours$red_earth,
                               brc_colours$red_dunant,
                               brc_colours$red_light))
                               

----#Detention Pregnant Women#----

detention_pregnant_women %>%
  select(Year, `Number of pregnant women detained in the immigration detention estate`) %>%
  group_by(Year) %>%
  summarise(Total = sum(`Number of pregnant women detained in the immigration detention estate`)) |>
  ggplot(aes(Year, Total)) +
  geom_line(aes(colour = brc_colours$red_dunant), show.legend = FALSE) +
  geom_point(aes(colour = brc_colours$red_dunant), alpha = 0.5, show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(3)) +
  theme_classic() +
  labs(title = "Number of pregnant women in immigration detention from 2016 to March 2023",
       x = "Year", 
       y = "Number of Pregnant People", 
       caption = "British Red Cross analysis of Home Office data, July 2016 to March, 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_x_continuous(breaks = c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) 


----#Children in Detention#----
ChildrenDetention <- people_in_detention %>%
  filter(Age == "17 and under") %>%
  group_by(Year) %>%
  summarise(Total = sum(People)) 
  

ChildrenDetention %>%
  ggplot(aes(Year, Total)) +
  geom_col(colour = brc_colours$red_dunant, fill = brc_colours$red_dunant)
  

#using children entering detention as a more accurate depiction rather than in detention as the data is not recorded as well? 
view(children_entering_detention)

children_entering_detention %>%
  ggplot(aes(`Date of entry to detention`, `Total children`)) +
  geom_col(aes(fill = brc_colours$red_dunant, colour = brc_colours$red_dunant), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(`Total children`)), show.legend = FALSE, size = rel(2), position = position_dodge(width=1), vjust=-0.25) +
  theme_classic() +
  labs(title = "Number of children entering immigration detention from 2010 to 2022",
       x = "Year",
       y = "Number of Children",
       caption = "British Red Cross analysis of Home office data from March 2010 to March 2022") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 600)) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) 


#Check with team about children IN and children entering detention and why these numbers are so different?!

----#Leaving Detention and Reason#---- 

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
  theme_classic() +
  labs(title = "Number of people leaving detention from 2010 to 2023",
       subtitle = str_wrap("Bailed includes bailed by immigration judge and Secretary of State. Other is defined as people returned to criminal detention, released unconditionally, those sectioned under the Mental Health Act, deaths, absconds and reasons unavailable when leaving detention"),
       x = "Year",
       y = "Number of People",
       caption = "British Red Cross analysis of Home Office data, March 2010 to March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 40000)) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) +
  scale_fill_manual(values = c(brc_colours$red_light,
                               brc_colours$red_mercer,
                               brc_colours$red_earth,
                               brc_colours$red_dunant))
  
----#Women in Detention#----
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
  theme_classic() +
  scale_fill_manual(values = c(brc_colours$red_deep,
                              brc_colours$red_earth,
                              brc_colours$red_dunant,
                              brc_colours$red_light,
                              brc_colours$red_mercer)) +
  labs(title = "Number of women in immigration detention from 2010 to 2023",
       x = "Year",
       y = "Number of Women",
       caption = "British Red Cross analysis of Home Office data, year ending March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 1500)) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023))