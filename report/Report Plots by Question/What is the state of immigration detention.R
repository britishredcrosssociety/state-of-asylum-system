----#QUESTION: WHAT IS THE STATE OF DETENTION?

#People in Detention#
OverallinDetention <- people_in_detention %>%
  select(Year, Age, Sex, `Length of detention`, People) %>%
  group_by(Year) %>%
  summarise(TotalinDetention = sum(People))

OverallinDetention |>
  ggplot(aes(Year, TotalinDetention)) +
  geom_line(aes(colour = "red"), show.legend = FALSE) +
  geom_point(aes(colour = "red", alpha = 0.5, size = TotalinDetention), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(TotalinDetention)), show.legend = FALSE, size = rel(2)) +
  theme_classic() +
  labs(title = "Number of people in detention from March 2010 - March 2023",
       x = NULL,
       y = "Number of People", 
       caption = "British Red Cross Analysis of Home Office data, year ending March 2023") +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 15000))


#Length of Time in Detention#
LengthinDetention <- people_in_detention %>%
  group_by(Year, `Length of detention`) %>%
  summarise(Total = sum(People))

library(plyr)

LengthinDetention$`Length of detention` <- revalue(LengthinDetention$`Length of detention`, 
                                 c("A: 3 days or less" = "7 days or less",
                                   "B: 4 to 7 days" = "7 days or less", 
                                   "C: 8 to 14 days" = "8 days to 14 days",
                                   "D: 15 to 28 days" = "15 days to 28 days",
                                   "E: 29 days to less than 2 months" = "29 days to 3 months",
                                   "F: 2 months to less than 3 months" = "29 days to 3 months",
                                   "G: 3 months to less than 4 months" = "3 months to 6 months",
                                   "H: 4 months to less than 6 months" = "3 months to 6 months",
                                   "I: 6 months to less than 12 months" = "6 months to 12 months",
                                   "J: 12 months to less than 18 months" = "1 year to 2 years",
                                   "K: 18 months to less than 24 months" = "1 year to 2 years", 
                                   "L: 24 months to less than 36 months" = "2 years to 4 years",
                                   "M: 36 months to less than 48 months" = "2 years to 4 years", 
                                   "N: 48 months or more" = "4 years +"))

LengthinDetention$`Length of detention` <- factor(LengthinDetention$`Length of detention`, levels=c('4 years +',
                                                                                                    '2 years to 4 years',
                                                                                                    '1 year to 2 years',
                                                                                                    '6 months to 12 months',
                                                                                                    '3 months to 6 months',
                                                                                                    '29 days to 3 months',
                                                                                                    '15 days to 28 days',
                                                                                                    '8 days to 14 days',
                                                                                                    '7 days or less',
                                                                                                    'Not available'))
LengthinDetention %>%
  group_by(`Length of detention`) %>%
  filter(`Length of detention` != "Not available") %>%
  ggplot(aes(fill =`Length of detention`, x = Year, y = LengthTotal)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  labs(title = "Length of time people have spent in immigration detention from 2010 - 2023",
       x = NULL,
       y = "Number of People",
       caption = "British Red Cross analysis of Home Office data, year ending September 2022 as further data labelled 'Not Available'") +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 15000)) +
  scale_fill_manual(values = c(brc_colours$black_shadow,
                               brc_colours$red_deep,
                               brc_colours$red_earth,
                               brc_colours$red_mercer,
                               brc_colours$red_light,
                               brc_colours$red_dunant,
                               brc_colours$teal,
                               brc_colours$sky,
                               brc_colours$steel))

#Is there a reason why they have not recorded how long individuals are held in detention for 2023 Q1?#


#Detention: Age and Sex# 

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
  filter(Year > 2021) %>%
  ggplot(aes(fill = Age, Sex, AgeTotal)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  labs(title = "Age and sex of people in immigration detention from 2022 - 2023",
       x = NULL,
       y = "Number of People",
       caption = "British Red Cross analysis of Home Office data, year ending March, 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_fill_manual(values = c(brc_colours$red_deep,
                               brc_colours$red_earth,
                               brc_colours$red_mercer,
                               brc_colours$red_light,
                               brc_colours$red_dunant))
                               

#Detention Pregnant Women#

detention_pregnant_women %>%
  select(Year, `Number of pregnant women detained in the immigration detention estate`) %>%
  group_by(Year) %>%
  summarise(Total = sum(`Number of pregnant women detained in the immigration detention estate`)) |>
  ggplot(aes(Year, Total)) +
  geom_line(aes(colour = brc_colours$red_dunant), show.legend = FALSE) +
  geom_point(aes(colour = brc_colours$red_dunant, size = Total), alpha = 0.5, show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(4)) +
  theme_classic() +
  labs(title = "Number of pregnant women in detention from July 2016 to January 2023",
       x = NULL, 
       y = "Number of Pregnant Women", 
       caption = "British Red Cross analysis of Home Office data, year ending March, 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_x_continuous(breaks = c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) 


#Children in Detention#
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
  geom_text(aes(label = scales::comma(`Total children`)), show.legend = FALSE, size = rel(4), vjust = 0.5) +
  theme_classic() +
  labs(title = "Number of children entering detention from 2010 to 2022",
       x = NULL,
       y = "Number of Children",
       caption = "British Red Cross analysis of Home office data") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 600)) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) 


#Check with team about children IN and children entering detention and why these numbers are so different?!

#Leaving Detention and Reason# 

people_leaving_detention %>%
  group_by(Year, `Reason for leaving detention`) %>%
  summarise(Total = sum(Leaving)) %>%
  ggplot(aes(fill = `Reason for leaving detention`, x = Year, y = Total)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  labs(title = "Number of people leaving detention from 2010 to 2023",
       subtitle = "Bailed (IJ) is defined as bailed (immigration judge), Bailed (SoS) is defined as bailed (secretary of state) and other as defined as people returned to criminal detention, those released unconditionally, those sectioned under the Mental Health Act, deaths, absconds and reasons unavailable when leaving detention",
       x = NULL,
       y = "Number of People",
       caption = "British Red Cross analysis of Home Office data, year ending March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 40000)) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) +
  scale_fill_manual(values = c(brc_colours$red_deep,
                               brc_colours$red_mercer,
                               brc_colours$red_earth,
                               brc_colours$red_dunant,
                               brc_colours$red_light))
  
#Women in Detention#
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
  labs(title = "Number of women in detention from 2010 to 2023",
       x = NULL,
       y = "Number of Women",
       caption = "British Red Cross analysis of Home Office data, year ending March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 1500)) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023))
  


