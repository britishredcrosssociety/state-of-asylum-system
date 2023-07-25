library(tidyverse)
library(asylum)
----#QUESTION: WHO IS APPLYING FOR ASYLUM IN THE UK (NATIONALITY, SEX, AGE, UASC, KIDS)#----
----#Total Asylum Applications#----

TotalApps <- applications %>%
  group_by(Year) %>%
  summarise(Total = sum(Applications))

view(TotalApps)

TotalApps |>
  ggplot(aes(Year, Total)) +
  geom_line(colour = "red") +
  geom_point(aes(size = Total, alpha = 0.5, colour = 'red'), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(2)) +
  theme_classic() +
  scale_x_continuous(breaks = c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 120000)) +
  labs(title = "Total number of asylum applications from 2001 to 2023", 
       x = NULL, 
       y = "Applications", 
       caption = "British Red Cross analysis of Home Office data from January,2001 to January,2023")

----#Top Nationalities#----

ApplicationbyNationality <- applications %>%
  select(Year, Nationality, Applications) %>%
  group_by(Year, Nationality) %>%
  summarise(Total = sum(Applications))

ApplicationbyNationality %>%
  filter(Total > 2000) %>%
  ggplot(aes(x = Year, y = Total, fill = Nationality), show.legend = FALSE) + 
  geom_area() +
  theme_classic() +
  labs (title = "Applications") +
  scale_x_continuous(breaks = c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=1)) +
  #scale_fill_manual(values = c(brc_colours$claret,
                               brc_colours$mustard,
                               brc_colours$sand,
                               brc_colours$earth,
                               brc_colours$grey,
                               brc_colours$grey_fog,
                               brc_colours$white_flag,
                               brc_colours$duck,
                               brc_colours$steel,
                               brc_colours$sky,
                               brc_colours$teal,
                               brc_colours$blue,
                               brc_colours$green_dark))

#Have to adjust the colour- to discuss with Matt
#Look at adjusting the filter to 2000/3000 rather than 1000. 


----#Age and Sex Analysis 2022/23#----
AgeAnalysis <- applications %>%
  select(Year, Age, Sex, Applications) %>%
  filter(Year > '2008')

view(AgeAnalysis)

AgeAnalysis <- AgeAnalysis %>%
  group_by(Age, Sex, Year) %>%
  summarise(AgeGroupSum = sum(Applications))

AgeAnalysis <- AgeAnalysis %>%
  filter(Sex != "Unknown Sex") %>%
  filter(Age != "Unknown")

#2022-2023 Sex and Age Plot 
AgeAnalysis$Age <- factor(AgeAnalysis$Age, levels=c('70+', '50-69', '30-49', '18-29', 'Under 18'))

AgeAnalysis %>%
  filter(Year > 2022) %>%
  ggplot(aes(fill = Age, x = Sex, y = AgeGroupSum)) +
  geom_bar(position = "stack", stat = "identity") +
  #geom_text(aes(label = AgeGroupSum), position = position_stack(vjust = -0.25),size = rel(2)) + 
  theme_classic() +
  labs(title = "Asylum applicants in January 2023 by age and sex", 
       x = NULL, 
       y = "Applications", 
       caption = "British Red Cross analysis of Home Office data, January 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 15000)) +
  scale_fill_manual(values = c(brc_colours$red_deep,
                                     brc_colours$red_light,
                                     brc_colours$red_dunant,
                                     brc_colours$red_mercer,
                                     brc_colours$red_earth))
----#UASC#----
UASC <- applications %>%
  select(Year, Nationality, UASC, Applications) %>%
  group_by(Year, UASC) %>%
  summarise(TotalUASC = sum(Applications))

view(UASC)

UASCOnly <- UASC %>%
  filter(Year > 2005, UASC == "UASC")

view(UASCOnly)

UASCOnly |>
  ggplot(aes(Year, TotalUASC)) +
  geom_line(colour = "red") +
  geom_text(aes(label = scales::comma(TotalUASC)), show.legend = FALSE, size = rel(4)) +
  theme_classic() +
  scale_x_continuous(breaks = c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  labs(title = "Number of asylum applications from Unaccompanied asylum-seeking children (UASC), 2006-2023", 
       x = NULL, 
       y = "Applications", 
       caption = "British Red Cross analysis of Home Office data, from January 2006 - January 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 6000))
  

----#Dependant Children#----
DependentC <- applications %>%
  select(Year, `Applicant type`, Age, Applications) %>%
  group_by(Year, `Applicant type`, Age) %>%
  summarise(TotalDC = sum(Applications))

DependentC <- DependentC %>%
  filter(`Applicant type` == "Dependant", Age == "Under 18")

view(DependentC)

DependentC |>
  ggplot(aes(Year, TotalDC)) +
  geom_point(aes(size = TotalDC, colour = brc_colours$red_dunant, alpha = 0.5), show.legend = FALSE) +
  geom_line(colour = brc_colours$red_dunant) +
  geom_text(aes(label = scales::comma(TotalDC)), show.legend = FALSE, size = rel(3)) +
  theme_classic() +
  labs(title = 'Number of asylum applications of dependent children from 2009 to 2023', 
       x = NULL, 
       y = 'Applications', 
       caption = 'British Red Cross analysis of Home Office data January 2009 - January 2023') +
  scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 10000))
