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
  geom_point(aes(size = Total, alpha = 0.4, colour = 'red'), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(4)) +
  theme_classic() +
  scale_x_continuous(breaks = c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title = "Total Number of Asylum Applications", 
       x = NULL, 
       y = "Applications", 
       caption = "British Red Cross Analyses 2023, Q1")

----#Top Nationalities#----

ApplicationbyNationality <- applications %>%
  select(Year, Nationality, Applications) %>%
  group_by(Year, Nationality) %>%
  summarise(Total = sum(Applications))

ApplicationbyNationality %>%
  filter(Total > 1000) %>%
  ggplot(aes(x = Year, y = Total, fill = Nationality), show.legend = FALSE) + 
  geom_area() +
  theme_classic() +
  labs (title = "Applications") +
  scale_x_continuous(breaks = c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=1))

#Have to adjust the colour- to discuss with Matt




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

AgeandSexPlot <- (AgeAnalysis %>%
  filter(Year > 2022) %>%
  ggplot(aes(fill = Age, x = Sex, y = AgeGroupSum)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  labs(title = "Asylum Applicants in 2022-2023 Q1 by Age and Sex", 
       x = "Sex", 
       y = "Applications", 
       caption = "British Red Cross Analyses of Home Office Data, March 2023"))

AgeandSexPlot + scale_fill_manual(values = c(brc_colours$red_deep,
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
  labs(title = "Unaccompanied Asylum Seeking Children (UASC) Applications", 
       x = NULL, 
       y = "Applications", 
       caption = "British Red Cross Analyses 2023, Q1")

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
  geom_line(colour = "red") +
  geom_text(aes(label = scales::comma(TotalDC)), show.legend = FALSE, size = rel(4)) +
  theme_classic() +
  labs(title = 'Asylum Applications of Depedant Children', 
       x = NULL, 
       y = 'Applications', 
       caption = 'British Red Cross Analyses 2023, Q1') +
  scale_x_continuous(breaks = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023))
