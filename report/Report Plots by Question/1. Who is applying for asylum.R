library(tidyverse)
library(asylum)
source("report/brc_colours.R")
source("report/theme_brc.R")

# ---- QUESTION: WHO IS APPLYING FOR ASYLUM IN THE UK (NATIONALITY, SEX, AGE, UASC, KIDS) ----

# ---- Total Asylum Applications ----
total_applications <- applications |>
  group_by(Year) |>
  summarise(Total = sum(Applications))

view(total_applications)

total_applications |>
  ggplot(aes(Year, Total)) +
  geom_line(colour = brc_colours$red_dunant) +
  geom_point(aes(size = Total, alpha = 0.5, colour = brc_colours$red_dunant), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(2)) +
  theme_brc() +
  scale_x_continuous(breaks = c(2001:2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 120000), expand = c(0, NA)) +
  labs(title = "Total number of asylum applications from 2001 to 2023", 
       x = "Year", 
       y = "Applications", 
       caption = "British Red Cross analysis of Home Office data, March 2001 to March 2023")

# ---- Top 5 Nationalities Each Year ----
application_by_nationality <- applications |>
  select(Year, Nationality, Applications) |>
  group_by(Year, Nationality) |>
  summarise(Total = sum(Applications))

application_by_nationality |>
  slice_max(Total, n = 5) |>
  ggplot(aes(x = Year, y = Total, fill = Nationality), show.legend = FALSE) + 
  geom_bar(stat = "identity") +
  theme_brc() +
  labs(title = "Number of asylum applications to the United Kingdom by nationality from 2001 to 2023",
        subtitle = "Top 10 nationalities with the highest number of asylum applications to the United Kingdom",
        x = "Year", 
        y = "Number of asylum applications", 
        caption = "British Red Cross analysis of Home Office data, March 2001 to March 2023") +
  scale_x_continuous(breaks = c(2001 : 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA), expand = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values = c(brc_colours$red_light,
                               brc_colours$red_dunant,
                               brc_colours$red_earth,
                               brc_colours$red_mercer,
                               brc_colours$red_deep,
                               brc_colours$claret,
                               brc_colours$mustard,
                               brc_colours$sand,
                               brc_colours$grey_fog,
                               brc_colours$grey,
                               brc_colours$duck,
                               brc_colours$steel,
                               brc_colours$sky,
                               brc_colours$teal,
                               brc_colours$blue,
                               brc_colours$green,
                               brc_colours$green_dark)) 
  


# ---- Age and Sex Analysis 2022/23 ----
age_and_sex_analysis <- applications |>
  select(Year, Age, Sex, Applications) |>
  filter(Year > '2008')

view(age_and_sex_analysis)

age_and_sex_analysis <- age_and_sex_analysis |>
  group_by(Age, Sex, Year) |>
  summarise(Total = sum(Applications))

age_and_sex_analysis <- age_and_sex_analysis |>
  filter(Sex != "Unknown Sex") |>
  filter(Age != "Unknown")

# Adjusting the order of age
age_and_sex_analysis$Age <- factor(age_and_sex_analysis$Age, levels=c('70+', '50-69', '30-49', '18-29', 'Under 18'))

age_and_sex_analysis |>
  filter(Year > 2022) |>
  ggplot(aes(fill = Age, x = Sex, y = Total)) +
  geom_bar(position = "stack", stat = "identity") +
  #geom_text(aes(label = Total), position = position_stack(vjust = -0.25),size = rel(2)) + 
  theme_brc() +
  labs(title = "Number of asylum applicants by age and sex for year ending March 2023 ", 
       x = "Sex", 
       y = "Applications", 
       caption = "British Red Cross analysis of Home Office data, March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 15000), expand = c(0,NA)) +
  scale_fill_manual(values = c(brc_colours$red_deep,
                                     brc_colours$red_light,
                                     brc_colours$red_dunant,
                                     brc_colours$red_mercer,
                                     brc_colours$red_earth))

# ---- UASC ----
UASC <- applications |>
  select(Date, Year, Nationality, UASC, Applications) |>
  group_by(Date, Year, UASC, Nationality) |>
  summarise(Total = sum(Applications))

view(UASC)

# ---- UASC Nationality ----
UASC22 <- UASC |>
  filter(Year > 2021, UASC == "UASC") |>
  group_by(Year, Nationality)

UASC22$Nationality <- factor(UASC22$Nationality, levels = UASC22$Nationality[order(UASC22$Total, decreasing = TRUE)])

UASC22 |>
  group_by(Nationality) |>
  summarise(Total = sum(Total)) |>
  filter(Total > 120) |>
  ggplot(aes(x = reorder(Nationality, desc(Total), sum), y = Total)) +
  geom_col(fill = brc_colours$red_dunant) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(3),  position = position_dodge(width=1), vjust=-0.25, colour = brc_colours$black_shadow) +
  theme_brc() +
  labs(title = "Number of asylum applications of Unaccompanied asylum seeking children for year ending March 2023",
       subtitle =  "Top 10 nationalities with the highest applications",
       x = "Country", 
       y = 'Number of Children', 
       caption = 'British Red Cross analysis of Home Office data, March 2022 to March 2023') +
  scale_y_continuous(labels = scales::comma, limits = c(0, 2000), expand = c(0,NA)) +
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5))


# ---- UASC Total Applications ----
UASCOnly <- UASC |>
  filter(Year > 2005, UASC == "UASC") |>
  group_by(Year) |>
  summarise(UASCTotal = sum(Total))

UASCOnly |>
  ggplot(aes(x = Year, y = UASCTotal)) +
  geom_line(aes(colour = brc_colours$red_dunant), show.legend = FALSE) +
  geom_point(colour = brc_colours$red_dunant, alpha = 0.5, show.legend = FALSE) +
  geom_text(aes(label = scales::comma(UASCTotal)), show.legend = FALSE, size = rel(4)) +
  theme_brc() +
  scale_x_continuous(breaks = c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  labs(title = "Applications for asylum by unaccompanied asylum-seeking children (UASC), 2006-2023", 
       x = "Year", 
       y = "Applications", 
       caption = "British Red Cross analysis of Home Office data, March 2006 to March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 6000), expand = c(0, NA))
  

# ---- Dependant Children ----
dependent_children <- applications |>
  select(Year, `Applicant type`, Age, Applications) |>
  group_by(Year, `Applicant type`, Age) |>
  summarise(Total = sum(Applications))

dependent_children <- dependent_children |>
  filter(`Applicant type` == "Dependant", Age == "Under 18")

view(DependentC)

dependent_children |>
  ggplot(aes(Year, Total)) +
  geom_point(aes(size = Total, colour = brc_colours$red_dunant, alpha = 0.5), show.legend = FALSE) +
  geom_line(colour = brc_colours$red_dunant) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(3)) +
  theme_brc() +
  labs(title = "Number of asylum applications by dependent children from 2009 to 2023", 
       x = "Year", 
       y = 'Applications', 
       caption = 'British Red Cross analysis of Home Office data, March 2009 to March 2023') +
  scale_x_continuous(breaks = c(2009:2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 10000), expand = c(0, NA))
