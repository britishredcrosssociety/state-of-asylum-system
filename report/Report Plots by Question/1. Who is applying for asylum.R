library(tidyverse)
library(asylum)
library(zoo)
source("report/brc_colours.R")
source("report/theme_brc.R")
source("functions/rolling-annual-sum.R")

# ---- QUESTION: WHO IS APPLYING FOR ASYLUM IN THE UK (NATIONALITY, SEX, AGE, UASC, KIDS) ----

# ---- 1. Total Asylum Applications ----
total_applications <- applications |>
  group_by(Year) |>
  summarise(Total = sum(Applications))

total_applications |>
  ggplot(aes(Year, Total)) +
  geom_line(colour = brc_colours$red_dunant) +
  geom_point(aes(size = Total, alpha = 0.5, colour = brc_colours$red_dunant), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(2)) +
  #geom_vline(xintercept = c(2002, 2022), size = 0.3, linetype = "dotted") +
  #annotate("text", x = 2002, y = 110000, label = "Conflicts in Afghanistan, Iran, Somalia and Sri Lanka", size = 2.5, hjust = -.01) +
  #annotate("text", x = 2022, y = 100000, label = "Conflicts in Ukraine and Afghanistan", size = 2.5) +
  theme_brc() +
  scale_x_continuous(breaks = c(2001:2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 120000), expand = c(0, NA)) +
  labs(title = "Total number of asylum applications from 2001 to 2023", 
       x = "Year", 
       y = "Applications", 
       caption = "British Red Cross analysis of Home Office data, March 2001 to June 2023") 

# ---- 1a. Total applications Q2 to Q2 ----
  
  # Prepare asylum applications data by calculating quarterly total numbers of applications
  applications_annual <- 
    asylum::applications |> 
    group_by(Date) |> 
    summarise(Applications = sum(Applications, na.rm = TRUE))
  
  # Use the `rolling_annual_sum()` function to calculate the total number of applications
  # for the year ending June 2023, June 2022, June 2021 etc.
  applications_year_ending_most_recent_quarter <- 
    applications_annual |> 
    rolling_annual_sum(Applications)
  
  View(applications_year_ending_most_recent_quarter)
  
applications_year_ending_most_recent_quarter |>
  ggplot(aes(Date, RollingSum)) +
  geom_line(colour = brc_colours$red_dunant) +
  geom_point(aes(size = RollingSum, alpha = 0.5, colour = brc_colours$red_dunant), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(RollingSum)), show.legend = FALSE, size = rel(2)) +
  #geom_vline(xintercept = c(2002, 2022), size = 0.3, linetype = "dotted") +
  #annotate("text", x = 2002, y = 110000, label = "Conflicts in Afghanistan, Iran, Somalia and Sri Lanka", size = 2.5, hjust = -.01) +
  #annotate("text", x = 2022, y = 100000, label = "Conflicts in Ukraine and Afghanistan", size = 2.5) +
  theme_brc() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 100000), expand = c(0, NA)) +
  labs(title = "Total number of asylum applications from 2002 to 2023", 
       x = "Year", 
       y = "Applications", 
       caption = "British Red Cross analysis of Home Office data, June 2002 to June 2023")

# ---- 2. Age and Sex Analysis 2023 Q2 Only ----
age_and_sex_analysis <- applications |>
  select(Date, Year, Age, Sex, Applications) |>
  filter(Year > '2008')

view(age_and_sex_analysis)

age_and_sex_analysis <- age_and_sex_analysis |>
  group_by(Date, Age, Sex, Year) |>
  summarise(Total = sum(Applications)) |>
  ungroup()
  
age_and_sex_analysis <- age_and_sex_analysis |>
  filter(Sex != "Unknown Sex") |>
  filter(Age != "Unknown") |>
  filter(Age != "NA")

# Adjusting the order of age
age_and_sex_analysis$Age <- factor(age_and_sex_analysis$Age, levels=c('70+', '50-69', '30-49', '18-29', 'Under 18'))

age_and_sex_analysis |>
  filter(Date >= max(Date) -dmonths(11)) |> 
  ggplot(aes(fill = Age, x = Sex, y = Total)) +
  geom_bar(position = "stack", stat = "identity") +
  #geom_text(aes(label = Total), position = position_stack(vjust = -0.25),size = rel(2)) + 
  theme_brc() +
  labs(title = "Number of asylum applicants by age and sex over last 12 months ", 
       x = "Sex", 
       y = "Applications", 
       caption = "British Red Cross analysis of Home Office data, March 2022 to June 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 80000), expand = c(0,NA)) +
  scale_fill_manual(values = c(brc_colours$red_deep,
                               brc_colours$red_mercer,
                               brc_colours$red_light,
                               brc_colours$sky,
                               brc_colours$steel,
                               brc_colours$teal))

# ---- 3. Dependent Children ----
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
  labs(title = "Number of asylum applications with dependent children from 2009 to 2023", 
       x = "Year", 
       y = 'Applications', 
       caption = 'British Red Cross analysis of Home Office data, March 2009 to June 2023') +
  scale_x_continuous(breaks = c(2009:2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 15000), expand = c(0, NA))

# ---- 3b. Dependent Children Q2 to Q2 ----

# Prepare asylum applications data by calculating quarterly total numbers of applications
dependentchildren_annual <- 
  asylum::applications |> 
  filter(`Applicant type` == "Dependant", Age == "Under 18") |>
  group_by(Date) |> 
  summarise(Total = sum(Applications, na.rm = TRUE))

# Use the `rolling_annual_sum()` function to calculate the total number of applications
# for the year ending June 2023, June 2022, June 2021 etc.
dependent_children_most_recent_Q <- 
  dependentchildren_annual |> 
  rolling_annual_sum(Total)

View(dependent_children_most_recent_Q)

dependent_children_most_recent_Q |>
  ggplot(aes(Date, RollingSum)) +
  geom_point(aes(size = RollingSum, colour = brc_colours$red_dunant, alpha = 0.5), show.legend = FALSE) +
  geom_line(colour = brc_colours$red_dunant) +
  geom_text(aes(label = scales::comma(RollingSum)), show.legend = FALSE, size = rel(3)) +
  theme_brc() +
  labs(title = "Number of asylum applications with dependent children from 2010 to 2023", 
       x = "Year", 
       y = 'Applications', 
       caption = 'British Red Cross analysis of Home Office data, June 2010 to June 2023') +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 15000), expand = c(0, NA))
  

# ---- 4. UASC ----
UASC <- applications |>
  select(Date, Year, Nationality, UASC, Applications) |>
  group_by(Date, Year, UASC, Nationality) |>
  summarise(Total = sum(Applications))

view(UASC)

# ---- UASC Total Applications ----
UASCOnly <- UASC |>
  filter(Year > 2005, UASC == "UASC") |>
  group_by(Year) |>
  summarise(UASCTotal = sum(Total))

UASCOnly |>
  ggplot(aes(x = Year, y = UASCTotal)) +
  geom_line(aes(colour = brc_colours$red_dunant), show.legend = FALSE) +
  geom_point(colour = brc_colours$red_dunant, alpha = 0.5, show.legend = FALSE) +
  geom_text(aes(label = scales::comma(UASCTotal)), show.legend = FALSE, size = rel(3)) +
  theme_brc() +
  scale_x_continuous(breaks = c(2006:2023)) +
  labs(title = "Applications for asylum by unaccompanied asylum seeking children (UASC) from 2006-2023", 
       x = "Year", 
       y = "Applications", 
       caption = "British Red Cross analysis of Home Office data, March 2006 to June 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 7000), expand = c(0, NA))

# ---- 4b. UASC Q2 to Q2 ----

# Prepare asylum applications data by calculating quarterly total numbers of applications
UASC_annual <- 
  asylum::applications |> 
  select(Date, Year, UASC, Applications) |>
  filter(UASC == "UASC") |>
  group_by(Date) |> 
  summarise(Total = sum(Applications, na.rm = TRUE))

# Use the `rolling_annual_sum()` function to calculate the total number of applications
# for the year ending June 2023, June 2022, June 2021 etc.
UASC_most_recent_Q <- 
  UASC_annual |> 
  rolling_annual_sum(Total)

View(UASC_most_recent_Q)

UASC_most_recent_Q |>
  ggplot(aes(x = Date, y = RollingSum)) +
  geom_line(aes(colour = brc_colours$red_dunant), show.legend = FALSE) +
  geom_point(colour = brc_colours$red_dunant, alpha = 0.5, show.legend = FALSE) +
  geom_text(aes(label = scales::comma(RollingSum)), show.legend = FALSE, size = rel(3)) +
  theme_brc() +
  labs(title = "Applications for asylum by unaccompanied asylum seeking children (UASC) from 2007-2023", 
       x = "Year", 
       y = "Applications", 
       caption = "British Red Cross analysis of Home Office data, June 2007 to June 2023") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 6000), expand = c(0, NA))


# ---- 5. UASC Nationality ----
UASC <- UASC |>
  filter(UASC == "UASC") |>
  group_by(Date, Nationality)


UASC <- UASC |>
  filter(Date >= max(Date) - dmonths(11)) |>
  group_by(Nationality) |>
  summarise(TotalUASC = sum(Total))


UASC |>
  filter(TotalUASC > 950) |>
  ggplot(aes(x = reorder(Nationality, desc(TotalUASC), sum), y = TotalUASC)) +
  geom_col(fill = brc_colours$red_dunant) +
  geom_text(aes(label = scales::comma(TotalUASC)), show.legend = FALSE, size = rel(3),  position = position_dodge(width=1), vjust=-0.25, colour = brc_colours$black_shadow) +
  theme_brc() +
  labs(title = "Number of asylum applications of unaccompanied asylum seeking children for year ending June 2023",
       subtitle =  "Top 10 nationalities with the highest applications",
       x = "Nationality", 
       y = 'Number of Children', 
       caption = 'British Red Cross analysis of Home Office data, March 2022 to June 2023') +
  scale_y_continuous(labels = scales::comma, limits = c(0, 15000), expand = c(0,NA)) +
  theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5))


# ---- 6. Top 5 Nationalities Each Year ----
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
       subtitle = "Top 5 nationalities with the highest number of asylum applications to the United Kingdom each year",
       x = "Year", 
       y = "Number of asylum applications", 
       caption = "British Red Cross analysis of Home Office data, March 2001 to June 2023") +
  scale_x_continuous(breaks = c(2001 : 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 60000), expand = c(0, NA)) +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1)) +
  scale_fill_manual(values = c(brc_colours$red_dunant,
                               brc_colours$red_deep,
                               brc_colours$red_mercer,
                               brc_colours$red_light,
                               brc_colours$red_earth,
                               brc_colours$earth,
                               brc_colours$grey,
                               brc_colours$grey_fog,
                               brc_colours$green,
                               brc_colours$green_dark,
                               brc_colours$blue,
                               brc_colours$teal,
                               brc_colours$sky,
                               brc_colours$steel,
                               brc_colours$duck,
                               brc_colours$sand,
                               brc_colours$black_full
  )) 