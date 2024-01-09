# Install our {compositr} package if you haven't already
devtools::install_github("humaniverse/compositr")

library(tidyverse)
library(asylum)
library(compositr)
library(readxl)
library(Hmisc)
library(zoo)

# Rolling Sum Annual Function #

#' Function to calculate the rolling year-ending sum of a variable in a data.frame 
#' based on the most recently available quarter in the data.
#' 
#' @param df The data.frame containing your data
#' @param variable The name of the column you want to calculate a rolling annual sum for
#' 
rolling_annual_sum <- function(df, variable) {
  # Calculate the first quarter that needs to be in the data, based on the most recent quarter
  # e.g. if the most recent data is for Q2 in the maximum year, then the first entry should be for Q3 in the minimum year
  first_quarter_to_filter <- quarter(max(df$Date)) + 1
  first_year_in_data <- year(min(df$Date))
  first_date_to_filter <- as.Date(as.yearqtr(paste(first_year_in_data, first_quarter_to_filter), format = "%Y %q"), frac = 1)
  
  df |> 
    filter(Date >= first_date_to_filter) |> 
    arrange(Date) |> 
    mutate(RollingSum = rollapplyr({{ variable }}, width = 4, FUN = sum, fill = NA)) |> 
    filter(row_number() %% 4 == 0) |> 
    select(Date, RollingSum)
}

# ---- Immigration to the UK ----
# People arriving on small boats who claimed asylum
migration_small_boats <- 
  asylum::small_boat_asylum_applications |> 
  filter(Year == 2022 & `Asylum application` == "Asylum application raised") |> 
  summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
  pull(Applications)

# Migration stats from ONS
# Data: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/internationalmigration/datasets/longterminternationalimmigrationemigrationandnetmigrationflowsprovisional
tf <- download_file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/internationalmigration/datasets/longterminternationalimmigrationemigrationandnetmigrationflowsprovisional/yearendingdecember2022/longterminternationalmigrationprovisional2018to2022.xlsx", ".xlsx")

# excel_sheets(tf)  # Check worksheets in the ONS file

immigration_non_EU <- read_excel(tf, sheet = "2. Non-EU reason immigration", skip = 3)

immigration_non_EU <- 
  immigration_non_EU |> 
  filter(Period == "YE Dec 2022 P") |> 
  mutate(`Other migration (non-EU)` = `Total Work` + `Total Study` + Family + Other) |> 
  mutate(BNO = as.integer(BNO)) |> 
  select(
    `Other migration (non-EU)`, 
    `British nationals (overseas)` = BNO,
    `Ukraine visas` = Ukraine,
    `Resettlement and other safe routes` = Resettlement,
    Asylum
  )

immigration_British_EU <- read_excel(tf, sheet = "4. British and EU nat by reason", skip = 2)

immigration_British_EU <- 
  immigration_British_EU |> 
  filter(Period == "YE Dec 2022 P") |> 
  summarise(`British and EU nationals` = sum(`All Reasons`))

immigration <- 
  immigration_non_EU |> 
  bind_cols(immigration_British_EU) |> 
  mutate(
    `Other immigration` = `Other migration (non-EU)` + `British and EU nationals`,
    Asylum 
    #`Small boat arrivals claiming asylum` = migration_small_boats,
    #`Asylum claims (not via small boats)` = Asylum - migration_small_boats
  )

immigration <- 
  immigration |> 
  select(`Other immigration`, `Ukraine visas`, `British nationals (overseas)`, `Asylum` , `Resettlement and other safe routes`) |> 
  pivot_longer(cols = everything(), names_to = "Migration type", values_to = "Number of people") |> 
  arrange(desc(`Number of people`)) |> 
  
  # Calculate proportions
  mutate(`Percentage of people immigrating` = `Number of people` / sum(`Number of people`)) |> 
  mutate(`Percentage of people immigrating` = scales::percent(`Percentage of people immigrating`, accuracy = 0.1))

# Add definitions
immigration <- 
  immigration |> 
  mutate(Definition = case_match(
    `Migration type`,
    "Resettlement and other safe routes" ~ "Resettlement is the selection and transfer of refugees from a country in which they have sought protection to a third country that has agreed to admit them as refugees with permanent settlement status. (For example, the resettlement of Afghans from Pakistan to the UK.) This route is operated and facilitated by UNHCR.",
    "British nationals (overseas)" ~ "A visa to settle in the UK for people from Hong Kong who are British national (overseas) citizens (a type of British nationality) and their families.",
    .default = ""
  ))

immigration |> 
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/immigration.csv")

# - CAPTION -
immigration |> 
  mutate(`Sub-category` = case_match(
    `Migration type`,
    c("Asylum claims (not via small boats)", "Small boat arrivals claiming asylum") ~ "Asylum",
    .default = "Other"
  )) |> 
  
  group_by(`Sub-category`) |> 
  summarise(Total = sum(`Number of people`)) |> 
  ungroup() |> 
  mutate(Proportion = scales::percent(Total / sum(Total)))

# ---- Updated for Sep 2023 ----

# Please add Date of data release to file saved to keep track of the differences in data between each upload. 


# ---- Total number of people applying for asylum each year ----
asylum::applications |> 
  group_by(Year) |> 
  summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/applications - total.csv")

# ---- Total number of people applying for asylum year ending September ----

applications_annual <- 
  asylum::applications |> 
  group_by(Date) |> 
  filter(`Applicant type` == "Main applicant") |>
  summarise(Applications = sum(Applications, na.rm = TRUE))

# Use the `rolling_annual_sum()` function to calculate the total number of applications
# for the year ending September 2023, 2022 etc.
applications_year_ending_most_recent_quarter <- 
  applications_annual |> 
  rolling_annual_sum(Applications) |> 
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/applications - total rolling sum Sept 23.csv")


# - CAPTION -
# Number of people applying for asylum so far this year
asylum::applications |> 
  filter(Year == max(Year)) |> 
  summarise(Applications = sum(Applications, na.rm = TRUE))

# ---- Asylum applications over time, by nationality without 2023 ----
asylum::applications |> 
  filter(Year != "2023") |>
  group_by(Year, Nationality) |> 
  summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
  ungroup() |> 
  
  pivot_wider(names_from = Nationality, values_from = Applications) |> 
  mutate(across(-(Year), as.character)) |> 
  mutate(across(-(Year), ~ replace_na(.x, ""))) |> 
  
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/applications - by nation wo 2023.csv")

asylum::applications |>
  filter(Year == "2023") |>
  group_by(Nationality) |>
  summarise(People = sum(Applications, na.rm = TRUE)) |>
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/applications - by nation only 2023.csv")


# - CAPTION -
# Which nationalities are consistently in the top 5% of applications?
num_years <- max(asylum::applications$Year) - min(asylum::applications$Year)

asylum::applications |> 
  group_by(Year, Nationality) |> 
  summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
  ungroup() |> 
  
  group_by(Year) |> 
  mutate(Applications_rank = rank(Applications)) |> 
  mutate(Vigintile = as.integer(Hmisc::cut2(Applications_rank, g = 20))) |> 
  filter(Vigintile == max(Vigintile)) |> 
  ungroup() |> 
  
  count(Nationality, sort = TRUE) |> 
  mutate(proportion_of_years = n / num_years) |> 
  filter(proportion_of_years > 0.8)

# ---- Applications for asylum over the last 12 months ----
applications_nationality <- 
  asylum::applications |> 
  # Filter applications within the last 12 months
  filter(Date >= max(Date) - dmonths(11)) |> 
  group_by(Nationality) |> 
  summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
  ungroup() |> 
  slice_max(Applications, n = 10) |> 
  rename(Category = Nationality, Nationality = Applications) |> 
  mutate(Type = "Nationality") 

applications_age <- 
  asylum::applications |> 
  # Filter applications within the last 12 months
  filter(Date >= max(Date) - dmonths(11)) |> 
  group_by(Age) |> 
  summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
  filter(Age != "Unknown") |> 
  rename(Category = Age, Age = Applications) |> 
  arrange(match(Category, c("Under 18", "18-29", "30-49", "50-69", "70+"))) |> 
  mutate(Type = "Age")

applications_sex <- 
  asylum::applications |> 
  # Filter applications within the last 12 months
  filter(Date >= max(Date) - dmonths(11)) |> 
  group_by(Sex) |> 
  summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
  filter(Sex != "Unknown Sex") |> 
  rename(Category = Sex, Sex = Applications) |> 
  mutate(Type = "Sex")

applications_uasc <- 
  asylum::applications |> 
  # Filter applications within the last 12 months
  filter(Date >= max(Date) - dmonths(11)) |> 
  #filter(`Applicant type`== "Dependant") |>
  filter(Age == "Under 18") |>
  group_by(UASC) |> 
  summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
  rename(Category = UASC, Children = Applications) |> 
  mutate(Category = case_when(
    Category == "UASC" ~ "Unaccompanied children",
    Category == "Non-UASC" ~ "Accompanied children",
  )) |>
  mutate(Type = "Children seeking asylum") 

applications_UASC_updated <- 
  bind_rows(
  #applications_non_uasc,
  applications_uasc
)

# Combine into a single dataframe and save
bind_rows(
  applications_nationality,
  applications_age,
  applications_sex,
  applications_uasc,
  
) |> 
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/applications - by category Sept 23.csv")

# - CAPTION - 
# Percentage of applications by age group
applications_age |>
  mutate(Percent = scales::percent(Age / sum(Age)))

# Number of applications from unaccompanied children
applications_uasc |> 
  filter(Category == "Unaccompanied children") |> 
  pull(Children)

# ---- Top five nationalities applying for asylum over time ----
asylum::applications |> 
  group_by(Quarter, Region, Nationality) |> 
  summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
  ungroup() |> 
  
  mutate(Applications = as.character(Applications)) |> 
  
  pivot_wider(names_from = Quarter, values_from = Applications) |> 
  mutate(across(-(Region:Nationality), ~ replace_na(.x, ""))) |> 
  
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/applications - top five nations Sept 2023.csv")

# ---- Change in initial grant rates ----
# Top ten nations, by number of grants and grant rate in the most recent year
top_ten_nations <-
  grant_rates_initial_annual |>
  filter(Year == max(Year)) |>
  arrange(desc(Grant), desc(`Initial grant rate`)) |> view()
  slice(1:10) |>
  pull(Nationality) 

grant_rates_initial_annual |>
  filter(Year >= max(Year) - 1) |>
  filter(Nationality %in% top_ten_nations) |>
  select(Nationality, Year, `Initial grant rate`, `Number of grants` = Grant) |>
  arrange(desc(`Initial grant rate`)) |> 
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/initial-grant-rates-annual-recent Sep 23.csv")

# ---- Initial grant rates, by quarter ----
grant_rates_initial_quarterly |>
  
  mutate(
    Grant = replace_na(Grant, 0),
    Refused = replace_na(Refused, 0)
  ) |> 
  mutate(`Initial grant rate` = Grant / (Grant + Refused)) |> 
  
  select(Date, Quarter, Nationality, `Initial grant rate`) |>
  pivot_wider(names_from = Nationality, values_from = `Initial grant rate`) |>

  # Move the ten nations with the highest number of grants and highest grant rates to the left, so they get shown on the chart by default
  relocate(Date, Quarter, any_of(top_ten_nations)) |>

  # Remove columns that contain only NAs
  select(where(~!all(is.na(.x)))) |>

  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/initial grant rates - by quarter Sep 2023.csv")

 # ---- Asylum-related and non asylum-related returns ----
asylum::returns_asylum |> 
  relocate(`Voluntary returns`, .after = Nationality) |>  # Reorder so voluntary returns comes first in the stacked bars
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/returns - by asylum Sept 23.csv")

asylum::returns_asylum |> 
  filter(Category == "Asylum-related") |> 
  relocate(`Voluntary returns`, .after = Nationality) |>  # Reorder so voluntary returns comes first in the stacked bars
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/returns - by asylum only Sept 23.csv")

# - CAPTION -
# What % of all returns were asylum-related?
asylum::returns_asylum |>
  mutate(Total = `Enforced returns` + `Voluntary returns` + `Refused entry at port and subsequently departed`) |> 
  
  group_by(Category) |> 
  summarise(Total = sum(Total)) |> 
  ungroup() |> 
  
  mutate(prop = scales::percent(Total / sum(Total)))

# How many returns in total?
asylum::returns_asylum |>
  mutate(Total = `Enforced returns` + `Voluntary returns` + `Refused entry at port and subsequently departed`) |> 
  summarise(sum(Total))

# ---- Inadmissibility cases ----
inadmissibility_cases_considered$Stage <- factor(inadmissibility_cases_considered$Stage , 
levels=c('Total identified for consideration on inadmissibility grounds', 
         'Notice of intent issued', 
         'Inadmissibility decision served', 
         'Removals', 
         'Subsequently admitted into UK asylum process'))

inadmissibility_cases_considered |>
  filter(Stage != "Notice of intent issued") |>
  group_by(Stage) |>
  summarise(Total = sum(Cases)) |> 
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/inadmissibility Sep 2023.csv")






# ----# Deno example on GitHub use, please ignore.#
asylum::applications |>
  filter(Year != "2023") |>
  group_by(Year) |>
  summarise(Applications = sum(Applications, na.rm = TRUE)) |>
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/applications only until 2022 - total.csv")

