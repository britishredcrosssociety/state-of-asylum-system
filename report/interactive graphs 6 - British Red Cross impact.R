library(tidyverse)
library(readxl)

brc_jun22_23 <- read_excel("C:/Users/040026704/Downloads/RS Actions June 22 to 23 (1).xlsx")

# ---- How many people have we supported through our refugee support and anti-trafficking services and where? ----
# Total people supported
brc_jun22_23 |> 
  distinct(MainPSN) |> 
  count()

brc_jun22_23 |> 
  distinct(MainPSN, City = Main_City) |> 
  
  # Data cleaning
  mutate(City = str_to_title(City)) |> 
  
  # arrange(City) |> 
  # distinct(City) |> 
  # write_csv("data-raw/cities.csv")
  
  mutate(City = case_match(
    City,
    "Asahington" ~ "Ashington",
    c("Bounemouth", "Bouremout", "Bouremouth", "Bournemouth", "Bournmouth", "398 Charminster Road, Bournemouth") ~ "Bournemouth",
    "---Birmingham," ~ "Birmingham",
    ",Melton Mowbray" ~ "Melton Mowbray",
    "Bolton, Manchester" ~ "Bolton",
    "Bristol," ~ "Bristol",
    "Glagow" ~ "Glasgow",
    c("Leicester5", "Leicetser") ~ "Leicester",
    c("Liveerpool", "Liverool", "Liverppool", "Lliverpool") ~ "Liverpool",
    c("London (Cricklewood Area)", "London Area", "London Heathrow", "London/Ukraine") ~ "London",
    "Middlesborough" ~ "Middlesbrough",
    "Newcastle" ~ "Newcastle Upon Tyne",
    c("Norwch", "Norwich Nr5 8yl") ~ "Norwich",
    "Peterborugh" ~ "Peterborough",
    "Plyymouth" ~ "Plymouth",
    "Preston." ~ "Preston",
    "Stockton On Tees" ~ "Stockton-On-Tees",
    .default = City
  )) |> 
  
  count(City, sort = TRUE, name = "Number of people supported") |> 
  filter(City != "Null") |> 
  filter(`Number of people supported` > 100) |> 
  
  write_csv("data-raw/flourish/6 - BRC/people supported by location.csv")

# ---- Who have we supported? (age, gender, nationality) ----
# Age and sex
brc_jun22_23 |> 
  distinct(MainPSN, Age, Gender = Main_Gender) |> 
  
  filter(!is.na(Age)) |> 
  filter(!is.na(Gender)) |> 
  
  # Make age groups
  mutate(`Age group` = case_when(
    Age < 18 ~ "Under 18",
    Age >= 18 & Age < 30 ~ "18-29",
    Age >= 30 & Age < 50 ~ "30-49",
    Age >= 50 & Age < 70 ~ "50-69",
    Age >= 70 ~ "70+"
  )) |> 
  
  mutate(Gender = if_else(!Gender %in% c("Female", "Male"), "Other", Gender)) |> 
  
  count(`Age group`, Gender, sort = TRUE) |> 
  
  pivot_wider(names_from = Gender, values_from = n) |> 
  mutate(across(where(is.integer), ~replace_na(.x, 0))) |> 
  arrange(match(`Age group`, c("Under 18", "18-29", "30-49", "50-69", "70+"))) |> 
  
  write_csv("data-raw/flourish/6 - BRC/people supported by age and gender.csv")

# Country of origin
# brc_jun22_23 |> 
#   distinct(MainPSN, Main_CountryofOrigin) |> 
#   count(Main_CountryofOrigin, sort = TRUE) |> 
#   filter(Main_CountryofOrigin != "NULL") |> 
#   
#   mutate(Main_CountryofOrigin = case_match(
#     Main_CountryofOrigin,
#     "French Guiana" ~ 
#   ))

# Nationality
brc_jun22_23 |> 
  distinct(MainPSN, Main_Nationality) |> 
  count(Main_Nationality, sort = TRUE) |> 
  filter(Main_Nationality != "NULL") |> 
  rename(Nationality = Main_Nationality, `Number of people supported` = n) |> 
  slice(1:30) |> 
  write_csv("data-raw/flourish/6 - BRC/people supported by nationality.csv")

# How many nulls/unknowns?
brc_jun22_23 |> 
  distinct(MainPSN, Main_Nationality) |> 
  filter(Main_Nationality %in% c("NULL", "Unknown")) |> 
  count()

# ---- What is the breakdwon of enquiries that we have received to refugee support? ----

# ---- How have we supported people in the last 12 months (is it CBA and destitution support, advice, referrals to LA for housing etc?) ----

# ---- How has the support we have provided to people changed in the last 12 months and what are the possible causes? ----


# number of actions per person (in last 12 months) by 'beneficiary since'
