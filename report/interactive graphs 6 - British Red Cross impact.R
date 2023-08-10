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

# By immigration status
brc_jun22_23 |> 
  distinct(MainPSN, Main_Immigration_Status) |> 
  count(Main_Immigration_Status, sort = TRUE) |> 
  filter(Main_Immigration_Status != "NULL") |> 
  mutate(Main_Immigration_Status = str_to_sentence(Main_Immigration_Status)) |> 
  mutate(Main_Immigration_Status = case_match(
    Main_Immigration_Status,
    "Indefinite leave to remain (ilr)" ~ "Indefinite leave to remain",
    "Discretionary leave to remain (dlr)" ~ "Discretionary leave to remain",
    "Homes for ukraine scheme" ~ "Homes for Ukraine Scheme",
    "Ukraine family visa scheme" ~ "Ukraine Family Visa Scheme",
    "Ukraine extension scheme" ~ "Ukraine Extension Scheme",
    "Syrian resettlement scheme" ~ "Syrian Resettlement Scheme",
    "Vulnerable children resettlement scheme" ~ "Vulnerable Children Resettlement Scheme",
    "Fully refused - no further reps" ~ "Fully refused - no further legal representations",
    "Fully refused - further reps submitted" ~ "Fully refused - further legal representations submitted",
    "Asylum seeker" ~ "People seeking asylum",
    "Uasc leave" ~ "Unaccompanied Asylum-Seeking Children",
    "Eea national" ~ "EEA national",
    .default = Main_Immigration_Status
  )) |>
  rename(`Immigration status` = Main_Immigration_Status, `Number of people supported` = n) |> 
  write_csv("data-raw/flourish/6 - BRC/people supported by immigration status.csv")

# How many nulls/unknowns?
brc_jun22_23 |> 
  distinct(MainPSN, Main_Immigration_Status) |> 
  filter(Main_Immigration_Status %in% c("NULL", "Unknown")) |> 
  count()

# ---- What is the breakdwon of enquiries that we have received to refugee support? ----

# ---- How have we supported people in the last 12 months (is it CBA and destitution support, advice, referrals to LA for housing etc?) ----
rs_actions <- 
  brc_jun22_23 |> 
  filter(ActionStatusName == "Completed - Successful") |> 
  mutate(
    Response_Type = str_remove(Response_Type, " - RFC") |> 
      str_replace("&", "and") |> 
      str_to_sentence()
  ) |> 
  count(Response_Type, sort = TRUE) |> 
  filter(Response_Type != "Null") |> 
  rename(`Type of response` = Response_Type, `Number of actions in last year` = n)
  
rs_actions |> 
  filter(!str_detect(`Type of response`, "General")) |> 
  write_csv("data-raw/flourish/6 - BRC/types of actions.csv")

# How many general actions?
rs_actions |> 
  filter(str_detect(`Type of response`, "General")) |> 
  summarise(sum(`Number of actions in last year`))

# ---- How has the support we have provided to people changed in the last 12 months and what are the possible causes? ----
# We do not have data for this

# ---- Length of support ----
brc_jun22_23 |> 
  distinct(MainPSN, BeneficiarySince) |> 
  select(BeneficiarySince) |> 
  na.omit() |> 
  
  mutate(MonthsOfSupport = interval(start = BeneficiarySince, end = ymd("2023-07-31")) %/% months(1)) |> 
  
  # ggplot(aes(x = MonthsOfSupport)) + geom_histogram(binwidth = 12)
  
  mutate(`Length of British Red Cross support` = case_when(
    MonthsOfSupport <= 12 ~ "1 year",
    MonthsOfSupport > 12 & MonthsOfSupport <= 24 ~ "2 years",
    MonthsOfSupport > 24 & MonthsOfSupport <= (12*5) ~ "3-5 years",
    MonthsOfSupport > (12*5) & MonthsOfSupport <= (12*10) ~ "6-10 years",
    MonthsOfSupport > (12*10) ~ "More than 10 years"
  )) |> 
  count(`Length of British Red Cross support`, name = "Number of people supported") |> 
  
  write_csv("data-raw/flourish/6 - BRC/length of support.csv")
