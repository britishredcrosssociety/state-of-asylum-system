library(tidyverse)
library(readxl)

RS_Actions_Sep_30_22_to_23_1_ <- read_excel("~/GitHub/state-of-asylum-system/data-raw/data source/RS Actions Sep 30 22 to 23 (1).xlsx", 
                                                                             +     sheet = "Sheet1")
View(RS_Actions_Sep_30_22_to_23_1_)

brc_sep22_23 <- RS_Actions_Sep_30_22_to_23_1_

# ---- Flourish- Section 6, Slide 3: Support by immigration status ----
brc_sep22_23 |>
  distinct(MainPSN, Main_Immigration_Status) |> 
  count(Main_Immigration_Status, sort = TRUE) |> 
  filter(Main_Immigration_Status != "NULL") |> 
  #mutate(Main_Immigration_Status = str_to_sentence(Main_Immigration_Status)) |> 
  mutate(Main_Immigration_Status = case_match(
    Main_Immigration_Status,
    c("Asylum Seeker",
    "Fully Refused - Further Reps Submitted",
    "Fully Refused - No Further Reps") ~ "People who have claimed asylum",
    c("Refugee Status",
    "Indefinite Leave to Remain (ILR)",
    "Discretionary Leave to Remain (DLR)",
    "Family Reunion Visa",
    "Humanitarian Protection",
    "UASC Leave",
   "Leave to Remain") ~ "People who have been granted protection",
    c("Spousal Visa", 
   "British National", 
   "EEA National") ~ "People with other forms of leave to remain",
    c("Syrian Resettlement Scheme", 
    "Homes for Ukraine Scheme",
    "Ukraine Extension Scheme",
    "Ukraine Family Visa Scheme",
    "Vulnerable Children Resettlement Scheme") ~ "People arrived through resettlement scheme",
    c("Overstayer",
   "Irregular Migrant")~ "Other migrants",
    .default = Main_Immigration_Status
  )) |> 
  rename(`Immigration status` = Main_Immigration_Status, `Number of people supported` = n) |> 
  group_by(`Immigration status`) |>
  summarise(Total = sum(`Number of people supported`)) |> 
  write_csv("data-raw/flourish/6 - BRC/people supported by immigration status Sep 2023.csv")
            
BRC_immigration_updated <- brc_sep22_23 |>
  distinct(MainPSN, Main_Immigration_Status) |>
  count(Main_Immigration_Status, sort = TRUE) |> 
  filter(Main_Immigration_Status != "NULL") |> 
  rename(`Immigration Status` = Main_Immigration_Status, `Number of people supported` = n) 
    
 #How many nulls/unknowns?
brc_sep22_23 |> 
  distinct(MainPSN, Main_Immigration_Status) |> 
  filter(Main_Immigration_Status %in% c("NULL", "Unknown")) |> 
  count()

# ---- Flourish- Section 6, Slide 4: Support by country of origin ----
brc_sep22_23 |> 
  distinct(MainPSN, Main_CountryofOrigin) |> 
  count(Main_CountryofOrigin, sort = TRUE) |> 
  filter(Main_CountryofOrigin != "NULL") |> 
  rename(`Country of origin` = Main_CountryofOrigin, `Number of people supported` = n) |> 
  slice(1:30) |> 
  write_csv("data-raw/flourish/6 - BRC/people supported by country of origin Sep 2023.csv")

# How many nulls/unknowns?
 brc_sep22_23 |> 
   distinct(MainPSN, Main_CountryofOrigin) |> 
   filter(Main_CountryofOrigin %in% c("NULL", "Unknown")) |> 
   count()

# ---- Flourish- Section 6, Slide 5: How many people have we supported through our refugee support and anti-trafficking services and where? ----
# Total people supported
brc_sep22_23 |> 
  distinct(MainPSN) |> 
  count()

# 22,495 people have been helped through our refugee and anti-trafficking services.

brc_sep22_23 |> 
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
  
  write_csv("data-raw/flourish/6 - BRC/people supported by location Sep 2023.csv")

# ---- Flourish- Section 6, Slide 6: People supported by age and gender ----
brc_sep22_23 |> 
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
  
  write_csv("data-raw/flourish/6 - BRC/people supported by age and gender Sep 2023.csv")

# ---- Flourish- Section 6, Slide 7: Length of support ----
brc_length_of_support_Sep_23 <- 
  brc_sep22_23 |> 
  distinct(MainPSN, BeneficiarySince) |> 
  select(BeneficiarySince) |> 
  na.omit() |> 
  
  mutate(MonthsOfSupport = interval(start = BeneficiarySince, end = ymd("2023-09-30")) %/% months(1)) |> 
  
  # ggplot(aes(x = MonthsOfSupport)) + geom_histogram(binwidth = 12)
  
  mutate(`Length of British Red Cross support` = case_when(
    MonthsOfSupport < 12 ~ "Less than 1 year",
    #MonthsOfSupport >= 12 ~ "1 year",
    MonthsOfSupport >= 12 & MonthsOfSupport <= 24 ~ "1-2 years",
    MonthsOfSupport > 24 & MonthsOfSupport <= (12*5) ~ "3-5 years",
    MonthsOfSupport > (12*5) & MonthsOfSupport <= (12*10) ~ "6-10 years",
    MonthsOfSupport > (12*10) ~ "More than 10 years"
  )) |> 
  count(`Length of British Red Cross support`, name = "Number of people supported") |> 
  view()
  
brc_length_of_support_Sep_23 |> 
  write_csv("data-raw/flourish/6 - BRC/length of support Sep 2023.csv")


# - CAPTION -
brc_length_of_support_Sep_23 |> 
  mutate(`Proportion of people` = `Number of people supported` / sum(`Number of people supported`)) |> 
  arrange(`Number of people supported`) |> 
  mutate(`Cumulative proportion` = cumsum(`Proportion of people`))
<<<<<<< HEAD
=======

# ---- How have we supported people in the last 12 months (is it CBA and destitution support, advice, referrals to LA for housing etc?) ----
rs_actions <- 
  brc_sep22_23 |> 
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

#---- Flourish- Section 6, Slide 10: BRC Destitution ---- #
library(readxl)

Beneficiaries_with_destitution_action_by_Top_10_Country_of_Origin_5_ <- read_excel("~/GitHub/state-of-asylum-system/data-raw/data source/Beneficiaries with destitution action by Top 10 Country of Origin (5).xlsx", 
                                                                                     +     skip = 2)
View(Beneficiaries_with_destitution_action_by_Top_10_Country_of_Origin_5_)

DestitutionBRC <- Beneficiaries_with_destitution_action_by_Top_10_Country_of_Origin_5_ 

DestitutionBRC |>
  distinct(MainPSN, Main_CountryofOrigin) |> 
  count(Main_CountryofOrigin, sort = TRUE) |>
  rename(`Country of origin` = Main_CountryofOrigin, `Number of people supported` = n) |> 
  write_csv("data-raw/flourish/6 - BRC/destitution by nationality Sep 2023.csv")


# ---- Family Reunion Data ---- #

#---- Flourish- Section 6: Family Reunion Travel Assistance ----

FRTA_2_ <- read_excel("~/GitHub/state-of-asylum-system/data-raw/data source/FRTA 31 Dec 2022 to 31 Dec 2023.xlsx")

View(FRTA_2_)

FRTA_Dec23_ <- FRTA_2_ |>
  select(`Reference Number`, `Date of Birth`, Gender, `Country of Origin`, Age)

FRTA_Dec23_ |>
  distinct(`Reference Number`) |> 
  count()

# 465 people were helped by the British Red Cross travel assistance program. 


# Age and Gender of FRTA #

FRTA_Dec23_ |> 
  distinct(`Reference Number`, Age, Gender) |> 
  
  filter(!is.na(Age)) |> 
  filter(!is.na(Gender)) |> 
  
  # Make age groups
  mutate(`Age` = case_when(
    Age < 18 ~ "Under 18",
    Age >= 18 & Age < 30 ~ "18-29",
    Age >= 30 & Age < 50 ~ "30-49",
    Age >= 50 & Age < 70 ~ "50-69",
    Age >= 70 ~ "70+"
  )) |> 
  
  mutate(Gender = if_else(!Gender %in% c("Female", "Male"), "Other", Gender)) |> 
  
  count(`Age`, Gender, sort = TRUE) |> 
  
  pivot_wider(names_from = Gender, values_from = n) |> 
  mutate(across(where(is.integer), ~replace_na(.x, 0))) |> 
  arrange(match(`Age`, c("Under 18", "18-29", "30-49", "50-69", "70+"))) |> 
  
  write_csv("data-raw/flourish/6 - BRC/Family reunion travel people supported by age and gender Dec 23.csv")

#---- Flourish- Section 6, Slide 8: Family Reunion Travel Assistance BRC ---- 

FRTA_Dec23_ |> 
  distinct(`Reference Number`, `Country of Origin`) |> 
  count(`Country of Origin`, sort = TRUE) |> 
  filter(`Country of Origin`!= "NULL") |> 
  rename(`Country of origin` = `Country of Origin`, `Number of people supported` = n) |> 
  write_csv("data-raw/flourish/6 - BRC/Family reunion travel by country of origin Dec 23.csv")


## ---- Family Reunion Support Project ---- ## 

#FRSP_2_ <- read_excel("C:/Users/MathuraKugan/Downloads/FRSP (2).xlsx")

#View(FRSP_2_)

#FRSP_2_ <- FRSP_2_ |>
# select(`Reference Number`, `Date of Birth`, Gender, `Country of Origin`, Age)

#FRSP_2_ |>
#distinct(`Reference Number`) |> 
#count()

# 339 people were helped by the British Red Cross family reunion project . 


#BRC_family_reunion_support <- FRSP_2_

# Age and Gender of FRTA #

#BRC_family_reunion_support |> 
# distinct(`Reference Number`, Age, Gender) |> 

#filter(!is.na(Age)) |> 
#filter(!is.na(Gender)) |> 

# Make age groups
# mutate(`Age` = case_when(
#  Age < 18 ~ "Under 18",
# Age >= 18 & Age < 30 ~ "18-29",
#Age >= 30 & Age < 50 ~ "30-49",
#Age >= 50 & Age < 70 ~ "50-69",
#Age >= 70 ~ "70+"
# )) |> 

# mutate(Gender = if_else(!Gender %in% c("Female", "Male"), "Other", Gender)) |> 

# count(`Age`, Gender, sort = TRUE) |> 

#  pivot_wider(names_from = Gender, values_from = n) |> 
# mutate(across(where(is.integer), ~replace_na(.x, 0))) |> 
#  arrange(match(`Age`, c("Under 18", "18-29", "30-49", "50-69", "70+"))) |> 

# write_csv("data-raw/flourish/6 - BRC/Family reunion SUPPORT people supported by age and gender.csv")

# Country of Origin #

#BRC_family_reunion_support |> 
# distinct(`Reference Number`, `Country of Origin`) |> 
#count(`Country of Origin`, sort = TRUE) |> 
#filter(`Country of Origin`!= "NULL") |> 
#rename(`Country of origin` = `Country of Origin`, `Number of people supported` = n) |> 
#write_csv("data-raw/flourish/6 - BRC/Family reunion SUPPORT by country of origin.csv")

#---- Flourish- Section 6, Slide 10: BRC Destitution ---- #
library(readxl)

Beneficiaries_with_destitution_action_by_Top_10_Country_of_Origin_5_ <- read_excel("~/GitHub/state-of-asylum-system/data-raw/data source/Beneficiaries with destitution action by Top 10 Country of Origin (5).xlsx", 
                                                                                     +     skip = 2)
View(Beneficiaries_with_destitution_action_by_Top_10_Country_of_Origin_5_)

DestitutionBRC <- Beneficiaries_with_destitution_action_by_Top_10_Country_of_Origin_5_ 

DestitutionBRC |>
  distinct(MainPSN, Main_CountryofOrigin) |> 
  count(Main_CountryofOrigin, sort = TRUE) |>
  rename(`Country of origin` = Main_CountryofOrigin, `Number of people supported` = n) |> 
  write_csv("data-raw/flourish/6 - BRC/destitution by nationality Sep 2023.csv")



#---- OLD CODE ---- 
## OLD CODE FROM MATT BEFORE RENAMED BY BRC OPS##

# ---- old code: Support by immigration status ----
# By immigration status
#brc_jun22_23 |> 
#distinct(MainPSN, Main_Immigration_Status) |> 
#count(Main_Immigration_Status, sort = TRUE) |> 
#filter(Main_Immigration_Status != "NULL") |> 
#mutate(Main_Immigration_Status = str_to_sentence(Main_Immigration_Status)) |> 
#mutate(Main_Immigration_Status = case_match(
#Main_Immigration_Status,
#c("Asylum Seeker", "Fully Refused - Further Reps Submitted", "Fully ") ~ "People who have claimed asylum",
#"Discretionary leave to remain (dlr)" ~ "Discretionary leave to remain",
#"Homes for ukraine scheme" ~ "Homes for Ukraine Scheme",
# "Ukraine family visa scheme" ~ "Ukraine Family Visa Scheme",
#"Ukraine extension scheme" ~ "Ukraine Extension Scheme",
#"Syrian resettlement scheme" ~ "Syrian Resettlement Scheme",
# "Vulnerable children resettlement scheme" ~ "Vulnerable Children Resettlement Scheme",
# "Fully refused - no further reps" ~ "Fully refused - no further legal representations",
#"Fully refused - further reps submitted" ~ "Fully refused - further legal representations submitted",
#"Asylum seeker" ~ "People seeking asylum",
# "Uasc leave" ~ "Unaccompanied Asylum-Seeking Children",
# "Eea national" ~ "EEA national",
#.default = Main_Immigration_Status
# )) |>
#rename(`Immigration status` = Main_Immigration_Status, `Number of people supported` = n) |> 
#write_csv("data-raw/flourish/6 - BRC/people supported by immigration status2.csv")


# ALL RS BRM ACTIONS NO LONGER NEEDED AS PER REQUEST FROM POLICY #
# ---- Old code: All RS BRM Actions ---- #

#'library(readxl)
#All_RS_BRM_Actions_ <- read_excel("C:/Users/MathuraKugan/Downloads/All RS BRM Actions!.xlsx", 
#                                  +     sheet = "BRM - RS Actions (July 23)", 
#                                 +     skip = 1)
#View(All_RS_BRM_Actions_)

#Updated_BRC_Ops <- All_RS_BRM_Actions_ |>
 # select(`Unique n. SU's with this action Jan 22 - July 23`, `Current BRM Category`, `Final Recommendation`)

#BRC_Actions_Summary <- Updated_BRC_Ops |>
 # group_by(`Final Recommendation`) |>
  #summarise(Total = sum(`Unique n. SU's with this action Jan 22 - July 23`)) 

#view(BRC_Actions_Summary)

# Other and Workstream Specific are 0, so will be removed from dataset, before being uploaded to flourish. 

#BRC_Actions_Summary |>
 # filter(`Final Recommendation` != "Other") |>
  #filter(`Final Recommendation` != "Workstream Specific") |>
  #write_csv("data-raw/flourish/6 - BRC/Updated action summary.csv")

#OLD CODE- THIS IS FOR ACTION ITEMS, NOT REQUIRED UNLESS ASKED FOR#
# ---- How have we supported people in the last 12 months (is it CBA and destitution support, advice, referrals to LA for housing etc?) ----
#rs_actions <- 
# brc_jun22_23 |> 
#filter(ActionStatusName == "Completed - Successful") |> 
#mutate(
# Response_Type = str_remove(Response_Type, " - RFC") |> 
#  str_replace("&", "and") |> 
# str_to_sentence()
#) |> 
#count(Response_Type, sort = TRUE) |> 
#filter(Response_Type != "Null") |> 
#rename(`Type of response` = Response_Type, `Number of actions in last year` = n)

#rs_actions |> 
# filter(!str_detect(`Type of response`, "General")) |> 
#write_csv("data-raw/flourish/6 - BRC/types of actions.csv")

# How many general actions?
#rs_actions |> 
# filter(str_detect(`Type of response`, "General")) |> 
#summarise(sum(`Number of actions in last year`))

