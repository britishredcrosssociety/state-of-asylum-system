library(tidyverse)
library(asylum)
library(readxl) 
  RS_Actions_June_22_to_23 <- readxl::read_excel("C:/Users/MathuraKugan/OneDrive - British Red Cross Society/RS Actions June 22 to 23.xlsx") %>%
 
  view(RS_Actions_June_22_to_23) 


# Question: How many people have we supported through refugee support and anti-trafficking services? 

  view(RS_Actions_June_22_to_23 |>
    distinct(MainPSN) |>
    count())
  
  #7484 people have been helped from June 2022- June 2023


RS_Actions_June_22_to_23 |>
  distinct(MainPSN, City = Main_City) |>
  mutate(City = str_to_title(City)) |>
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
  filter(City != "Null") 
  
# Question: Who have we supported (age, gender, nationality)?

view(RS_Actions_June_22_to_23 |>
  distinct(MainPSN, Age, Sex = Main_Gender) |> 
  
  filter(!is.na(Age)) |> 
  filter(!is.na(Sex)) |> 
  
  # Make age groups
  mutate(`Age group` = case_when(
    Age < 18 ~ "Under 18",
    Age >= 18 & Age < 30 ~ "18-29",
    Age >= 30 & Age < 50 ~ "30-49",
    Age >= 50 & Age < 70 ~ "50-69",
    Age >= 70 ~ "70+"
 
     )) |> 
  
  mutate(Sex = if_else(Sex == "NULL", "Unknown", Sex)) |> 
  
  count(`Age group`, Sex, sort = TRUE) |> 
  
  pivot_wider(names_from = Sex, values_from = n))

