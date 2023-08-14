library(tidyverse)
library(asylum)
library(readxl) 
  RS_Actions_June_22_to_23 <- readxl::read_excel("C:/Users/MathuraKugan/OneDrive - British Red Cross Society/RS Actions June 22 to 23.xlsx") 
 
  view(RS_Actions_June_22_to_23) 

# ---- 1. How many people have we supported through our refugee support and anti-trafficking services and where? ----
# Total people supported
BRC_supported_where <-
{RS_Actions_June_22_to_23 |> 
  distinct(MainPSN) |> 
  count()

RS_Actions_June_22_to_23 |>
  distinct(MainPSN, City = Main_City) |> 
  
  # Data cleaning
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
  filter(City != "Null") |> 
  group_by(City) |>
  summarise(Total = sum(`Number of people supported`))

}

BRC_supported_where |>
  summarise(BRCT = sum(Total))

# 18328 people are supported across 

BRC_supported_where|>
  slice_max(Total, n = 10) |>
  ggplot(aes(x = reorder(City, desc(Total)), y = Total)) +
  geom_col(colour = brc_colours$red_dunant, fill = brc_colours$red_dunant) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(3), position = position_dodge(width=1), vjust=-0.25, colour = brc_colours$black_shadow) +
  theme_brc() +
  labs(title = "Top 10 cities where the British Red Cross has provided support for year ending June 2023",
       subtitle = str_wrap("18,328 people have been helped across the United Kingdom including the Midlands, East of England, London, the North East, the North West, 
                          Northern Ireland, South East, South West, Scotland, Wales and Yorkshire"), 
       x = "City", 
       y = "Number of people", 
       caption = "British Red Cross analysis of British Red Cross data, June 2022 to June 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 2000), expand = c(0,NA)) +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5))
  
  

# ---- 2. Who have we supported? (age, gender, nationality) ----
# Age and sex
BRC_support_sex_age <- 

{  
RS_Actions_June_22_to_23 |> 
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
  pivot_longer(cols = `Male`:`Other`, names_to = "Gender", values_to = "People")
  
}

BRC_support_sex_age$`Age group` <- factor(BRC_support_sex_age$`Age group`, levels = c('70+', '50-69', '30-49', '18-29', 'Under 18'))

BRC_support_sex_age |>
  ggplot(aes(fill = `Age group`, x = Gender, y = People)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_brc() +
  labs(title = "Number of people assisted by the British Red Cross by Age and Sex for year ending June 2023",
       subtitle = "'Other' includes those who prefer not to disclose their gender, who are gender fluid, non-binary and other genders",
       x = "Sex", 
       y = "Number of people", 
       fill = "Age",
       caption = "British Red Cross analysis of British Red Cross data, June 2022 to June 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 12000), expand = c(0,NA)) +
  scale_fill_manual(values = c(brc_colours$blue,
                               brc_colours$teal,
                               brc_colours$steel,
                               brc_colours$red_light,
                               brc_colours$red_mercer))

# Nationality
BRC_support_nationality <- 

{
  RS_Actions_June_22_to_23 |> 
  distinct(MainPSN, Main_Nationality) |> 
  count(Main_Nationality, sort = TRUE) |> 
  filter(Main_Nationality != "NULL") |> 
  rename(Nationality = Main_Nationality, `Number of people supported` = n) |> 
  slice(1:30) |> 
  group_by(Nationality) %>%
  summarise(Total = sum(`Number of people supported`)) 
}

BRC_support_nationality |>
  slice_max(Total, n = 10) |>
  ggplot(aes(x = reorder(Nationality, desc(Total)), y = Total)) +
  geom_col(colour = brc_colours$red_dunant, fill = brc_colours$red_dunant) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(3), position = position_dodge(width=1), vjust=-0.25, colour = brc_colours$black_shadow) +
  theme_brc() +
  labs(title = "Top 10 nationalities of the people supported by the British Red Cross for year ending June 2023",
       subtitle = "Over the last 12 month period, individuals from 143 different countries have been supported by the British Red Cross",
       x = "Nationality", 
       y = "Number of people", 
       caption = "British Red Cross analysis of British Red Cross data, June 2022 to June 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 2000), expand = c(0,NA)) +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5))

##Need to determine how many unknown nationalities we have helped- issues with how we record it.#
# How many nulls/unknowns?
RS_Actions_June_22_to_23 |> 
  distinct(MainPSN, Main_Nationality) |> 
  filter(Main_Nationality %in% c("NULL", "Unknown")) |> 
  count() |>
  summarise(Total = sum(n))

# ---- 3. By immigration status ----
BRC_support_immigration <- 
{
RS_Actions_June_22_to_23 |> 
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
  group_by(`Immigration status`) |>
  summarise(Total = sum(`Number of people supported`))
}

BRC_support_immigration |>
  ggplot(aes(x = reorder(`Immigration status`, desc(Total)), y = Total)) +
  geom_col(colour = brc_colours$red_dunant, fill = brc_colours$red_dunant) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(3), position = position_dodge(width=1), vjust=-0.25, colour = brc_colours$black_shadow) +
  theme_brc() +
  labs(title = "Immigration statuses of the people supported by the British Red Cross for year ending June 2023",
       x = "Immigration Status", 
       y = "Number of people", 
       caption = "British Red Cross analysis of British Red Cross data, June 2022 to June 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 15000), expand = c(0,NA)) +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5))


# How many nulls/unknowns?
brc_jun22_23 |> 
  distinct(MainPSN, Main_Immigration_Status) |> 
  filter(Main_Immigration_Status %in% c("NULL", "Unknown")) |> 
  count()

# ---- 4. Length of support ----
BRC_support_length <-
  {
    RS_Actions_June_22_to_23 |>
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
      group_by(`Length of British Red Cross support`) |>
      summarise(Total = sum(`Number of people supported`))
  }

BRC_support_length |>
  ggplot(aes(x = `Length of British Red Cross support`, y = Total)) +
  geom_col(colour = brc_colours$red_dunant, fill = brc_colours$red_dunant) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(3), position = position_dodge(width=1), vjust=-0.25, colour = brc_colours$black_shadow) +
  theme_brc() +
  labs(title = "Length of time people have been supported by the British Red Cross from year ending June 2023",
       x = "Length of time", 
       y = "Number of people", 
       caption = "British Red Cross analysis of British Red Cross data, June 2022 to June 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 15000), expand = c(0,NA)) 

# ---- 5. How have we supported people in the last 12 months (is it CBA and destitution support, advice, referrals to LA for housing etc?) ----
rs_actions <- 
  
{
  RS_Actions_June_22_to_23 |> 
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
  group_by(`Type of response`) |>
  summarise(Total = sum(`Number of actions in last year`))

}

rs_actions |>
  ggplot(aes(x = reorder(`Type of response`, desc(Total)), y = Total)) +
  geom_col(colour = brc_colours$red_dunant, fill = brc_colours$red_dunant) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(3), position = position_dodge(width=1), vjust=-0.25, colour = brc_colours$black_shadow) +
  theme_brc() +
  labs(title = "Types of support provided by the British Red Cross from year ending June 2023",
       x = "Methods of support", 
       y = "Number of people", 
       caption = "British Red Cross analysis of British Red Cross data, June 2022 to June 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 20000), expand = c(0,NA)) +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5))


# How many general actions?
rs_actions |> 
  filter(str_detect(`Type of response`, "General")) |> 
  summarise(sum(`Number of actions in last year`))


