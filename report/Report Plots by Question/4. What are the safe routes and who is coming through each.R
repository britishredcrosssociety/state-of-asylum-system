library(tidyverse)
library(asylum)
source("report/brc_colours.R")
source("report/theme_brc.R")

# ---- QUESTION:HOW MANY PEOPLE HAVE ARRIVED AND HAVE BEEN GRANTED PROTECTION UNDER SAFE ROUTES? ----
view(decisions_resettlement)

colnames(decisions_resettlement)

resettlement_total <- decisions_resettlement %>%
  select(Year, `Case outcome group`, Decisions, `Case type`) %>%
  group_by(Year, `Case outcome group`, `Case type`) %>%
  summarise(Total = sum(Decisions))

# ---- QUESTION: WHAT SAFE ROUTES HAVE BEEN AVAILABLE IN THE LAST 12 MONTHS? ----

# Resettlement by Scheme

resettlement_scheme <- decisions_resettlement |>
  select(Date, Year, Quarter, `Case outcome`, Age, Nationality, Sex, Decisions) |>
  group_by(Date, Year, Quarter, Nationality, Age, Sex, `Case outcome`) |>
  summarise(Total = sum(Decisions))

resettlement_scheme <- resettlement_scheme |>
  filter(Year > 2021) |>
  #filter(Date >= max(Date) - dmonths(11)) %>%
  group_by(Year, `Case outcome`) |>
  summarise(Total = sum(Total))
  
resettlement_scheme <- resettlement_scheme |>
  filter(`Case outcome` != "3rd Country Refusal") |>	
  filter(`Case outcome` !=  "Certified Refusal") |>
  filter(`Case outcome` != "Discretionary Leave") |>
  filter(`Case outcome` != "Humanitarian Protection") |>
  filter(`Case outcome` != "Non-Substantiated Withdrawal") |>
  filter(`Case outcome` != "Non-Substantiated Withdrawal") |>     
  filter(`Case outcome` != "Other Grants") |>
  filter(`Case outcome` != "Other Refusals") |>
  filter(`Case outcome` !=  "Other Withdrawal") |>
  filter(`Case outcome` != "UASC Leave") |>
  filter(`Case outcome` != "Refugee Permission") |>
  filter(`Case outcome` != "UASC Leave") |>
  filter(`Case outcome` != "Temporary Refugee Permission") |>
  filter(`Case outcome` != "Relocation - ARAP - Settled accommodation") |>
  filter(`Case outcome` != "Relocation - ARAP - Temporary accommodation") |>
  filter(`Case outcome` != "Relocation - ARAP - Accommodation not recorded ") |>
  filter(`Case outcome` != "Relocation - ARAP - Accommodation not recorded")
  
resettlement_scheme |> 
  mutate(`Case outcome` = 
           case_match(
             `Case outcome`,
             (c("Resettlement - ACRS Pathway 1 - Accommodation not recorded",
               "Resettlement - ACRS Pathway 1 - Temporary accommodation", 
               "Resettlement - ACRS Pathway 2 - Settled accommodation",
               "Resettlement - ACRS Pathway 3 - Settled accommodation", 
               "Resettlement - ACRS Pathway 3 - Temporary accommodation",
               "Resettlement - ACRS Pathway 1 - Settled accommodation",
               "Resettlement - Afghan route not recorded - Settled accommodation",
               "Resettlement - Afghan route not recorded - Temporary accommodation") ~ "Afghan resettlement route"), 
             ("Resettlement - Community Sponsorship Scheme" ~ "Community Sponsorship Scheme"),
             ("Resettlement - Mandate Scheme" ~ "Mandate resettlement scheme"),
             ("Resettlement - UK Resettlement Scheme" ~ "UK resettlement scheme")
             )
         ) |>
  ggplot(aes(fill = `Case outcome`, x = Year, y = Total)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_continuous(breaks = c(2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 2000), expand = c(0, NA)) +
  scale_fill_manual(values = c(brc_colours$red_dunant,
                               brc_colours$red_mercer,
                               brc_colours$red_deep,
                               brc_colours$red_light)) +
  theme_brc() +
  labs(title = "Number of people granted protection under a resettlement scheme for year ending March 2023", 
       x = "Year",
       y = "Number of grants", 
       caption = "British Red Cross analysis of Home Office data, March 2022 to March 2023",
       fill = "Resettlement route")

# Use .default = `Case outcome` to keep the names from the mutation above, ie if you want to only mutate a few things and not others, use .default# 

# ---- QUESTION: HOW MANY PEOPLE HAVE COME THROUGH FAMILY REUINION PATHWAYS 
# ---- Family Reunion ----

family_reunion |>
  group_by(Year) |>
  summarise(Total = sum(`Visas granted`)) |>
  ggplot(aes(Year, Total)) +
  geom_line(aes(colour = "red"), show.legend = FALSE) +
  geom_point(aes(colour = "red", alpha = 0.5, size = Total), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(3)) +
  scale_x_continuous(breaks = c(2010 : 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 8000), expand = c(0,NA)) +
  theme_brc() +
  labs(title = "Number of family reunion visas granted from 2010 to 2023",
       x = NULL,
       y = "Number of visas granted", 
       caption = "British Red Cross analysis of Home Office data, March 2010 to March 2023")

# ---- Family Reunion Visas by Sex and Age ----
FamilyReunion <- family_reunion |> 
  group_by(Date, Nationality, Sex, Age) |>
  summarise(Total = sum(`Visas granted`)) |> 
  ungroup()

FamilyReunion22 <- FamilyReunion |>
  filter(Date >= max(Date) - dmonths(11)) |> 
  group_by(Sex, Age) |>
  summarise(TotalbyAge = sum(Total)) 

FamilyReunion22$Age <- factor(FamilyReunion22$Age, levels=c('70+', '50-69', '30-49', '18-29', 'Under 18'))

FamilyReunion22 |>
  ggplot(aes(fill = Age, x = Sex, y = TotalbyAge)) +
  geom_bar(position = "stack", stat = "identity") +
  ggrepel::geom_text_repel(aes(label = scales::number(TotalbyAge, big.mark = ',', accuracy = 1)), size = 3, 
                           position = position_stack(vjust = 0.5), direction = "y", 
                           box.padding = unit(0.01, "lines")) +
  theme_brc() +
  labs(title = "Family reunion visas granted by sex and age for year ending December 2022", 
       x = "Sex", 
       y = "Number of visas granted", 
       caption = "British Red Cross analysis of Home Office data, year ending December 2022") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 3500), expand = c(0, NA)) +
  scale_fill_manual(values = c(brc_colours$red_deep,
                               brc_colours$red_light,
                               brc_colours$red_dunant,
                               brc_colours$red_mercer,
                               brc_colours$red_earth))





















########### To confirm with team if they want nationalities, age and sex for every resettlement scheme or if we just want to focus on the important ones?# # 

# ---- Age of those per Resettlement Scheme ----
UKResettlement <- decisions_resettlement %>%
  filter(`Case outcome` == "Resettlement - UK Resettlement Scheme")

UKResettlement <- UKResettlement %>%
  select(Date, Year, Quarter, Nationality, Age, Sex, Decisions) %>%
  group_by(Date, Year, Quarter, Nationality, Age, Sex) %>%
  summarise(TotalperCategory = sum(Decisions)) 

UKResettlement

# Sex Last 12 Months UK Resettlement Scheme#  
UKResettlement %>%
  group_by(Date, Year, Quarter, Sex) %>%
  filter(Year > 2021) %>%
  summarise(TotalbySex = sum(TotalperCategory)) |>
  ggplot(aes(Quarter, TotalbySex, group = Sex)) +
  geom_line(aes(colour = Sex)) +
  geom_point(aes(colour = Sex)) +
  theme_classic() +
  labs(title = "Number of People Arriving through UK Resettlement Scheme by Sex, year ending December 2",
       x = "Quarter", 
       y = "Total Number of People")

# By Age Last 12 Months UK Resettlement Scheme# 
UKResettlement %>%
  group_by(Year, Quarter, Age) %>%
  filter(Year > 2021) %>%
  summarise(TotalbyAge = sum(TotalperCategory)) |>
  ggplot(aes(Quarter, TotalbyAge, group = Age)) +
  geom_line(aes(colour = Age)) +
  geom_point(aes(colour = Age)) +
  theme_classic() +
  labs(title = "Number of People Arriving Through UK Resettlement Scheme by Age", 
       x = "Quarter", 
       y = "Total Number of People")

# By Nationality UK Resettlement Scheme#  
UKResettlement %>%
  group_by(Year, Quarter, Nationality) %>%
  filter(Year > 2021) %>%
  summarise(TotalbyNat = sum(TotalperCategory)) |>
  ggplot(aes(Quarter, Nationality)) +
  # geom_line(aes(group = Nationality)) +
  geom_point(aes(size = TotalbyNat, colour = Nationality), show.legend = NULL) +
  geom_text(aes(label = scales::comma(TotalbyNat)), show.legend = FALSE, size = rel(3)) +
  theme_classic() +
  labs(title = "Nationalities of People in UK Resettlement Scheme", 
       x = "Quarter", 
       y = "Number of People")
