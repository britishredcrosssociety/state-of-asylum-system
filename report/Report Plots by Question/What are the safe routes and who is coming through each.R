----#QUESTION:HOW MANY PEOPLE HAVE ARRIVED AND HAVE BEEN GRANTED PROTECTION UNDER SAFE ROUTES?#----

view(decisions_resettlement)

colnames(decisions_resettlement)

ResettlmentTotal <- decisions_resettlement %>%
  select(Year, `Case outcome group`, Decisions, `Case type`) %>%
  group_by(Year, `Case outcome group`, `Case type`) %>%
  summarise(RTotal = sum(Decisions))

view(ResettlmentTotal)



----#QUESTION: WHAT SAFE ROUTES HAVE BEEN AVAILABLE IN THE LAST 12 MONTHS?#----

#Resettlement#  

ResettlementScheme <- decisions_resettlement %>%
  select(Date, Year, Quarter, `Case outcome`, Age, Nationality, Sex, Decisions) %>%
  group_by(Date, Year, Quarter, Nationality, Age, Sex, `Case outcome`) %>%
  summarise(Total = sum(Decisions))

Resettlement22 <- ResettlementScheme %>%
  filter(Year > 2021) %>%
  group_by(Year, `Case outcome`) %>%
  summarise(TotalR = sum(Total)) 
  
Resettlement22 <- Resettlement22 %>%
  filter(`Case outcome` != "3rd Country Refusal") %>%	
  filter(`Case outcome` !=  "Certified Refusal") %>%
  filter(`Case outcome` != "Discretionary Leave") %>%
  filter(`Case outcome` != "Humanitarian Protection") %>%
  filter(`Case outcome` != "Non-Substantiated Withdrawal") %>%
  filter(`Case outcome` != "Non-Substantiated Withdrawal") %>%      
  filter(`Case outcome` != "Other Grants") %>% 
  filter(`Case outcome` != "Other Refusals") %>%
  filter(`Case outcome` !=  "Other Withdrawal") %>%
  filter(`Case outcome` != "UASC Leave") %>%
  filter(`Case outcome` != "Refugee Permission") %>%
  filter(`Case outcome` != "UASC Leave") %>%
  filter(`Case outcome` != "Temporary Refugee Permission") %>%
  filter(`Case outcome` != "Relocation - ARAP - Settled accommodation") %>%
  filter(`Case outcome` != "Relocation - ARAP - Temporary accommodation") %>%
  filter(`Case outcome` != "Relocation - ARAP - Accommodation not recorded ") %>%
  filter(`Case outcome` != "Relocation - ARAP - Accommodation not recorded")
  
Resettlement22 |> 
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
  ggplot(aes(fill = `Case outcome`, x = Year, y = TotalR)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  labs(title = "Number of people granted protection under a resettlement scheme for year ending March 2023", 
       x = "Year",
       y = "Number of Grants", 
       caption = "British Red Cross analysis of Home Office data, year ending March 2023") +
  scale_x_continuous(breaks = c(2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 2000)) +
  scale_fill_manual(values = c(brc_colours$red_dunant,
                               brc_colours$red_mercer,
                               brc_colours$red_deep,
                               brc_colours$red_light))



#Use .default = `Case outcome` to keep the names from the mutation above, ie if you want to only mutate a few things and not others, use .default#


##To confirm with team if they want nationalities, age and sex for every resettlement scheme or if we just want to focus on the important ones?##
----#Age of those per Resettlement Scheme#----
UKResettlement <- decisions_resettlement %>%
  filter(`Case outcome` == "Resettlement - UK Resettlement Scheme")

UKResettlement <- UKResettlement %>%
  select(Date, Year, Quarter, Nationality, Age, Sex, Decisions) %>%
  group_by(Date, Year, Quarter, Nationality, Age, Sex) %>%
  summarise(TotalperCategory = sum(Decisions)) 

UKResettlement

#Sex Last 12 Months UK Resettlement Scheme# 
UKResettlement %>%
  group_by(Date, Year, Quarter, Sex) %>%
  filter(Year > 2021) %>%
  summarise(TotalbySex = sum(TotalperCategory)) |>
  ggplot(aes(Quarter, TotalbySex, group = Sex)) +
  geom_line(aes(colour = Sex)) +
  geom_point(aes(colour = Sex)) +
  theme_classic() +
  labs(title = "Number of People Arriving through UK Resettlement Scheme by Sex",
       x = "Quarter", 
       y = "Total Number of People")

#By Age Last 12 Months UK Resettlement Scheme#
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

#By Nationality UK Resettlement Scheme# 
UKResettlement %>%
  group_by(Year, Quarter, Nationality) %>%
  filter(Year > 2021) %>%
  summarise(TotalbyNat = sum(TotalperCategory)) |>
  ggplot(aes(Quarter, Nationality)) +
  #geom_line(aes(group = Nationality)) +
  geom_point(aes(size = TotalbyNat, colour = Nationality), show.legend = NULL) +
  geom_text(aes(label = scales::comma(TotalbyNat)), show.legend = FALSE, size = rel(3)) +
  theme_classic() +
  labs(title = "Nationalities of People in UK Resettlement Scheme", 
       x = "Quarter", 
       y = "Number of People")
