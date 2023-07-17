----#QUESTION:HOW MANY PEOPLE HAVE ARRIVED AND HAVE BEEN GRANTED PROTECTION UNDER SAFE ROUTES?#----
----#Safe Routes Analysis- Resettlement Asylum Case#----

view(decisions_resettlement)

colnames(decisions_resettlement)

ResettlmentTotal <- decisions_resettlement %>%
  select(Year, `Case outcome group`, Decisions) %>%
  group_by(Year, `Case outcome group`) %>%
  summarise(RTotal = sum(Decisions))

view(ResettlmentTotal)

----##Resettlement Bar Graph##----
ResettlmentTotal %>%
ggplot(aes(fill = `Case outcome group`, y = RTotal, x = Year)) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  labs(title = "Resettlement Outcomes of Asylum Applications at Initial Decision",
       subtitle = "Resettlement grouped by case outcome",
       x = NULL, 
       y = "Decisions", 
       caption = "British Red Cross Analysis of Home Office Data, until 2022") +
  scale_x_continuous(breaks = c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) + 
  scale_fill_manual(values = c(brc_colours$red_deep,
                               brc_colours$red_mercer,
                               brc_colours$red_dunant,
                               brc_colours$red_light))

 
----#QUESTION: WHAT SAFE ROUTES HAVE BEEN AVAILABLE IN THE LAST 12 MONTHS?#----

#Safe Routes#  

SafeRoutes <- decisions_resettlement %>%
  select(Date, Year, Quarter, `Case outcome`, Age, Nationality, Sex, Decisions) %>%
  group_by(Date, Year, Quarter, Nationality, Age, Sex, `Case outcome`) %>%
  summarise(Total = sum(Decisions))

SF2022 <- SafeRoutes %>%
  filter(Year > 2021) %>%
  group_by(Year, `Case outcome`) %>%
  summarise(Total = sum(TotalF)) 
  
SF2022 <- SF2022 %>%
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
  filter(`Case outcome` != "Relocation - ARAP - Accommodation not recorded ")
  
library(plyr)

SF2022$`Case outcome` <- revalue(SF2022$`Case outcome`, 
                                 c("Resettlement - ACRS Pathway 1 - Accommodation not recorded" = "ACRS", 
                                   "Resettlement - ACRS Pathway 1 - Temporary accommodation" = "ACRS", 
                                   "Resettlement - ACRS Pathway 2 - Settled accommodation" = "ACRS",
                                   "Resettlement - ACRS Pathway 3 - Settled accommodation" = "ACRS", 
                                   "Resettlement - ACRS Pathway 3 - Temporary accommodation" = "ACRS",
                                   "Resettlement - ACRS Pathway 1 - Settled accommodation" = "ACRS"))

SF2022 %>%
  filter(`Case outcome` != "Relocation - ARAP - Accommodation not recorded") %>%
  ggplot(aes(fill = `Case outcome`, x = Year, y = Total)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  labs(title = "Number of People Granted Protection Under a Safe Route", 
       x = NULL,
       y = "Grants", 
       caption = "British Red Cross Analyses on Home Office Data, year ending March, 2023") +
  scale_x_continuous(breaks = c(2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))


##To confirm with team if they want nationalities, age and sex for every resettlement scheme or if we just want to focus on the important ones?##
----#UK Resettlement Scheme#----

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
