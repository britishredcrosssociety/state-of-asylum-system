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
ggplot(ResettlmentTotal, aes(fill = `Case outcome group`, y = RTotal, x = Year)) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  labs(title = "Resettlment Case Total", x = "Year", y = "Total Cases")

#should be a stacked bar chart that is interactive for Shiny Web App# 



----##Resettlement Case Outcome##----

CaseOutcome <- decisions_resettlement %>%
  select(Year, `Case outcome`, Decisions) %>%
  group_by(Year,`Case outcome`) %>%
  summarise(Total = sum(Decisions))

#Case Outcome Overall
CaseOutcome %>%
  filter(Year > 2021) |>
  ggplot(aes(Year, Total)) +
  geom_point(aes(colour = `Case outcome`)) +
  geom_line(aes(colour = `Case outcome`)) +
  theme_classic() 

#Safe Routes#
CaseOutcomeRecent <- CaseOutcomeRecent %>%
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
  filter

CaseOutcomeRecent |>
  ggplot(aes(Year, Total)) +
  geom_point(aes(colour = `Case outcome`)) +
  geom_line(aes(colour = `Case outcome`)) +
  theme_classic()




#To discuss with the team on what they would like to have included in the Safe Routes graph. 
----#QUESTION: WHAT SAFE ROUTES HAVE BEEN AVAILABLE IN THE LAST 12 MONTHS? What is the Age, Sex, Nationality?#----

#Safe Routes# COME BACK TO THIS 

SafeRoutes <- decisions_resettlement %>%
  select(Date, Year, Quarter, `Case outcome`, Age, Nationality, Sex, Decisions) %>%
  group_by(Date, Year, Quarter, Nationality, Age, Sex, `Case outcome`) %>%
  summarise(TotalperCat = sum(Decisions))

SafeRoutesTotal <- SafeRoutes %>%
  group_by(Date, Year, Quarter, `Case outcome`) %>%
  summarise(TotalinSafeRoutes = sum(TotalperCat))

SafeRoutes <- SafeRoutes %>%
  filter(`Case outcome` == "Resettlement - UK Resettlement Scheme") %>%
  filter(`Case outcome` == "Resettlement - Community Sponsorship Scheme") %>%
  filter(`Case outcome` == "Resettlement - Mandate Scheme") %>%
  filter()



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

UKResettlementNat <- UKResettlement %>%
  group_by(Date, Year, Quarter, Nationality) %>%
  filter(Year > 2021) %>%
  summarise(TotalbyNat = sum(TotalperCategory)) 

#To check with team about this method of Nationality Visualization
UKResettlementNat%>%
  streamgraph("Nationality", "TotalbyNat", "Date") %>%
  sg_axis_x(1, "Date", "%Y") %>%
  sg_fill_brewer("PuOr") %>%
  sg_legend(show=TRUE, label="Nationalities: ")

