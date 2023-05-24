##Applications and Questions#

##Questions: 

#Who is applying for asylum in the last 12 months?
#age 
#nationality
#gender

library(tidyverse)
library(asylum)
library(ggfittext)
library(ggtext)
library(IMD)
source("https://github.com/matthewgthomas/brclib/raw/master/R/colours.R")

----#Code that works without issue & creates good graphs#----


----#QUESTION: WHO IS APPLYING FOR ASYLUM IN THE UK (NATIONALITY, GENDER AND AGE)#----
----##Age Analysis with Aesthetic Graph##----
AgeAnalysis <- applications %>%
  select(Year, Age, Applications) %>%
  filter(Year > '2008')

view(AgeAnalysis)

AgeAnalysis <- AgeAnalysis %>%
  group_by(Age, Year) %>%
  summarise(AgeGroupSum = sum(Applications))
  
AllAgesAllYears <- AgeAnalysis |>
  ggplot(aes(Year, AgeGroupSum)) +
  geom_line(aes(colour = Age)) +
  geom_point(aes(colour = Age), size = 1) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_color_manual(values = c(get_brc_colours()$teal, get_brc_colours()$teal_light, get_brc_colours()$red, get_brc_colours()$red_dark, get_brc_colours()$red_light, get_brc_colours()$blue)) +
  scale_fill_manual(values = c(get_brc_colours()$teal, get_brc_colours()$teal_light, get_brc_colours()$red, get_brc_colours()$red_dark, get_brc_colours()$red_light, get_brc_colours()$blue)) +
  theme_classic() +
  guides(fill=guide_legend(nrow=6,byrow=TRUE), color = guide_legend(nrow=6,byrow=TRUE))+
  theme(
    legend.position = "right",
    # legend.box = "vertical",
    # legend.margin = margin(),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 12)
  ) +
  labs(
    title = "Asylum Application by Age Group All Years",
    x = "Year",
    y = "Number of Applicants",
    caption = "British Red Cross analysis of Home Office data"
  )

AllAgesAllYears 

----#Last 12 Months- Age Analysis#----
AgeAnalysis12Months <- applications %>%
  select(Date, Year, Age, Applications) %>%
  filter(Year > '2021')

view(AgeAnalysis12Months)

AgeAnalysis12Months <- AgeAnalysis12Months %>%
  group_by(Age, Date) %>%
  summarise(AgeGroupSum = sum(Applications))

AllAge12Months <- AgeAnalysis12Months %>%
  ggplot(aes(Date, AgeGroupSum)) +
  geom_line(aes(colour = Age)) +
  geom_point(aes(colour = Age), size = 1) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_color_manual(values = c(get_brc_colours()$teal, get_brc_colours()$teal_light, get_brc_colours()$red, get_brc_colours()$red_dark, get_brc_colours()$red_light, get_brc_colours()$blue)) +
  scale_fill_manual(values = c(get_brc_colours()$teal, get_brc_colours()$teal_light, get_brc_colours()$red, get_brc_colours()$red_dark, get_brc_colours()$red_light, get_brc_colours()$blue)) +
  theme_classic() +
  guides(fill=guide_legend(nrow=6,byrow=TRUE), color = guide_legend(nrow=6,byrow=TRUE))+
  theme(
    legend.position = "right",
    # legend.box = "vertical",
    # legend.margin = margin(),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 12)
  ) + labs(title = "Asylum Application by Age Group Last 12 Months",
                     x = "Year",
                     y = "Number of Applicants",
                     caption = "British Red Cross analysis of Home Office data")

AllAge12Months
  
----##Sex Analysis with Aesthetic Graph##----
SexAnalysis <- applications %>%
  select(Year, Sex, Applications, Nationality, Age, Quarter) %>%
  filter(Year > '2008')

view(SexAnalysis)

SexAnalysis2 <- SexAnalysis %>%
  group_by(Sex, Year) %>%
  summarise(SexGroupSum = sum(Applications))

view(SexAnalysis2)

AllSAllYears <- SexAnalysis2 |>
  ggplot(aes(Year, SexGroupSum)) +
  geom_line(aes(colour = Sex)) +
  geom_point(aes(colour = Sex), size = 1) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_color_manual(values = c(get_brc_colours()$teal, get_brc_colours()$red, get_brc_colours()$blue)) +
  scale_fill_manual(values = c(get_brc_colours()$teal, get_brc_colours()$red, get_brc_colours()$blue)) +
  theme_classic() +
  guides(fill=guide_legend(nrow=3,byrow=TRUE), color = guide_legend(nrow=3,byrow=TRUE))+
  theme(
    legend.position = "right",
    # legend.box = "vertical",
    # legend.margin = margin(),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 12)
  ) +
  labs(
    title = str_glue("Asylum Application by Sex All Years",
                     x = "Year",
                     y = "Number of Applicants",
                     caption = "British Red Cross analysis of Home Office data"
    ))

AllSAllYears 

----#QUESTION: HOW LONG IS IT TAKING FOR DECISIONS TO BE MADE?#---- 
----#Backlog Analyses#----

#Note from their own data release, it looks like they only include age groups and gender. No nationality. Does not break down each reason by age or gender though.#
#Backlog analyses only released during Q2!!!!!#

Backlog <- asylum_work_in_progress %>%
  select( Date , `Total Work In Progress`) 
  
 ----##Graph: Backlog Overall##---- 
  Backlog |>
    ggplot(aes(Date, `Total Work In Progress`)) +
    geom_line(aes(colour = 'red'), show.legend = FALSE) +
    geom_point(aes(size = `Total Work In Progress`, alpha = 0.4, colour = 'red'), show.legend = FALSE) +
    geom_text(aes(label = scales::comma(`Total Work In Progress`)), show.legend = FALSE, size = rel(4)) +
    theme_classic() +
    labs(title = 'Backlog of Applications', 
         x = 'Date', 
         y = 'Total Number of Applications in Progress', 
         caption = 'BRC Analysis of UK Home Office Data. Mock analyses')
  
  
----##Backlog Reason##----
backlogbyreason <- asylum_work_in_progress %>%
  select(Date, `Awaiting Initial Asylum Decision`,`Post Decision`,`Asylum Appeal Outstanding`,
         `Subject to Removal Action`,`On Hold`,`Further Leave Application Outstanding`)
view(backlogbyreason)

#Checking class before starting to plot#

class(asylum_work_in_progress$`Awaiting Initial Asylum Decision`)
class(asylum_work_in_progress$`Post Decision`)
#classified as character#
class(asylum_work_in_progress$`Asylum Appeal Outstanding`)
class(asylum_work_in_progress$`Subject to Removal Action`)
class(asylum_work_in_progress$`On Hold`)
#classified as character#
class(asylum_work_in_progress$`Further Leave Application Outstanding`)


----##Backlog by Age##----
backlogbyage <- asylum_work_in_progress %>%
  select(Date, `Case Age: 0:12 Months`,`Case Age: 12:24 Months`,`Case Age: 24:36 Months`,`Case Age: 36+ Months`)

view(backlogbyage)

----##Backlog by Age Graph, which needs legend edited##----   
  
###To get rid of the backlog by agegroup legend and to make it BRC colours, add bracket between y=case...),colour..##
  #Trouble shoot this issue with Matt#

backlogxage<-  ggplot(backlogbyage) +
  geom_line(aes(x = Date, y = `Case Age: 0:12 Months`, colour = get_brc_colours()$red)) +
  geom_line(aes(x = Date, y = `Case Age: 12:24 Months`, colour = get_brc_colours()$teal)) +
  geom_line(aes(x = Date, y = `Case Age: 24:36 Months`, colour = get_brc_colours()$green)) +
  geom_line(aes(x = Date, y = `Case Age: 36+ Months`, colour = get_brc_colours()$black)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  theme_classic() +
  theme(
    legend.position = "right",
    # legend.box = "vertical",
    #legend.margin = margin(),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 12)
  )

backlogxage +
  labs(title = "Backlog of Applications by Age group",
                   x = "Year",
                    y = "Number of Applications in Progress",
                    caption = "British Red Cross analysis of Home Office data")
  

----##Backlog by Sex##----
BacklogbySex <- asylum_work_in_progress %>%
  select(Date, `Gender: Male`, `Gender: Female`, `Gender: Unknown`)

#check class of variable by using class()
#Note that Gender:unknown is character and not numberic. Has to be converted to
#numeric as seen below, but issue arises with NA? 

#asylum_work_in_progress$`Gender: Unknown` <- as.numeric(asylum_work_in_progress$`Gender: Unknown`)

----###By Sex Graph###----
ggplot(BacklogbySex) +
  geom_line(aes(x = Date, y = `Gender: Male`), colour = "red") +
  geom_point(aes(x = Date, y = `Gender: Male`), colour = "red") +
  geom_line(aes(x = Date, y = `Gender: Female`), colour = "black") +
  geom_point(aes(x = Date, y = `Gender: Female`), colour = "black") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
  theme_classic() +
  labs(title = "Backlog of Applications by Sex group",
       x = "Year",
       y = "Number of Applications in Progress",
       caption = "British Red Cross analysis of Home Office data")

----#Applicants Awaiting Decisions#----
#This has been included in the interactive graph- would this be easier to do 
#high level: countries with the highest numbers awaiting decisions?# 
----##Graph of Overall Time for Initial Decisions##----
view(DecisionTime)

DecisionTime <- awaiting_decision %>%
  select(Date, Nationality, Duration, `Application stage`, Applications) %>%
  group_by(Date, Duration, Nationality) %>%
  summarise(Total = sum(Applications))

DecisionTime <- DecisionTime %>%
  group_by(Date, Duration) %>%
  summarise(Total = sum(Total))

DecisionTime |>
  ggplot(aes(Date, Total)) +
  geom_line(aes(colour = Duration)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  theme_classic()+
  labs(title = "Duration for Decision on Applications", 
       x = "Date", 
       y = "Total Applications")

----##Applicants Waiting Over 6 Months##----  

AwaitingDecisions6Mon <- awaiting_decision %>%
  select(Date, Nationality, Duration, Applications) %>%
  filter(Duration == 'More than 6 months')

AwaitingDecisions6Mon <- AwaitingDecisions6Mon %>%
  group_by(Date, Nationality) %>%
  summarise(ByNationality = sum(Applications))

AwaitingDecisions6Mon %>%
  group_by(Date)%>%
  filter(ByNationality > 10000)
 
#Albania is the country that continuously has the greatest number of applicants over 6 months.) 

AwaitingDecisions6Mon %>%
  filter(Nationality == 'Albania') %>%
  ggplot(aes(Date, ByNationality)) +
  geom_line(aes(colour = "red")) +
  geom_point(aes(colour = "red")) +
  theme_classic()+
  labs(title = "Albania is the country with the highest number of applicants waiting for a decision over 6 months", x = "Date", y = "Number of Applications")

----###Awaiting Decisions- Dependent###----

AwaitingDecisionsDependant <- awaiting_decision %>%
  select(Date, Nationality, Duration, Applications, `Applicant type`) %>%
  filter(`Applicant type` == 'Dependant')

DependantMorethan6Month <- AwaitingDecisionsDependant %>%
  filter(Duration == 'More than 6 months')%>%
  group_by(Date, Nationality) %>%
  summarise(Morethan6monthtotal = sum(Applications))

view(DependantMorethan6Month)

DependantMorethan6Month |>
  filter(Morethan6monthtotal > 1000) |>
  ggplot(aes(Nationality, Morethan6monthtotal)) +
  geom_line(aes(Nationality), alpha = 0.5, colour = "red")


----#QUESTION:HOW MANY PEOPLE HAVE ARRIVED AND HAVE BEEN GRANTED PROTECTION UNDER SAFE ROUTES?#----
----#Safe Routes Analysis- Resettlement Asylum Case#----

view(decisions_resettlement)

colnames(decisions_resettlement)

DecisionsAsylum <- decisions_resettlement %>%
  select(Year, Age, Sex, Nationality, `Case type`, `Case outcome group`, `Case outcome`, Decisions) %>%
  filter(Year > 2008)

view(DecisionsAsylum)

DecisionsAsylumAge <- DecisionsAsylum %>%
  group_by(Year, Age, Nationality, `Case outcome group`) %>%
  summarise(caseoutcomesum = sum(Decisions))

view(DecisionsAsylumAge)

DecisionsAsylumAge <- DecisionsAsylumAge %>%
  group_by(Year, Age, `Case outcome group`) %>%
  summarise(Total = sum(caseoutcomesum))

DecisionsAsylumAge |>
  ggplot(aes(Year, Total)) +
  geom_line(aes(colour = Age), show.legend = TRUE) +
  theme_classic()+
  labs(title = "Resettlement: Asylum Case Outcome Totals", 
       x = "Year", 
       y = "Total Decisions")

----#Irregular Migration#----
----##Small Boat##----
smallboat <- irregular_migration %>%
  filter(`Method of entry`== "Small boat arrivals") 
  
view(smallboat)

----#Small Boat x Gender#----
smallboat %>%
  select(Date, Sex, `Age Group`, Nationality, `Number of detections`)%>%
  group_by(Date, Sex) %>%
  summarise(TotalSex = sum(`Number of detections`)) |>
  ggplot(aes(Date, TotalSex)) +
  geom_line(aes(colour = Sex)) +
  theme_classic() +
  labs(title = "Small Boat Crossing by Gender", x = "Date", y = "Number of Detections")
  
----#Small Boat x Age Group#----
smallboat %>%
  select(Date, Sex, `Age Group`, Nationality, `Number of detections`) %>%
  group_by(Date, `Age Group`) %>%
  summarise(TotalAge = sum(`Number of detections`)) |>
  ggplot(aes(Date, TotalAge)) +
  geom_line(aes(colour = `Age Group`)) +
  theme_classic() +
  labs(title = "Small Boat Crossing by Age Group",
       x = "Date", 
       y = "Number of Detections")

----###Small Boat -To revise Nationalities###---- 
smallboat %>%
  select(Date, Year, Sex, `Age Group`, Nationality, `Number of detections`) %>%
  filter(Date > "2021-10-01")
  group_by(Date, Nationality) %>%
  summarise(TotalNationality = sum(`Number of detections`)) %>%
  ggplot(aes(Date, TotalNationality)) +
  geom_point(aes(colour = Nationality)) +
  theme_classic() +
  labs(title = "Small Boat Crossing by Nationality",
       x = "Date", 
       y = "Number of Detections")

irregular_migration %>%
  select(Date, `Method of entry`, `Number of detections`) %>%
  group_by(Date, `Method of entry`) %>%
  summarise(TotalbyMethod = sum(`Number of detections`)) %>%
  ggplot(aes(Date, TotalbyMethod)) +
  geom_line(aes(colour = `Method of entry`)) +
  theme_classic() +
  labs(title =  "Irregular Migration by Method of Entry",
       x = "Date",
       y = "Number of Detections")

----#Family Reunification#----

view(family_reunion)

Totalfamreunion <- family_reunion %>%
  select(Year, `Visas granted`) %>%
  group_by(Year) %>%
  summarise(Total = sum(`Visas granted`)) %>%
  ggplot(aes(Year, Total)) +
  geom_line(aes(colour = "red"), show.legend = FALSE) +
  geom_point(aes(colour = "red", alpha = 0.5, size = Total), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(2)) +
  theme_classic() +
  labs(title = "Number of Family Reunion Visas Granted",
       x = "Year",
       y = "Number of Visas Granted")

Totalfamreunion + scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))


----#Cost & Productivity#----
view(asylum_costs_and_productivity) 

asylum_costs_and_productivity %>%
  pivot_longer()

#Setting up data frame#

asylum_costs_and_productivity |>
  ggplot() +
  geom_bar()
  ggplot(aes(x =`Financial Year`), colour = "red") +
  geom_line(aes(y = `Total Asylum Costs`), colour = "green") +
  geom_line(aes(y = Productivity), colour = "blue") +
  geom_line(aes(y = `Asylum Caseworking Staff`), colour = "black")
  

----#CODE THAT DOESNT WORK WELL YET- TO BE REVISED#----
----#Nationalities x Age x Sex#----##REVIESE THIS!!!!!######
ByThree <- applications %>%
  select(Year, Age, Sex, Nationality, Applications) %>%
  filter(Year > '2008')

AllThree <- ByThree %>%
  group_by(Year, Sex, Nationality) %>%
  summarise(ByThreeSum = sum(Applications))

view(AllThree)

AllThreeonGraph <- AllThree |>
  ggplot(aes(Year, Nationality), group = 'Sex') +
  geom_point(aes(colour = Sex), size = 1)
AllThreeonGraph

##----would be best to take away nationality from the report and keep it on the interactive website alone----#

