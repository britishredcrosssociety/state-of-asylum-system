#Running Analyses by Question and Area#

library(tidyverse)
library(asylum)
library(ggfittext)
library(ggtext)
library(IMD)
source("https://github.com/matthewgthomas/brclib/raw/master/R/colours.R")

----#Code that works without issue & creates good graphs#----

----#QUESTION: WHO IS APPLYING FOR ASYLUM IN THE UK (NATIONALITY, SEX, AGE, UASC, KIDS)#----
----#Total Asylum Applications#----

TotalApps <- applications %>%
  group_by(Year) %>%
  summarise(Total = sum(Applications))

view(TotalApps)

TotalApps |>
  ggplot(aes(Year, Total)) +
  geom_line(colour = "red") +
  geom_point(aes(size = Total, alpha = 0.4, colour = 'red'), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(4)) +
  theme_classic() +
  scale_x_continuous(breaks = c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title = "Total Number of Asylum Applications", 
       x = "Year", 
       y = "Total Number of Applications", 
       caption = "BRC Mock analyses 2023, Q1")

----#Total Asylum Applications Quarterly Analysis#----

TotalQApps <- applications %>%
  select(Quarter, Year, Applications) %>%
  filter(Year > 2021) %>%
  group_by(Quarter) %>%
  summarise(Total = sum(Applications))

view(TotalQApps)

TotalQApps |>
  ggplot(aes(Quarter, Total, group = 1)) +
  geom_line(aes (colour = "red"), show.legend = FALSE) +
  geom_point(aes(size = Total, alpha = 0.4, colour = 'red'), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(4)) +
  theme_classic() +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title = "Total Number of Asylum Applications, by Quarter", 
       x = "Quarter", 
       y = "Total Number of Applications", 
       caption = "BRC Mock analyses 2023, Q1")

----#Top 5 Nationalities 2020 - 2023#----

ByNationality <- applications %>%
  group_by(Nationality, Year) %>%
  summarise(Total = sum(Applications))

view(ByNationality)

Top20 <- ByNationality |>
  filter(Year == 2020, Total>2000) |>
  ggplot(aes(Nationality, Total)) +
  geom_point(aes(colour = "red"), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(4)) +
  theme_classic() +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title = "Top 5 Nationalities Applying for Asylum in 2020", 
       x = "Nationalities", 
       y = "Number of Applications")

Top21 <- ByNationality |>
  filter(Year == 2021, Total >3500) |>
  ggplot(aes(Nationality, Total)) +
  geom_point(aes(colour = "red"), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(4)) +
  theme_classic() +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title = "Top 5 Nationalities Applying for Asylum in 2021", 
       x = "Nationalities", 
       y = "Number of Applications")

Top22 <- ByNationality |>
  filter(Year == 2022, Total>4500) |>
  ggplot(aes(Nationality, Total)) +
  geom_point(aes(colour = "red")) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(4)) +
  theme_classic() +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title = "Top 5 Nationalities Applying for Asylum in 2022", 
       x = "Nationalities", 
       y = "Number of Applications")

Top23 <- ByNationality |>
  filter(Year == 2023, Total>1000) |>
  ggplot(aes(Nationality, Total)) +
  geom_point(aes(colour = "red"), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(4)) +
  theme_classic() +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title = "Top Nationalities Applying for Asylum in 2023, Q1", 
       x = "Nationalities", 
       y = "Number of Applications")

install.packages("cowplot")


cowplot::plot_grid(Top20, Top21, Top22, Top23, labels = c(), label_size = 12)

----#Age Analysis#----
----##Age All Years Analysis#----
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
    caption = "BRC Mock Analysis for 2023, Q1"
  )

AllAgesAllYears + scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023))

----##Last 12 Months- Age Analysis#----
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
  
----#Sex Analysis#----##----
SexAnalysis <- applications %>%
  select(Year, Sex, Applications, Nationality, Age, Quarter) %>%
  filter(Year > '2008', Sex != 'Unknown Sex')

view(SexAnalysis)

SexAnalysis <- SexAnalysis %>%
  group_by(Sex, Year) %>%
  summarise(SexGroupSum = sum(Applications))

AllSAllYears <- SexAnalysis |>
  ggplot(aes(Year, SexGroupSum)) +
  geom_line(aes(colour = Sex)) +
  geom_point(aes(colour = Sex), size = 1) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_color_manual(values = c(get_brc_colours()$teal, get_brc_colours()$red)) +
  scale_fill_manual(values = c(get_brc_colours()$teal, get_brc_colours()$red)) +
  theme_classic() +
  guides(fill=guide_legend(nrow=3,byrow=TRUE), color = guide_legend(nrow=3,byrow=TRUE)) +
  theme(
    legend.position = "right",
    # legend.box = "vertical",
    # legend.margin = margin(),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 12)) +
  labs(
    title = str_glue("Asylum Application by Sex All Years",
                     x = "Year",
                     y = "Number of Applicants",
                     caption = "BRC Mock Analysis 2023, Q1"
    ))

AllSAllYears + scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) + labs(x = "Year", y = "Number of Applicants")
----#UASC#----#----
UASC <- applications %>%
  select(Year, Nationality, UASC, Applications) %>%
  group_by(Year, UASC) %>%
  summarise(TotalUASC = sum(Applications))

view(UASC)

UASCOnly <- UASC %>%
  filter(Year > 2005, UASC == "UASC")

view(UASCOnly)

UASCOnly |>
  ggplot(aes(Year, TotalUASC)) +
  geom_line(colour = "red") +
  geom_text(aes(label = scales::comma(TotalUASC)), show.legend = FALSE, size = rel(4)) +
  theme_classic() +
  scale_x_continuous(breaks = c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  labs(title = "Number of UASC Applications", 
       x = "Year", 
       y = "Total Number of Applications", 
       caption = "BRC Mock analyses 2023, Q1")
  
----#Dependent Children#----#----

DependentC <- applications %>%
  select(Year, `Applicant type`, Age, Applications) %>%
  group_by(Year, `Applicant type`, Age) %>%
  summarise(TotalDC = sum(Applications))

DependentC <- DependentC %>%
  filter(`Applicant type` == "Dependant", Age == "Under 18")

view(DependentC)

DependentC |>
  ggplot(aes(Year, TotalDC)) +
  geom_line(colour = "red") +
  geom_text(aes(label = scales::comma(TotalDC)), show.legend = FALSE, size = rel(4)) +
  theme_classic() +
  labs(title = 'Number of Depedent Applications', 
       x = 'Year', 
       y = 'Total Number of Applications', 
       caption = 'BRC Mock analyses 2023, Q1') +
  scale_x_continuous(breaks = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023))

----#QUESTION: WHAT IS THE ASYLUM GRANT RATE AT INITIAL DECISION AND APPEAL?#----
----#Grant Rate and Appeals#----
GrantRatebyYear <- grant_rates_initial_annual %>%
  select(Year, Grant, Refused) %>%
  group_by(Year) %>%
  summarise(TGrant = sum(Grant),TRefused = sum(Refused))

GrantRatebyYear <- GrantRatebyYear %>%
     mutate(TotalCases = TGrant + TRefused) %>%
     mutate(GrantRate = TGrant / TotalCases) %>%
     mutate(GrantRate = (GrantRate <- GrantRate*100))

GrantRatebyYear <- ceiling(GrantRatebyYear) 

GrantRatebyYear |>
  ggplot(aes(Year, GrantRate)) +
  geom_line(aes(colour = "red"), show.legend = NULL) +
  geom_point(aes(size = GrantRate, alpha = 0.4, colour = 'red'), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(GrantRate)), show.legend = FALSE, size = rel(4)) + 
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_x_continuous(breaks = c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  theme_classic() +
  guides(fill=guide_legend(nrow=6,byrow=TRUE), color = guide_legend(nrow=6,byrow=TRUE))+
  theme(
    legend.position = "right",
    # legend.box = "vertical",
      # legend.margin = margin(),
      plot.title.position = "plot",
      plot.title = element_textbox_simple(size = 12)) +
      labs(
         title = "Grant Rates Initial -Annual",
         x = "Year",
         y = "Grant Rate (%)",
        caption = "BRC Mock Analyses until 2023, Q1")


#Grant Rate Top Nationalities#
GrantRateNationality <- grant_rates_initial_annual %>%
  filter(Year == 2023, Grant > 0, Refused > 0)
  #To discuss with the team on how they want to go by this?#


----#Appeals#----
AppealsLodgedTotal <- appeals_lodged %>%
  select(Year,`Appeals lodged`) %>%
  group_by(Year) %>%
  summarise(TotalLodged = sum(`Appeals lodged`))

AppealsLodgedTotal |>
  ggplot(aes(Year, TotalLodged)) +
  geom_line(aes(colour = "red"), show.legend = NULL) +
  geom_point(aes(size = TotalLodged, alpha = 0.4, colour = 'red'), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(TotalLodged)), show.legend = FALSE, size = rel(4)) + 
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  theme_classic() +
  guides(fill=guide_legend(nrow=6,byrow=TRUE), color = guide_legend(nrow=6,byrow=TRUE))+
  theme(
    legend.position = "right",
    # legend.box = "vertical",
    # legend.margin = margin(),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 12)) +
  labs(
    title = "Total Appeals Lodged",
    x = "Year",
    y = "Total Appeals Lodged",
    caption = "BRC Mock Analyses 2023, Q1") +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023))

----#Appeals Determined#----
ADetermined <- appeals_determined %>%
  select(Year, Outcome, `Appeals determined`) %>%
  group_by(Year, Outcome) %>%
  summarise(TDetermined = sum(`Appeals determined`))

ADetermined |>
  ggplot(aes(Year, TDetermined)) +
  geom_line(aes(colour = Outcome)) +
  geom_point(aes(colour = Outcome)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  theme_classic() +
  labs(title = "Appeals Determined by Outcome", 
       x = "Year", 
       y = "Total Number of Appeals Determined", 
       caption = "BRC Mock Analyses 2023, Q1")

#Appeals Non-Suspensive#

Nonsuspensive <- appeals_non_suspensive %>%
  select(Year, `Initial decisions from designated states`, `Refusals from designated states`, `Clearly unfounded refusals (designated states)`, `Clearly unfounded refusals (non-designated states)`, `Total eligible for the NSA process`) %>%
  group_by(Year) %>%
  summarise(IDDS = sum(`Initial decisions from designated states`), RDS = sum(`Refusals from designated states`), CDS = sum(`Clearly unfounded refusals (designated states)`), CNDS = sum(`Clearly unfounded refusals (non-designated states)`), NSATotal = sum(`Total eligible for the NSA process`))

#To revise and examine how to plot non-suspensive and where the focus should be- ie the total number of NSA or other?#


----#QUESTION: HOW MANY PEOPLE HAVE BEEN RETURNED? AND TO WHERE?#----
----#Returned#----
#Returned Total#

ReturnTotal <- returns %>%
  select(Year, `Number of returns`) %>%
  group_by(Year) %>%
  summarise(TotalReturn = sum(`Number of returns`))

ReturnTotal |>
  ggplot(aes(Year, TotalReturn)) +
  geom_line(aes(colour = "red"), show.legend = NULL) +
  geom_text(aes(label = scales::comma(TotalReturn)), show.legend = FALSE, size = rel(4)) +
  scale_x_continuous(breaks = c(2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) +
  theme_classic() +
  labs(title = 'Total Numbers of Returns per Year', 
       x = 'Date', 
       y = 'Number of Returns', 
       caption = 'BRC Analysis of UK Home Office Data. Mock analyses')

#Returns by Type#
ReturnbyType <- returns %>%
  select(Year, `Return type`, `Number of returns`) %>%
  group_by(Year, `Return type`) %>%
  summarise(TotalReturnbyType = sum(`Number of returns`))

view(ReturnbyType)

ReturnbyType |>
  ggplot(aes(Year, TotalReturnbyType)) +
  geom_line(aes(colour = `Return type`)) +
  #geom_text(aes(label = scales::comma(TotalReturnbyType)), show.legend = FALSE, size = rel(4)) +
  theme_classic() +
  labs(title = 'Returns by Return Type', 
       x = 'Date', 
       y = 'Total Number of Returns', 
       caption = 'BRC Analysis of UK Home Office Data. Mock analyses')

#check to see if they would prefer return by group- simplified# 

ReturnbyType <- returns %>%
  select(Year, `Return type group`, `Number of returns`) %>%
  group_by(Year, `Return type group`) %>%
  summarise(TotalReturnbyType = sum(`Number of returns`))

ReturnbyType |>
  ggplot(aes(Year, TotalReturnbyType)) +
  geom_line(aes(colour = `Return type group`)) +
  #geom_text(aes(label = scales::comma(TotalReturnbyType)), show.legend = FALSE, size = rel(4)) +
  theme_classic() +
  labs(title = 'Returns by Return Type Group- Simplified', 
       x = 'Date', 
       y = 'Total Number of Returns', 
       caption = 'BRC Analysis of UK Home Office Data. Mock analyses')

#Return by Destination Group#
ReturnbyDestination <- returns %>%
  select(Year, `Return destination group`, `Number of returns`) %>%
  group_by(Year, `Return destination group`) %>%
  summarise(TotalDG = sum(`Number of returns`))

ReturnbyDestination |>
  ggplot(aes(Year, TotalDG)) +
  geom_line(aes(colour = `Return destination group`)) +
  theme_classic() +
  labs(title = 'Returns by Destination Type', 
       x = 'Year', 
       y = 'Total Number of Returns', 
       caption = 'BRC Analysis of UK Home Office Data. Mock analyses')
  
#Returns by Destination Country#

returns_by_destination %>%
  select(Year, `Return destination`, `Number of returns`) %>%
  group_by(Year, `Return destination`) %>%
  summarise(DestinationTotal = sum(`Number of returns`))

#Ask if Nationality graph specifically wanted or if there can be analysis by regions?#

----#QUESTION: HOW MANY CLAIMS HAVE BEEN DEEMED INADMISSABLE?#---- 
#Inadmissible- to discuss with the team if this revised graph is better?#
Inadmissable <- inadmissibility_cases_considered %>%
  select(Date, Year, Stage, Cases, Quarter) %>%
  group_by(Date, Year, Quarter, Stage) %>%
  summarise(TotalCases = sum(Cases))
  
Inadmissable |>
  ggplot(aes(Date, TotalCases)) +
  geom_line(aes(group = Stage, colour = Stage)) +
  geom_point(aes(colour = Stage), size = 1) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_color_manual(values = c(get_brc_colours()$teal, get_brc_colours()$green, get_brc_colours()$red, get_brc_colours()$blue_light, get_brc_colours()$black)) +
  scale_fill_manual(values = c(get_brc_colours()$teal, get_brc_colours()$green, get_brc_colours()$red, get_brc_colours()$blue_light, get_brc_colours()$black)) +
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
    title = "Inadmissable Cases by Stage",
    x = "6 Month Intervals",
    y = "Number of Cases",
    caption = "British Red Cross Mock Analyses."
  )

----#TO ADD: QUESTION: HOW MANY PEOPLE HAVE BEEN DETAINED AND REMOVED SINCE MARCH 7 2023?#----
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
#this graph needs to be updated- does not work as well. 

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

----#Small Boat x Quarter#----
Smallboatbyquarter <- smallboat %>% 
  select(Date, Quarter, `Number of detections`) %>%
  group_by(Date, Quarter) %>%
  summarise(TotalQ = sum(`Number of detections`))

view(Smallboatbyquarter)

Smallboatbyquarter |>
  ggplot(aes(Date, TotalQ, group = Quarter)) +
  geom_line(aes(color = Quarter)) +
  geom_point(aes(colour = Quarter)) +
  theme_classic() +
  labs(title = "Small Boat Arrivals by Quarter", 
       x = "Year", 
       y = "Number of Small Boat Detections")

#colour need to be edited for this graph.#

----#Small Boat x Asylum Applications#----

SmallboatAsylum <- small_boat_asylum_applications %>%
  select(Year, `Age Group`, Sex, Region, Applications, `Asylum application`)
  filter("Asylum application" != "No asylum application raised")

SmallboatAsylum %>%
  select(Year, `Age Group`, Applications) %>%
  group_by(Year, `Age Group`) %>%
  summarise(SBAsyTotal = sum(Applications)) |>
  ggplot(aes(Year, SBAsyTotal)) +
  geom_line(aes(colour = `Age Group`)) +
  theme_classic() +
  labs(title = "Asylum Applications from Small Boat Arrivals",
       x = "Year", 
       y = "Number of Applications")





----###Small Boat -To revise Nationalities###---- 
smallboat %>%
  filter(Date > "2021-10-01")
  select(Date, Nationality, `Number of detections`) %>%
  group_by(Date, Nationality) %>%
  summarise(TotalNationality = sum(`Number of detections`)) %>%
  ggplot(aes(Date, TotalNationality)) +
  geom_point(aes(colour = Nationality)) +
  theme_classic() +
  labs(title = "Small Boat Crossing by Nationality",
       x = "Date", 
       y = "Number of Detections")

  
----#Irregular Migration by Method of Entry#----  
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

Totalfamreunion + scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023.1))

----#QUESTION: WHAT IS THE STATE OF DETENTION?
----#Detention Overall Analyses#----

#People Entering Detention#
OverallEnteringDetention <- people_entering_detention %>%
  select(Year, Nationality, Sex, Age, `First place of detention`, Entering) %>%
  group_by(Year) %>%
  summarise(TotalinDetention = sum(Entering))

OEDG <- OverallEnteringDetention |>
  ggplot(aes(Year, TotalinDetention)) +
  geom_line(aes(colour = "red"), show.legend = FALSE) +
  geom_point(aes(colour = "red", alpha = 0.5, size = TotalinDetention), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(TotalinDetention)), show.legend = FALSE, size = rel(2)) +
  theme_classic() +
  labs(title = "People Entering Detention (2010 - 2022)",
       x = "Year",
       y = "Number of People Entering")
OEDG + scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))

#People in Detention#
OverallinDetention <- people_in_detention %>%
  select(Year, Quarter, Age, Sex, `Length of detention`, People) %>%
  group_by(Year, Quarter) %>%
  summarise(TotalinDetention = sum(People))

OverallinDetention |>
  ggplot(aes(Year, TotalinDetention)) +
  geom_line(aes(colour = "red"), show.legend = FALSE) +
  geom_point(aes(colour = "red", alpha = 0.5, size = TotalinDetention), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(TotalinDetention)), show.legend = FALSE, size = rel(2)) +
  theme_classic() +
  labs(title = "People in Detention (2010 - 2022)",
       x = "Year",
       y = "Number of People") +
scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))


#Detention by Quarter- Stacked bar chart#
OverallinDetention %>%
  #filter(Year > 2021) |>
  ggplot(aes(fill = Quarter, y = TotalinDetention, x = Year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  #scale_x_continuous(breaks = c(2022, 2023)) +
  theme_classic() +
  labs(title = "People in Detention by Quarter", x = "Year", y = "Total Cases")

#Detention Last 12 months

Detention12M <- people_in_detention %>%
  filter(Year > 2021) %>%
  select(Year, Date, Quarter, People) %>%
  group_by(Date) %>%
  summarise(Total12M = sum(People))

Detention12M |>
  ggplot(aes(Date, Total12M)) +
  geom_line() +
  geom_point() +
  theme_classic() 
  



#Length of Detention#
LengthinDetention <- people_in_detention %>%
  select(Year, `Length of detention`, People) %>%
  group_by(Year, `Length of detention`) %>%
  summarise(LengthTotal = sum(People))

LengthinDetention |>
  ggplot(aes(Year, LengthTotal)) +
  geom_line(aes(colour = `Length of detention`)) +
  theme_classic() +
  labs(title = "Length in Detention",
        x = "Year",
        y = "Number of People") +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))
#To revise length and colours for graph as too messy- discuss with team on what time periods to include#

#Detention and Age# 

AgeDetention <- people_in_detention %>%
  select(Year, Age, People) %>%
  group_by(Year, Age) %>%
  summarise(AgeTotal = sum(People))

AgeDetention |>
  ggplot(aes(Year, AgeTotal)) +
  geom_line(aes(colour = Age)) +
  theme_classic() +
  labs(title = "Age of those in Detention",
       x = "Year", 
       y = "Number of People", 
       caption = "BRC Mock Analysis")

#Detention by Sex#
DetentionbySex <- people_in_detention %>%
  select(Year, Sex, People) %>%
  group_by(Year, Sex) %>%
  summarise(Total = sum(People))

DetentionbySex |>
  ggplot(aes(Year, Total)) +
  geom_line(aes(colour = Sex)) +
  theme_classic() +
  labs(title = "Sex of those in Detention", 
       x = "Year", 
       y = "Number of People", 
       caption = "BRC Mock Analysis") +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023))

#Detention Pregnant Women#

detention_pregnant_women %>%
  select(Year, `Number of pregnant women detained in the immigration detention estate`) %>%
  group_by(Year) %>%
  summarise(Total = sum(`Number of pregnant women detained in the immigration detention estate`)) |>
  ggplot(aes(Year, Total)) +
  geom_line(aes(colour = "red")) +
  geom_point(aes(colour = "red")) +
  theme_classic() +
  labs(title = "Number of Pregnant Women in Detention",
       xlab = "Year", 
       ylab = "Number")







----#TO BE REVISED CODE#----
----#Cost & Productivity- to be revised#----
view(asylum_costs_and_productivity) 

asylum_costs_and_productivity |>
  ggplot(aes(`Financial Year`, `Total Asylum Costs`)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title = "Total Asylum System Cost", x = "Financial Year", y = "Cost")
  
----#Local Authority and Refugee Support#----

----##Map of Refugee Support and Local Authority- taken from Ukraine analysis##----

library(demographr)
library(geographr)
library(viridis)
library(sf)

colnames(local_authority_support)

local_authority_support |>
  select(`LAD Code`, People) |>
  left_join(demographr::households21_ltla21)

  homelessness_24feb_24mar |> 
  select(lad_code, homeless = `Total Ukrainian households owed a prevention or relief duty`) |> 
  left_join(demographr::households21_ltla21, by = c("lad_code" = "ltla21_code")) |> 
  mutate(homeless_per_100000 = homeless / households * 100000)

homelessness_latest_shp <- 
  geographr::boundaries_ltla21 |> 
  filter(str_detect(ltla21_code, "^E")) |> 
  left_join(homelessness_latest, by = c("ltla21_code" = "lad_code"))

----#Map showing absolute numbers of Ukraine homelessness by Local Authority ----
homelessness_latest_shp |> 
  ggplot() +
  geom_sf(
    aes(fill = homeless),
    colour = "#5c747a"
  ) +
  scale_fill_gradient(low = "#f6f6f6", high = "#ee2a24") +
  # scale_fill_viridis(
  #   na.value = "transparent",
  #   option = "magma",
  #   alpha = 0.7,
  #   begin = 0.1,
  #   end = 0.9,
  #   discrete = FALSE,
  #   direction = -1
  # ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(
      hjust = 0.5,
      margin = margin(
        b = 0.3,
        t = 0.2,
        l = 2,
        unit = "cm"
      )
    ),
    plot.caption = element_text(hjust = 0.5)
  ) +
  labs(
    # title = "Local Authorities potentially requiring deeper support with homelessness",
    # subtitle = str_wrap("Showing the subset of Local Authorities where people arriving on the Ukraine visa schemes are living and rates of homelessness (including people in temporary accommodation) were already high, housing stock is low, and waiting lists for housing are long. Local Authorities are shaded by number of Ukranian arrivals (darker colours mean more people).\n", 100),
    fill = "Number of Ukrainian households at risk of homelessness",
    caption = "Source: British Red Cross analysis of DLUHC data"
  )

ggsave("images/homelessness map for PRA spotlight - absolute count.png", width = 180, height = 160, units = "mm")

----#Map showing Ukraine homelessness as a proportion of all households, by Local Authority ----
homelessness_latest_shp |> 
  # Drop Scilly - clearly there's some error in the data
  filter(ltla21_code != "E06000053") |> 
  
  ggplot() +
  geom_sf(
    aes(fill = homeless_per_100000),
    colour = "#5c747a"
  ) +
  scale_fill_gradient(low = "#f6f6f6", high = "#ee2a24") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(
      hjust = 0.5,
      margin = margin(
        b = 0.3,
        t = 0.2,
        l = 2,
        unit = "cm"
      )
    ),
    plot.caption = element_text(hjust = 0.5)
  ) +
  labs(
    # title = "Local Authorities potentially requiring deeper support with homelessness",
    # subtitle = str_wrap("Showing the subset of Local Authorities where people arriving on the Ukraine visa schemes are living and rates of homelessness (including people in temporary accommodation) were already high, housing stock is low, and waiting lists for housing are long. Local Authorities are shaded by number of Ukranian arrivals (darker colours mean more people).\n", 100),
    fill = "Proportion of Ukrainian households at risk of homelessness \n(per 100,000 households)",
    caption = "Source: British Red Cross analysis of DLUHC data"
  )

ggsave("images/homelessness map for PRA spotlight - proportion.png", width = 180, height = 160, units = "mm")


