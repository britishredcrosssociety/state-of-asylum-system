#Running Analyses by Question and Area#

library(tidyverse)
library(asylum)
library(networkD3)
library(streamgraph)
library(ggfittext)
library(ggtext)
library(IMD)
library(cartography)
library(choroplethr)
library(leaflet)
library(raster)
library(rgdal)
source("https://github.com/matthewgthomas/brclib/raw/master/R/colours.R")

----#Code that works without issue & creates good graphs#----

library(tidyverse)
library(asylum)
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

----#Top 5 Nationalities 2020 - 2023# TO BE REVISED----

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

----#Nationalities Stream Plot#----

NationalitiesTotal <- applications %>%
  group_by(Date, Year, Quarter, Nationality) %>%
  summarise(Total = sum(Applications))

NationalitiesTotal %>%
streamgraph("Nationality", "Total", "Date") %>%
  sg_axis_x(1, "Date", "%Y") %>%
  sg_fill_brewer("Spectral") %>%
  sg_legend(show=TRUE, label="Nationalities: ")

#Last 12 Months Nationalities StreamGraph
NationalitiesTotal %>%
  filter(Year > 2021) %>%
  streamgraph("Nationality", "Total", "Date") %>%
  sg_axis_x(tick_units = "Date", tick_interval = 10, tick_format = "%Y") %>%
  #sg_axis_y(1, "Total") %>%
  sg_fill_brewer("Spectral") %>%
  sg_legend(show=TRUE, label="Nationalities: ")

#Last 12 Months Nationalities 
NationalitiesTotal %>%
  filter(Year > 2021) |> 
  ggplot(aes(Date, Total), group = Nationality, colour = Nationality) +
  geom_area(aes(group = Nationality), colour = Nationality) +
  theme_classic()

Nat22 <- NationalitiesTotal %>%
  filter(Year > 2021)

ggplot(Nat22, aes(x = Date, y = Total, fill = Nationality)) +
  geom_area(show.legend = FALSE) +
  theme_classic()

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
  geom_text(aes(label = scales::comma(TotalReturn)), show.legend = FALSE, size = rel(3)) +
  scale_x_continuous(breaks = c(2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  theme_classic() +
  labs(title = 'Total Numbers of Returns per Year', 
       x = 'Date', 
       y = 'Number of Returns', 
       caption = 'BRC Analysis of UK Home Office Data. Mock analyses')


#Returns Quarterly

ReturnsQ <- returns %>%
  select(Year, Quarter, `Number of returns`) %>%
  group_by(Year, Quarter) %>%
  summarise(TotalQReturn = sum(`Number of returns`))

ReturnsQ %>%
  filter(Year > 2021) |>
  ggplot(aes(Quarter, TotalQReturn)) +
  geom_line(aes(colour = "red"), show.legend = NULL) +
  geom_text(aes(label = scales::comma(TotalQReturn)), show.legend = FALSE, size = rel(3)) +
  scale_x_continuous(breaks = c(1, 2, 3, 4)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  theme_classic() +
  labs(title = 'Total Numbers of Returns per Quarter', 
       x = 'Quarter', 
       y = 'Number of Returns', 
       caption = 'BRC Analysis of UK Home Office Data. Mock analyses')
  
# Can examine what nationalities we see at different quarters, but may just be a better question to pose to team.#
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

#Inadmissable Sankey#


# Create sample data
InadmissibleforSankey <- data.frame(inadmissibility_cases_considered,
  Year = c("2021", "2022", "2023"),
  stage = c("Stage 1", "Stage 2", "Stage 1", "Stage 2", "Stage 1"),
  count = c(100, 70, 80, 50, 120)
)

# Create Sankey diagram
sankey(data, from = country, to = stage, weight = count)


----#QUESTION: HOW MANY PEOPLE HAVE BEEN GRANTED PROTECTION IN THE UK HAVING ARRIVED THROUGH SAFE ROUTES? 

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
  

----#Resettlement by Local Authority Map 

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







----#Age Dispute#----

AgeDispute12Month <- age_disputes %>%
  filter(Year > 2021)

#Age Dispute by Resolved Reason# 

AgeDispute12Month <- AgeDispute12Month %>%
  group_by(Year, Quarter, Nationality, `Raised type / Resolved outcome`) %>%
  summarise(Total = sum(`Age disputes`))

ByDispute <- AgeDispute12Month %>%
  group_by(Year, Quarter, `Raised type / Resolved outcome`) %>%
  summarise(TotalDispute = sum(Total))

ggplot(ByDispute, aes(fill = `Raised type / Resolved outcome`, y = TotalDispute, x = Quarter)) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  labs(title = "Age Dispute by Outcome", x = "Quarter of 2022", y = "Total Persons")
  

#Age Disputes Total Over All Years#

TotalDisputes <- age_disputes %>%
  group_by(Year, Quarter) %>%
  summarise(Total = sum(`Age disputes`))
  
TotalDisputes |>
ggplot(aes(fill = Quarter, y = Total, x = Year)) +
  geom_bar(position ="stack", stat="identity") +
  theme_classic() +
  labs(title =  "Number of Age Disputes by Quarter", 
       x = "Year", 
       y = "Number of Age Disputes") +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) 

CTotalDisputes <- TotalDisputes%>%
  group_by(Year) %>%
  summarise(CTotal = sum(Total)) 

CTotalDisputes |>
  ggplot(aes(x = Year, y = CTotal)) +
  geom_line(colour = "red") +
  geom_point( alpha = 0.5, colour = "red") +
  theme_classic() +
  labs(title = "Total Age Disputes", 
       x = "Year", 
       y = "Total Age Disputes") +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) 

  

#Age Dispute Nationality- REVISE#
AgeDispute12Month %>%
  group_by(Nationality, Year) %>%
  summarise(TotalNat = sum(Total)) |>
  ggplot(aes(Year, TotalNat)) +
  #geom_line(aes(colour = Nationality, group = Nationality))
  geom_point(aes(colour = Nationality, size = TotalNat), alpha = 0.5) +
  geom_text(aes(label = scales::comma(TotalNat)), show.legend = FALSE, size = rel(3)) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  theme_classic() +
  guides(color = guide_legend(override.aes = list(size = 0.5)))


----#Section 95 Support#----
----##Support Applications#----

Section95Apps <- support_applications %>%
  select(Year, Nationality, `Support type granted`, `Group type`, Applications) %>%
  group_by(Year, Nationality, `Support type granted`, `Group type`) %>%
  summarise(Total = sum(Applications))

TotalSection95Apps <- Section95Apps %>%
  group_by(Year, `Support type granted`,`Group type`) %>%
  summarise(Total95 = sum(Total))

#Applications per Support Type Granted# 

TotalSection95Apps |>
  ggplot(aes(fill = `Support type granted`, y = Total95, x = Year)) +
  geom_bar(position ="stack", stat="identity") +
  theme_classic() +
  labs(title = "Total Number of Applications for Section 95 by Support Type", 
       x = "Year", 
       y = "Number of Applications")
  scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))


#Applications per Group Type x Support Type in 2022#
  TotalSection95Apps %>%
    filter(Year == 2022) |>
    ggplot(aes(fill = `Support type granted`, y = Total95, x = `Group type`)) +
    geom_bar(position ="stack", stat="identity") +
    theme_classic() +
    labs(title = "Total Number of Applications for Section 95 by Group Type, 2022", 
         x = "Group Type", 
         y = "Number of Applications") +
  #scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
    scale_y_continuous(labels = scales::comma, limits = c(0, NA))
  

#Section 95 by Nationality# 
  
Nationalities95 <- Section95Apps %>%
  group_by(Year, Nationality) %>%
  summarise(TotalNat = sum(Total)) 

#In 2020 and 2022, of all the Section95 applications made, Unknown was the highest group in Nationality. Does this even fit with the narrative? 

----##Section 95 Received##----
view(support_received)

Section95Recieved <- support_received %>%
  select(Date, `Support Type`, `Accommodation Type`, "UK Region" , People) 

Section95Recieved <- Section95Recieved %>%
  mutate(Year = lubridate::year(Date))

TotalSection95Recieved <- Section95Recieved %>%
  group_by(Year) %>%
  summarise(Total = sum(People))

TotalSection95Recieved |>
  ggplot(aes(Year, Total)) +
  geom_line(aes(colour = "red"), show.legend = FALSE) +
  geom_point(aes(colour = "red", size = Total, alpha = 0.5), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(2)) +
  theme_classic() +
  labs(title = "Total Number of Section 95 Support Recieved per Year", 
       x = "Year", 
       y = "Number of People") +
  scale_x_continuous(breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))

SupportType <- SupportType %>%
  mutate(Year = lubridate::year(Date))

SupportType <- Section95Recieved %>%
  group_by(Year,`Support Type`,`Accommodation Type`) %>%
  summarise(Total = sum(People))  

SupportType |>
  ggplot(aes(fill = `Support Type`, y = Total, x = Year)) +
  geom_bar(position ="stack", stat="identity") +
  theme_classic() +
  labs(title = "Support for Refugees by by Support Type", 
       x = "Year", 
       y = "Number of People") +
  scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))


#2022/23 Section and Accommodation Type 
SupportType %>%
  filter(Year == 2022) |>
  ggplot(aes(fill = `Accommodation Type`, y = Total, x = `Support Type`)) +
  geom_bar(position ="stack", stat="identity") +
  theme_classic() +
  labs(title = "Accomodation by Support Type, 2022", 
       x = "Accomodation Type", 
       y = "Number of People") +
  #scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  guides(color = guide_legend(override.aes = list(size = 0.5)))


----#Destitution#----

Destitution <- support_received %>%
  select(Date, `Support Type`, `Accommodation Type`, "UK Region" , People) 

Destitution <- Destitution %>%
  mutate(Year = lubridate::year(Date))

TotalDestitute <- Destitution %>%
  group_by(Year,`Support Type`) %>%
  summarise(Total = sum(People))

TotalDestitute |>
ggplot(aes(x = Year,
           y = Total, 
           fill = `Support Type`)) +
  geom_area() +
  theme_classic() +
  labs(title = "Destitution by Support Type",
       x = "Year",
       y = "Number of People") +
  scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))

----#QUESTION: What is the scale of human trafficking in the UK and what is the provision of support to survivors?#----

----#Referral to NRM:----
nrm_referrals <- 
  bind_rows(
    asylum::nrm_referrals_2023_q1,
    asylum::nrm_referrals_2022_q4,
    asylum::nrm_referrals_2022_q3,
    asylum::nrm_referrals_2022_q2
  )

ByFirstResponder <- nrm_referrals |> 
  filter(`Exploitation type` == "Total" & `Age at exploitation` == "Total" & Gender == "Total" & Nationality == "Total") |> 
  filter(
    (`First responder type` == "Government agency" & `First responder` != "Government agency total") |
      (`First responder type` != "Government agency" & str_detect(`First responder`, "total"))
  ) |> 
  
  mutate(`First responder type` = if_else(`First responder type` == "Government agency", `First responder`, `First responder type`)) |> 
  
  
  group_by(`First responder type`) |> 
  summarise(People = sum(People)) 

 
  #NRM by group#
  
NRMTotal <- nrm_referrals %>%
  group_by(`First responder`, `Exploitation type`, `Age at exploitation`, Gender, Nationality) %>%
  summarise(Total = sum(People))

#NRM by Age in 2022/23#

NRMAge <- nrm_referrals |> 
  filter(`Age at exploitation` != "Total" & Nationality == "Total") |> 
  filter(Gender != "Total") |> 
  
  group_by(`Age at exploitation`, Gender) |> 
  summarise(Total = sum(People)) 

NRMAge |>
  ggplot(aes(fill = `Age at exploitation`, y = Total, x = Gender)) +
  geom_bar(position ="stack", stat="identity") + 
  theme_classic() +
  labs(title = "NRM Referrals by Age and Sex, 2022/23", 
       x = "Sex", 
       y = "Number of Referrals") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))

#NRM by Nationality- tO REVISE ON HOW TO SHOW. 

nrm_referrals <- nrm_referrals |> 
  filter(Nationality != "Total") |> 
  filter(str_detect(`First responder`, "total")) |> 
  
  group_by(Nationality) |> 
  summarise(People = sum(People)) |> 
  ungroup() |> 
  
  arrange(desc(People)) 

nrm_referrals |>
 ggplot(aes(x = Nationality, y = People)) +
 geom_point(alpha = 0.5)

PositiveConclusive <- nrm_conclusive_grounds %>%
  select(Year, Quarter, `Adult (18 or over) - Positive conclusive grounds`, `Child (17 or under) - Positive conclusive grounds`, `Age not specified or unknown - Positive conclusive grounds`) 
  
PC <- PositiveConclusive %>%
  filter(Quarter == "Total") |>
  ggplot(aes(x = Year)) +
  geom_line(aes(x = Year, y = `Adult (18 or over) - Positive conclusive grounds`), colour = "red", show.legend = TRUE) +
  geom_line(aes(x = Year, y = `Child (17 or under) - Positive conclusive grounds`), colour = "blue", show.legend = TRUE) +
  geom_line(aes(x = Year, y = `Age not specified or unknown - Positive conclusive grounds`), colour = "black", show.legend = TRUE) +
  theme_classic() +
  labs(title = "Number of People who Recieved Positive Conclusive Grounds", 
       x = "Year", 
       y = "Number of People") +
  scale_x_continuous(breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +

PC + scale_color_manual(name = "Age Groups", 
                     values = (c("Adult (18 or over) - Positive conclusive grounds" = "red", 
                                "Child (17 or under) - Positive conclusive grounds" = "blue", 
                                "Age not specified or unknown - Positive conclusive grounds" = "black"))) + 
  labs(color = 'Age Groups')

#To fix legend 

#NRM Reasonable Grounds# 

view(nrm_reasonable_grounds)

ReasonableGrounds <- nrm_reasonable_grounds %>%
  select(Year, Quarter, `Adult (18 or over) - Positive reasonable grounds`, `Child (17 or under) - Positive reasonable grounds`, `Age not specified or unknown - Positive reasonable grounds`) 

RGPlot <- ReasonableGrounds %>%
  filter(Quarter == "Total") |>
  ggplot(aes(x = Year)) +
  geom_line(aes(x = Year, y = `Adult (18 or over) - Positive reasonable grounds`), colour = "red") +
  geom_line(aes(x = Year, y = `Child (17 or under) - Positive reasonable grounds`), colour = "blue") +
  geom_line(aes(x = Year, y = `Age not specified or unknown - Positive reasonable grounds`), colour = "black") +
  theme_classic() +
  labs(title = "Number of People who Recieved Positive Reasonable Grounds", 
       x = "Year", 
       y = "Number of People") +
  scale_x_continuous(breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
 
RGPlot <- scale_colour_manual("", 
                      breaks = c("Adult (18 or over) - Positive reasonable grounds", "Child (17 or under) - Positive reasonable grounds", "Age not specified or unknown - Positive reasonable grounds"),
                      values = c("red", "green", "blue")) 
  xlab(" ") +
  scale_y_continuous("Temperatura (C)", limits = c(-10,40)) + 
  labs(title="TITULO")
  
#Fix the legend

----##NRM Duty to Notify#----

view(nrm_duty_to_notify_2022_q2)

nrm_duty_to_notify <- bind_rows(
    asylum::nrm_duty_to_notify_2023_q1,
    asylum::nrm_duty_to_notify_2022_q4,
    asylum::nrm_duty_to_notify_2022_q3,
    asylum::nrm_duty_to_notify_2022_q2
  )

DTNTotal <- nrm_duty_to_notify %>%
  group_by(Nationality) %>%
  filter(Nationality != "Total")

DTNTotal <- DTNTotal %>%
  group_by(Nationality) %>%
  summarise(Total2 = sum(Total))

DTNTotal %>%
  filter(Total2 > 100) %>%
  arrange(desc(Total2)) %>%
  ggplot(aes(Nationality, Total2)) +
  geom_point(aes(size = Total2, alpha = 0.5, colour = "red"), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total2)), show.legend = FALSE, size = rel(3)) +
  theme_classic() +
  labs(title = "Duty to Notitify by Nationalities (Top 10)", 
       x = "Nationalities", 
       y = "Number of People") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))




----##---- 
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



----#CODE THAT IS REMOVED#----
----#Returns Graphs that the team stated to remove#----
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
