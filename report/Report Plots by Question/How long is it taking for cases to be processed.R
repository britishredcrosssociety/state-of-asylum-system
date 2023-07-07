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


BacklogGraph <- (ggplot(Backlog) +
  geom_col(aes(x = Date, y = `Total Work In Progress`), colour = brc_colours$red_dunant, fill = brc_colours$red_dunant)  +
  theme_classic() +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title = "Backlog of Cases", 
       subtitle = "In 2022, the backlog reached 166,085 cases",
       x = NULL, 
       y = " Total Cases", 
       caption = "BRC Analyses of HO Data, March 2023")) 


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
  dplyr::mutate(year = lubridate::year(Date))

DecisionTime <- DecisionTime %>%
  group_by(year, Duration) %>%
  summarise(Total = sum(Total))

DecisionTime <- DecisionTime %>%
  filter(Duration != 	"N/A - Further review")

#Decision Plot 
DecisionTimeG <- ggplot(DecisionTime, aes(fill = Duration, x = year, y = Total)) +
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  labs(title = "Length of Time for an Initial Decision on Asylum Applications", 
       x = NULL, 
       y = "Applications", 
       caption = "BRC Analyses of HO Data, March 2023")

DecisionTimeG <- DecisionTimeG + scale_y_continuous(labels = scales::comma, limits = c(0, NA))

DecisionTimeG + scale_fill_manual(values = c(brc_colours$red_dunant,
                                            brc_colours$red_deep)) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023))


view(awaiting_decision)


----##Applicants Waiting Over 6 Months##----  

awaiting_decision <-  awaiting_decision %>%
  dplyr::mutate(Year = lubridate::year(Date))

AwaitingDecisions6Mon <- awaiting_decision %>%
  select(Year, Nationality, Duration, Applications) %>%
  filter(Duration == 'More than 6 months')

AwaitingDecisions6Mon <- AwaitingDecisions6Mon %>%
  group_by(Year, Nationality) %>%
  summarise(ByNationality = sum(Applications))

AwaitingDecisions6Mon %>%
  group_by(Year)%>%
  filter(Year == 2023)

#Albania is the country that continuously has the greatest number of applicants over 6 months.) 