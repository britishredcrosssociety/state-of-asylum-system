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
