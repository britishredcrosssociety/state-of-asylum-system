----#QUESTION: HOW LONG IS IT TAKING FOR DECISIONS TO BE MADE?#---- 
----#Backlog Analyses#----

#Note from their own data release, it looks like they only include age groups and gender. No nationality. Does not break down each reason by age or gender though.#
#Backlog analyses only released during Q2!!!!!#

Backlog <- asylum_work_in_progress %>%
  select(Year, `Total Work In Progress`) 

#Total work in progress is defined in the dataset as the number of cases that are not concluded at the given point in time.
#That means that in 2022, there were 166000 cases that had not concluded at Q2 in 2022. 
----##Graph: Backlog Overall##---- 

Backlog |>
  ggplot(aes(x = Year, y = `Total Work In Progress`)) +
  geom_col(aes(colour = brc_colours$red_dunant, fill = brc_colours$red_dunant), show.legend = FALSE) +
  geom_text(aes(x = Year, y = `Total Work In Progress`, label = scales::comma(`Total Work In Progress`)), vjust = -0.5, show.legend = FALSE, size = rel(3)) +
  labs(title = "Backlog of people waiting for a decision from 2011 to 2022", 
       x = "Year",
       y = "Number of People", 
       caption = "British Red Cross analysis of Home Office data, March 2011 to March 2022") + 
  theme_classic() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 200000)) +
  scale_x_continuous(breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) 
  

----##Graph of Overall Time for Initial Decisions##----
#Awaiting Decision is for Asylum Applications that are awaiting an initial decision or further review by nationality and applicant type#

DecisionTime <- awaiting_decision %>%
  select(Date, Nationality, Duration, `Application stage`, Applications) %>%
  group_by(Date, Duration, Nationality) %>%
  summarise(Total = sum(Applications))

view(DecisionTime)

DecisionTime <- DecisionTime %>%
  dplyr::mutate(year = lubridate::year(Date))

DecisionTime <- DecisionTime %>%
  group_by(Date, year, Duration) %>%
  summarise(Total = sum(Total))

DecisionTime <- DecisionTime %>%
  filter(Duration != 	"N/A - Further review") 

#Decision Plot 
DecisionTime %>%
ggplot(aes(x = Date, y = Total, fill = Duration)) +
geom_bar(position = "stack", stat = "identity") +
theme_classic() +
  labs(title = "Length of Time for an Initial Decision on Asylum Applications", 
       x = NULL, 
       y = "Applications", 
       caption = "British Red Cross Analyses of Home Office Data, March 2023") +
scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
scale_fill_manual(values = c(brc_colours$red_mercer,
                                             brc_colours$red_deep)) 
#scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))

#To discuss with Matt and team about how the data is spread and compare it to other work completed by other organizations.#

#Decision by Nationality in 2022# 

DecisionbyNat <- awaiting_decision %>%
  filter(Year == 2022) %>%
  group_by(Year, Nationality, Duration) %>%
  summarise(Total = sum(Applications))

DecisionbyNat$Nationality <- factor(DecisionbyNat$Nationality, levels = DecisionbyNat$Nationality[order(DecisionbyNat$Total, decreasing = TRUE)])
#Attempted to order from highest to lowest, but for some reason running into error- to revise. 

DecisionbyNat %>%
  filter(Duration == "More than 6 months") %>%
  filter(Total > 10000) %>%
  ggplot(aes(x = reorder(Nationality, desc(Total), sum), y = Total)) +
  geom_col(aes(colour = brc_colours$red_mercer, fill = brc_colours$red_mercer), show.legend = FALSE)  +
  geom_text(aes(x = Nationality, y = Total, label = scales::comma(Total)), vjust = -0.5, show.legend = FALSE, size = rel(4)) +
  theme_classic() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 50000)) +
  labs(title = "Nationalities with the largest number of people waiting over six months for an initial decision", 
       subtitle = "Top 10 nationalities",
        x = "Nationalities",
        y = "Applications", 
        caption = "British Red Cross analysis of Home Office data, March 2021 to March 2022")
