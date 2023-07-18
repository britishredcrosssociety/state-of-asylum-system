----#QUESTION: HOW LONG IS IT TAKING FOR DECISIONS TO BE MADE?#---- 
----#Backlog Analyses#----

#Note from their own data release, it looks like they only include age groups and gender. No nationality. Does not break down each reason by age or gender though.#
#Backlog analyses only released during Q2!!!!!#

Backlog <- asylum_work_in_progress %>%
  select( Date , `Total Work In Progress`) 

#Total work in progress is defined in the dataset as the number of cases that are not concluded at the given point in time.
#That means that in 2022, there were 166000 cases that had not concluded at Q2 in 2022. 
----##Graph: Backlog Overall##---- 

BacklogGraph <- (ggplot(Backlog) +
                   geom_col(aes(x = Date, y = `Total Work In Progress`), colour = brc_colours$red_dunant, fill = brc_colours$red_dunant)  +
                   theme_classic() +
                   scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
                   #scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
                   labs(title = "Backlog of People Waiting for a Decision", 
                        x = NULL,
                        y = " Total Cases", 
                        caption = "British Red Cross Analyses of Home Office Data, 2023")) 

BacklogGraph + geom_text(aes(x = Date, y = `Total Work In Progress`, label = scales::comma(`Total Work In Progress`)), vjust = -0.5, show.legend = FALSE, size = rel(4)) 

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
DecisionTimeG <- (DecisionTime %>%
ggplot(aes(x = Date, y = Total, fill = Duration)) +
geom_area() +
theme_classic() +
  labs(title = "Length of Time for an Initial Decision on Asylum Applications", 
       x = NULL, 
       y = "Applications", 
       caption = "British Red Cross Analyses of Home Office Data, March 2023") +
scale_y_continuous(labels = scales::comma, limits = c(0, NA)))

DecisionTimeG + scale_fill_manual(values = c(brc_colours$red_mercer,
                                             brc_colours$red_deep)) 
+
scale_x_continuous(breaks = c(2012, 2014, 2016, 2017,2018, 2019, 2020, 2021, 2022, 2023))


