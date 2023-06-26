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
