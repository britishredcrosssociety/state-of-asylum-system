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
