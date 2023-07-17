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
    plot.title.position = "plot") +
  labs(
    title = "Average Grant Rates an Initial Decisions",
    subtitle = "Average asylum grant rate is the proportion of initial decisions which resulted in a grant of protection or other leave",
    x = "Year",
    y = "Grant Rate (%)",
    caption = "British Red Cross Analyses until March 2023")


#Grant Rate Top Nationalities#
GrantRateNationality <- grant_rates_initial_annual %>%
  filter(Year == 2022, Grant > 0, Refused > 0) %>%
  select(Year, Nationality, Grant, Refused, `Initial grant rate`) 

GrantRateNationality <- GrantRateNationality %>%
  mutate(`Initial grant rate` * 100) %>%

TopGranted <- (GrantRateNationality %>%
  filter(`Initial grant rate` * 100 > 80))

TopGranted <- arrange(TopGranted, desc(`Initial grant rate` * 100))

GrantRate <- (ggplot() + 
  geom_col(TopGranted, mapping = aes(x = Nationality, y =`Initial grant rate` * 100)) +
  theme_classic() +
  labs(title = "Nationalities with the Highest Grant Rates at Initial Decision", 
       x = "Nationality",
       y = "Grant Rate (%)") +
    scale_fill_continuous(name = "Grant Rate"))

GrantRate + theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=1))


----#Appeals#----
AppealsLodgedTotal <- appeals_lodged %>%
  select(Date, Nationality, Year,`Appeals lodged`) %>%
  group_by(Date, Year, Nationality) %>%
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
    plot.title.position = "plot") +
  labs(
    title = "Asylum Appeals Lodged",
    subtitle = "Appeals raised at the First-Tier Tribunal",
    x = NULL,
    y = "Total Appeals Lodged",
    caption = "British Red Cross Analyses 2023, Q1") +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023))

----#Appeals Lodged by Nationality#----
Appeals2022 <- AppealsLodgedTotal %>%
  filter(Date > "2022-01-01") %>%
  group_by(Nationality) %>%
  summarise(TotalN = sum(TotalLodged))
  
Appeals2022 %>%
  filter(TotalN > 111) %>%
  ggplot(aes(x = Nationality, y = TotalN)) +
  geom_col(fill = brc_colours$red_mercer, show.legend = NULL) +
  geom_text(aes(label = scales::comma(TotalN)), show.legend = FALSE, size = rel(3)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.5)) +
  labs(title = "Nationalities with Highest Appeals Lodged, 2023 Q1", 
       subtitle = "Appeals lodged at the First-Tier Tribunal",
       x = "Nationality", 
       y = "Appeals", 
       caption = "British Red Cross Analyses of Home Office Data, April 2022 - April 2023")


----#Appeals Determined#----
ADetermined <- appeals_determined %>%
  select(Year, Outcome, `Appeals determined`) %>%
  group_by(Year, Outcome) %>%
  summarise(TDetermined = sum(`Appeals determined`))

AppealOutcomes <- (ADetermined |>
  ggplot(aes(fill = Outcome, x = Year, y = TDetermined)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  theme_classic() +
  labs(title = "Outcome of Asylum Appeals",
       subtitle = "Outcome of asylum appeals raised at the First-Tier Tribunal, grouped by whether the appeal was allowed, dismissed or withdrawn",
       x = NULL, 
       y = "Appeals", 
       caption = "British Red Cross Analyses until 2023, Q1"))

AppealOutcomes + scale_fill_manual(values = c(brc_colours$red_light,
                                              brc_colours$red_mercer,
                                              brc_colours$red_deep))

                                    
  


