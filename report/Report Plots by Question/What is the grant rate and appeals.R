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
  select(Nationality, Year,`Appeals lodged`) %>%
  group_by(Year, Nationality) %>%
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
Appeals2023 <- AppealsLodgedTotal %>%
  filter(Year > 2022) %>%
  
Appeals2023 %>%
  filter(TotalLodged > 20) %>%
  ggplot(aes(x = Nationality, y = TotalLodged, fill = brc_colours$red_dunant), show.legend = NULL) +
  geom_col() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=1)) +
  labs(title = "Nationalities with Highest Appeals Lodged, 2023 Q1", 
       subtitle = "Appeals lodged at the First-Tier Tribunal",
       x = "Nationality", 
       y = "Appeals")



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
  labs(title = "Outcome of Asylum Appeals",
       subtitle = "Outcome of asylum appeals raised at the First-Tier Tribunal",
       x = NULL, 
       y = "Appeals", 
       caption = "British Red Cross Analyses 2023, Q1") +
  scale_fill_continuous(brc_colours$red_dunant, brc_colours$red_deep, brc_colours$red_light)

#Need to determine why the colour is not being applied- speak with Matt

