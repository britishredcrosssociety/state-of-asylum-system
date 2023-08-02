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
    title = "Average asylum grant rate at initial decision from 2001 to 2023 ",
    subtitle = "Proportion of initial decisions which resulted in a grant of protection or other leave",
    x = "Year",
    y = "Grant Rate (%)",
    caption = "British Red Cross analysis on Home Office data, January 2001 to January 2023")


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

AppealsLodgedTotal %>%
  group_by(Year) %>%
  summarise(Total = sum(TotalLodged)) %>%
  ggplot(aes(Year, Total)) +
  geom_line(aes(colour = brc_colours$red_dunant), show.legend = FALSE) +
  geom_point(aes(size = Total, alpha = 0.4, colour = brc_colours$red_dunant), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(4)) + 
  scale_y_continuous(labels = scales::comma, limits = c(0, 15000)) +
  theme_classic() +
  guides(fill=guide_legend(nrow=6,byrow=TRUE), color = guide_legend(nrow=6,byrow=TRUE))+
  theme(
    legend.position = "right",
    # legend.box = "vertical",
    # legend.margin = margin(),
    plot.title.position = "plot") +
  labs(
    title = "Number of asylum appeals from 2010 to 2023",  
    subtitle = "Number of appeals lodged with the first-tier tribunal where an asylum claim has been refused at initial decision",
    x = NULL,
    y = "Number of Appeals Lodged",
    caption = "British Red Cross analysis of Home Office data, January 2010 to January 2023") +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023))

----#Appeals Lodged by Nationality#----
Appeals2022 <- AppealsLodgedTotal %>%
  filter(Date > "2021-10-01") %>%
  group_by(Nationality) %>%
  summarise(TotalN = sum(TotalLodged))

Appeals2022$Nationality <- factor(Appeals2022$Nationality, levels = Appeals2022$Nationality[order(Appeals2022$TotalN, decreasing = TRUE)])
  
Appeals2022 %>%
  filter(TotalN > 111) %>%
  ggplot(aes(x = Nationality, y = TotalN)) +
  geom_col(fill = brc_colours$red_dunant, show.legend = NULL) +
  geom_text(aes(label = scales::comma(TotalN)), show.legend = FALSE, size = rel(3), position=position_dodge(width=0.5), vjust=-0.25) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.5)) +
  labs(title = "Number of appeals lodged by nationalities for year ending March 2023 ", 
       subtitle = "Appeals lodged at the First-Tier Tribunal",
       x = "Nationality", 
       y = "Number of Appeals", 
       caption = "British Red Cross analysis of Home Office Data, March 2022 to March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 800))


----##Outcomes of Initial Decision Bar Graph##----
ResettlmentTotal %>%
  filter(`Case type` != "Resettlment Case") %>%
  ggplot(aes(fill = `Case outcome group`, y = RTotal, x = Year)) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  labs(title = "Number of grants, refusals or withdrawls of asylum applications at initial decision from 2001 to 2023",
       subtitle = "Inital decision on asylum application grouped by case outcome",
       x = "Year", 
       y = "Number of Decisions", 
       caption = "British Red Cross analysis of Home Office data, until January, 2023") +
  scale_x_continuous(breaks = c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 150000)) + 
  scale_fill_manual(values = c(brc_colours$red_dunant,
                               brc_colours$red_light,
                               brc_colours$red_earth,
                               brc_colours$red_deep))