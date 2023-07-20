----#QUESTION: HOW MANY PEOPLE HAVE AGE DISPUTES? 
  
----#Age Dispute Raised or Resolved#----
AgeDispute <- age_disputes %>%
  group_by(Year, `Raised or resolved`) %>%
  summarise(Total = sum(`Age disputes`)) 

AgeDispute %>%
  ggplot(aes(fill = `Raised or resolved`, y = Total, x = Year)) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  labs(title = "Total number of age dispute cases raised and resolved",
       x = NULL,
       y = "Number of cases",
       caption = "British Red Cross Analyses of Home Office Data, January 2022 - January, 2023") +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_fill_manual(values = c(brc_colours$red_mercer,
                               brc_colours$red_deep))
  
  
  
  
  
  
#Age Dispute Raised#
AgeDispute12MonthRaised <- age_disputes %>%
  filter(Year > 2021) %>%
  filter(`Raised or resolved` == "Raised") %>%
  group_by(Year,`Raised type / Resolved outcome`) %>%
  summarise(Total = sum(`Age disputes`))

#Age Dispute by Resolved Reason# 

AgeDisputeResolved12Month <- age_disputes %>%
  filter(Year > 2021)
  group_by(Year, Quarter, Nationality, `Raised type / Resolved outcome`, `Raised or resolved`) %>%
  summarise(Total = sum(`Age disputes`))

ByDispute <- AgeDispute12Month %>%
  group_by(Year, Quarter, `Raised type / Resolved outcome`) %>%
  summarise(TotalDispute = sum(Total))

ggplot(ByDispute, aes(fill = `Raised type / Resolved outcome`, y = TotalDispute, x = Quarter)) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  labs(title = "Outcome of Age Dispute Cases by Quarter in last 12 Months", 
       x = "Quarter", 
       y = "Total Cases", 
       caption = "British Red Cross Analyses of Home Office Data, year ending January 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_fill_manual(values = c(brc_colours$steel,
                               brc_colours$teal,
                               brc_colours$red_mercer,
                               brc_colours$red_deep))


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
