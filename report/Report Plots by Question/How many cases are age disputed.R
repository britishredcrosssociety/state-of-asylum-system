----#QUESTION: HOW MANY PEOPLE HAVE AGE DISPUTES? 
  
----#Age Dispute Raised or Resolved#----
AgeDispute <- age_disputes %>%
  group_by(Year, `Raised or resolved`) %>%
  summarise(Total = sum(`Age disputes`)) 

AgeDispute %>%
  ggplot(aes(fill = `Raised or resolved`, y = Total, x = Year)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = Total),
            position = position_stack(vjust = .5), size = 2) +
  theme_classic() +
  labs(title = "Number of age dispute cases raised and resolved from March 2010 to March 2023",
       x = 'Year',
       y = "Number of cases",
       caption = "British Red Cross analysis of Home Office data, March 2010 to March 2023") +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_fill_manual(values = c(brc_colours$red_light,
                               brc_colours$red_dunant))
  
----#Age Dispute by Resolved Reason#---- 

AgeDispute <- age_disputes %>%
  group_by(Year, Nationality, `Raised type / Resolved outcome`, `Raised or resolved`) %>%
  summarise(Total = sum(`Age disputes`))

ByOutcome <- AgeDispute %>%
  group_by(Year,`Raised type / Resolved outcome`) %>%
  filter(`Raised type / Resolved outcome` != 'Existing asylum application') %>%
  filter(`Raised type / Resolved outcome` != 'Asylum application raised in quarter') %>%
  summarise(TotalDispute = sum(Total))

ggplot(ByOutcome, aes(fill = `Raised type / Resolved outcome`, y = TotalDispute, x = Year)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = TotalDispute),
            position = position_stack(vjust = .5), size = 2) +
  theme_classic() +
  labs(title = "Outcome of age dispute cases from March 2010 to March 2023", 
       x = "Year", 
       y = "Total Cases",
       fill = 'Outcome of Age Dispute',
       caption = "British Red Cross analysis of Home Office data, March 2010 to March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 2500)) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_fill_manual(values = c(brc_colours$red_light,
                               brc_colours$red_dunant))


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
