----#QUESTION: HOW MANY PEOPLE HAVE AGE DISPUTES? 
  
----#Age Dispute#----

AgeDispute12Month <- age_disputes %>%
  filter(Year > 2021)

#Age Dispute by Resolved Reason# 

AgeDispute12Month <- AgeDispute12Month %>%
  group_by(Year, Quarter, Nationality, `Raised type / Resolved outcome`) %>%
  summarise(Total = sum(`Age disputes`))

ByDispute <- AgeDispute12Month %>%
  group_by(Year, Quarter, `Raised type / Resolved outcome`) %>%
  summarise(TotalDispute = sum(Total))

ggplot(ByDispute, aes(fill = `Raised type / Resolved outcome`, y = TotalDispute, x = Quarter)) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  labs(title = "Age Dispute by Outcome", x = "Quarter of 2022", y = "Total Persons")


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
