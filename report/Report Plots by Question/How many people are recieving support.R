----#QUESTION: HOW MANY PEOPLE ARE IN RECIEPT OF ASYLYUM SUPPORT (S4, S98, S95)
  
----#Section 95 Support#----
----##Support Applications#----

Section95Apps <- support_applications %>%
  select(Year, Nationality, `Support type granted`, `Group type`, Applications) %>%
  group_by(Year, Nationality, `Support type granted`, `Group type`) %>%
  summarise(Total = sum(Applications))

TotalSection95Apps <- Section95Apps %>%
  group_by(Year, `Support type granted`,`Group type`) %>%
  summarise(Total95 = sum(Total))

#Applications per Support Type Granted# 

TotalSection95Apps |>
  ggplot(aes(fill = `Support type granted`, y = Total95, x = Year)) +
  geom_bar(position ="stack", stat="identity") +
  theme_classic() +
  labs(title = "Total Number of Applications for Section 95 by Support Type", 
       x = "Year", 
       y = "Number of Applications")
scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))


#Applications per Group Type x Support Type in 2022#
TotalSection95Apps %>%
  filter(Year == 2022) |>
  ggplot(aes(fill = `Support type granted`, y = Total95, x = `Group type`)) +
  geom_bar(position ="stack", stat="identity") +
  theme_classic() +
  labs(title = "Total Number of Applications for Section 95 by Group Type, 2022", 
       x = "Group Type", 
       y = "Number of Applications") +
  #scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))


#Section 95 by Nationality# 

Nationalities95 <- Section95Apps %>%
  group_by(Year, Nationality) %>%
  summarise(TotalNat = sum(Total)) 

#In 2020 and 2022, of all the Section95 applications made, Unknown was the highest group in Nationality. Does this even fit with the narrative? 

----##Section 95 Received##----
view(support_received)

Section95Recieved <- support_received %>%
  select(Date, `Support Type`, `Accommodation Type`, "UK Region" , People) 

Section95Recieved <- Section95Recieved %>%
  mutate(Year = lubridate::year(Date))

TotalSection95Recieved <- Section95Recieved %>%
  group_by(Year) %>%
  summarise(Total = sum(People))

TotalSection95Recieved |>
  ggplot(aes(Year, Total)) +
  geom_line(aes(colour = "red"), show.legend = FALSE) +
  geom_point(aes(colour = "red", size = Total, alpha = 0.5), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(2)) +
  theme_classic() +
  labs(title = "Total Number of Section 95 Support Recieved per Year", 
       x = "Year", 
       y = "Number of People") +
  scale_x_continuous(breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))

SupportType <- SupportType %>%
  mutate(Year = lubridate::year(Date))

SupportType <- Section95Recieved %>%
  group_by(Year,`Support Type`,`Accommodation Type`) %>%
  summarise(Total = sum(People))  

SupportType |>
  ggplot(aes(fill = `Support Type`, y = Total, x = Year)) +
  geom_bar(position ="stack", stat="identity") +
  theme_classic() +
  labs(title = "Support for Refugees by by Support Type", 
       x = "Year", 
       y = "Number of People") +
  scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))


#2022/23 Section and Accommodation Type 
SupportType %>%
  filter(Year == 2022) |>
  ggplot(aes(fill = `Accommodation Type`, y = Total, x = `Support Type`)) +
  geom_bar(position ="stack", stat="identity") +
  theme_classic() +
  labs(title = "Accomodation by Support Type, 2022", 
       x = "Accomodation Type", 
       y = "Number of People") +
  #scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  guides(color = guide_legend(override.aes = list(size = 0.5)))


----#Destitution#----

Destitution <- support_received %>%
  select(Date, `Support Type`, `Accommodation Type`, "UK Region" , People) 

Destitution <- Destitution %>%
  mutate(Year = lubridate::year(Date))

TotalDestitute <- Destitution %>%
  group_by(Year,`Support Type`) %>%
  summarise(Total = sum(People))

TotalDestitute |>
  ggplot(aes(x = Year,
             y = Total, 
             fill = `Support Type`)) +
  geom_area() +
  theme_classic() +
  labs(title = "Destitution by Support Type",
       x = "Year",
       y = "Number of People") +
  scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))