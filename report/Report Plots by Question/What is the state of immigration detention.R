----#QUESTION: WHAT IS THE STATE OF DETENTION?
  ----#Detention Overall Analyses#----

#People Entering Detention#
OverallEnteringDetention <- people_entering_detention %>%
  select(Year, Nationality, Sex, Age, `First place of detention`, Entering) %>%
  group_by(Year) %>%
  summarise(TotalinDetention = sum(Entering))

OEDG <- OverallEnteringDetention |>
  ggplot(aes(Year, TotalinDetention)) +
  geom_line(aes(colour = "red"), show.legend = FALSE) +
  geom_point(aes(colour = "red", alpha = 0.5, size = TotalinDetention), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(TotalinDetention)), show.legend = FALSE, size = rel(2)) +
  theme_classic() +
  labs(title = "People Entering Detention (2010 - 2022)",
       x = "Year",
       y = "Number of People Entering")
OEDG + scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))

#People in Detention#
OverallinDetention <- people_in_detention %>%
  select(Year, Quarter, Age, Sex, `Length of detention`, People) %>%
  group_by(Year, Quarter) %>%
  summarise(TotalinDetention = sum(People))

OverallinDetention |>
  ggplot(aes(Year, TotalinDetention)) +
  geom_line(aes(colour = "red"), show.legend = FALSE) +
  geom_point(aes(colour = "red", alpha = 0.5, size = TotalinDetention), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(TotalinDetention)), show.legend = FALSE, size = rel(2)) +
  theme_classic() +
  labs(title = "People in Detention (2010 - 2022)",
       x = "Year",
       y = "Number of People") +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))


#Detention by Quarter- Stacked bar chart#
OverallinDetention %>%
  #filter(Year > 2021) |>
  ggplot(aes(fill = Quarter, y = TotalinDetention, x = Year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  #scale_x_continuous(breaks = c(2022, 2023)) +
  theme_classic() +
  labs(title = "People in Detention by Quarter", x = "Year", y = "Total Cases")

#Detention Last 12 months

Detention12M <- people_in_detention %>%
  filter(Year > 2021) %>%
  select(Year, Date, Quarter, People) %>%
  group_by(Date) %>%
  summarise(Total12M = sum(People))

Detention12M |>
  ggplot(aes(Date, Total12M)) +
  geom_line() +
  geom_point() +
  theme_classic() 




#Length of Detention#
LengthinDetention <- people_in_detention %>%
  select(Year, `Length of detention`, People) %>%
  group_by(Year, `Length of detention`) %>%
  summarise(LengthTotal = sum(People))

LengthinDetention |>
  ggplot(aes(Year, LengthTotal)) +
  geom_line(aes(colour = `Length of detention`)) +
  theme_classic() +
  labs(title = "Length in Detention",
       x = "Year",
       y = "Number of People") +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))
#To revise length and colours for graph as too messy- discuss with team on what time periods to include#

#Detention and Age# 

AgeDetention <- people_in_detention %>%
  select(Year, Age, People) %>%
  group_by(Year, Age) %>%
  summarise(AgeTotal = sum(People))

AgeDetention |>
  ggplot(aes(Year, AgeTotal)) +
  geom_line(aes(colour = Age)) +
  theme_classic() +
  labs(title = "Age of those in Detention",
       x = "Year", 
       y = "Number of People", 
       caption = "BRC Mock Analysis")

#Detention by Sex#
DetentionbySex <- people_in_detention %>%
  select(Year, Sex, People) %>%
  group_by(Year, Sex) %>%
  summarise(Total = sum(People))

DetentionbySex |>
  ggplot(aes(Year, Total)) +
  geom_line(aes(colour = Sex)) +
  theme_classic() +
  labs(title = "Sex of those in Detention", 
       x = "Year", 
       y = "Number of People", 
       caption = "BRC Mock Analysis") +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023))

#Detention Pregnant Women#

detention_pregnant_women %>%
  select(Year, `Number of pregnant women detained in the immigration detention estate`) %>%
  group_by(Year) %>%
  summarise(Total = sum(`Number of pregnant women detained in the immigration detention estate`)) |>
  ggplot(aes(Year, Total)) +
  geom_line(aes(colour = "red")) +
  geom_point(aes(colour = "red")) +
  theme_classic() +
  labs(title = "Number of Pregnant Women in Detention",
       xlab = "Year", 
       ylab = "Number")



