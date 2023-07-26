#EU Data

library(readxl)
EU_stat_ <- read_excel("~/EU stat .xlsx")
View(EU_stat_)


#UKTotal 
AppsQ1T <- applications %>%
  group_by(Year) %>%
  filter(Year == 2022) %>%
  summarise(Total = sum(Applications))

----#EU Across 10 Years#----

EU_Stats_10_Years %>%
  filter(Applications > 40000) %>%
  ggplot(aes(fill = Country, x = Year, y = Applications)) +
  geom_bar(position = "stack", stat = "identity") + 
  theme_classic() +
  labs(title = "Number of asylum applications to European Countries and the United Kingdom from 2013 to 2022",
       subtitle = "Top 10 countries with the highest asylum applications over the last decade",
       x = "Year", 
       y = "Number of Asylum Applications", 
       caption = "British Red Cross analysis of EuroStat Data, year ending 2022") +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  scale_x_continuous(breaks = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) +
  scale_fill_manual(values = c(brc_colours$blue,
                               brc_colours$teal,
                               brc_colours$steel,
                               brc_colours$grey,
                               brc_colours$claret,
                               brc_colours$red_deep,
                               brc_colours$red_earth,
                               brc_colours$red_mercer,
                               brc_colours$red_light,
                               brc_colours$red_dunant))

#EU Bar Graph -2022 Total

EU_Stat$Countries <- factor(EU_Stat$Countries, levels = EU_Stat$Countries[order(EU_Stat$Applications, decreasing = TRUE)])


EU_Stat %>%
  filter(Applications > 24000) %>%
  ggplot(aes(Countries, Applications, colour = brc_colours$red_dunant, show.legend = FALSE)) +
  geom_bar(stat = "identity", fill = brc_colours$red_dunant, show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Applications)), show.legend = FALSE, size = rel(3), position = position_dodge(width=1), vjust=-0.25, colour = brc_colours$black_shadow) +
  theme_classic() +
  labs(title = "Number of asylum applications to European countries and the United Kindom in 2022",
       subtitle =  "10 countries with the highest applications",
       x = "Country", 
       y = "Number of People", 
       caption = "British Red Cross analysis of EuroStat data, year ending 2022") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 250000)) +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5))


#EU Grant Rate & Top 5

EU_Grant_Rates %>%
  filter(Grant > 9000) %>%
  ggplot(aes(Country, Grant)) +
  geom_bar(stat = "identity", fill = brc_colours$red_dunant) +
  geom_text(aes(label = scales::comma(Grant)), show.legend = FALSE, size = rel(3), position = position_dodge(width=1), vjust=-0.25) +
  theme_classic() +
  labs(title = "Number of asylum grants across European countries and the United Kingdom in 2022",
        subtitle = "10 countries with the highest number of asylum grants",
        x = "Country", 
        y = "Number of Asylum Grants", 
        caption = "British Red Cross analysis of EuroStat data, year ending 2022") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 150000)) +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5))


##EU Grant Rate as % needs to be revised and check with Matt.##
EU_Grant_Rates$Country <- factor(EU_Grant_Rates$Country, levels = EU_Grant_Rates$Country[order(EU_Grant_Rates$Grant, decreasing = TRUE)])

EU_Grant_Rates <- EU_Grant_Rates %>%
  mutate(GrantRate1 = (Grant/Total)*100) %>%
 
  view(EU_Grant_Rates)

round(data = EU_Grant_Rates::GrantRate1, digits = 0)

ceiling(EU_Grant_Rates::GrantRate1)

PerEUGrant <- (EU_Grant_Rates %>%
  ggplot(aes(Country, GrantRate)) +
  geom_bar(stat = "identity", fill = brc_colours$red_mercer) +
  #geom_text(aes(label = GrantRate), position = position_dodge(width=0.5), vjust=-0.25) +
  theme_classic() +
  labs(title = "EU Grant Rate and United Kingdom Grant Rate", 
                x = "Country", 
                y = "Grant Rate (%)") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)))

PerEUGrant + theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5))

  
