#EU Data

library(readxl)
EU_stat_ <- read_excel("~/EU stat .xlsx")
View(EU_stat_)


#UKTotal 
AppsQ1T <- applications %>%
  group_by(Year) %>%
  filter(Year == 2022) %>%
  summarise(Total = sum(Applications))

#EU Bar Graph -2022 Total

EU_stat_$Country <- factor(EU_stat_$Country, levels = EU_stat_$Country[order(EU_stat_$`2022 Total`, decreasing = TRUE)])


Plot <- EU_stat_ %>%
  filter(Country != "EU Total 2022") %>%
 ggplot(aes(Country,`2022 Total`, colour = brc_colours$red_dunant, show.legend = FALSE)) +
  geom_bar(stat = "identity", fill = brc_colours$red_dunant, show.legend = FALSE) +
  geom_text(aes(label = scales::comma(`2022 Total`)), show.legend = FALSE, size = rel(2), colour = brc_colours$black_shadow) +
  theme_classic() +
  labs(title = "Asylum Applications for the European Union and United Kindom", 
       x = "Country", 
       y = "Asylum Applications", 
       caption = "British Red Cross Analyses of EuroStat, year ending 2022") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))

Plot + theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5))


#Top 5 Countries

EUTop5 <- (EU_stat_ %>%
  filter(`2022 Total` > 70000) %>%
  filter(Country != "EU Total 2022") %>%
  ggplot(aes(Country,`2022 Total`)) +
  geom_bar(stat = "identity", fill = brc_colours$red_mercer, show.legend = FALSE) +
  geom_text(aes(label = `2022 Total`), position=position_dodge(width=0.5), vjust=-0.25) +
  theme_classic() +
  labs(title = "Top 5 Countries in the European Union with People Applying for Asylum and the UK", 
       x = NULL, 
       y = "Asylum Applications", 
       caption = "British Red Cross Analyses of EuroStat, year ending 2022") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)))

EUTop5 

#EU Grant Rate & Top 5

EUGrant <- (EU_Grant_Rates %>%
              ggplot(aes(Country, Grant), colour = brc_colours$red_dunant) +
              geom_bar(stat = "identity") +
              geom_text(aes(label = Grant), position = position_dodge(width=1), vjust=-0.25, size = 2.5) +
              theme_classic() +
              labs(title = "Asylum Grants in European Union and the United Kingdom", 
                   x = NULL, 
                   y = "Asylum Grants", 
                   caption = "British Red Cross Analyses of EuroStat, year ending 2022") +
              scale_y_continuous(labels = scales::comma, limits = c(0, NA)))
  

EUGrant + theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5)) 



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

----#EU Across 10 Years#----

EU10Years <- (EU_Stats_10_Years %>%
  ggplot(aes(fill = Country, x = Year, y = Applications)) +
  geom_bar(position = "stack", stat = "identity") + 
  theme_classic() +
  labs(title = "Asylum Applications to the European Union over Last 10 Years", 
       x = NULL, 
       y = "Asylum Applications", 
       caption = "British Red Cross Analysis of EuroStat Data, year ending 2022") +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  scale_x_continuous(breaks = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)))
  
#To Discuss with Matt about the colours- too many countries for palettes to work?? including BRC.  
  
