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

Plot <- EU_stat_ %>%
  filter(Country != "EU Total 2022") %>%
ggplot(aes(Country,`2022 Total`, colour = brc_colours$red_dunant, show.legend = FALSE)) +
  geom_bar(stat = "identity", fill = brc_colours$red_dunant, show.legend = FALSE) +
  theme_classic() +
  labs(title = "Asylum Applications for the European Union and United Kindom", 
       x = "Country", 
       y = "Asylum Applications", 
       caption = "British Red Cross Analyses of EuroStat, August 2022- May 2023") +
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
  labs(title = "Countries with the Most Asylum Applications and the UK", 
       x = "Country", 
       y = "Asylum Applications") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)))

EUTop5 

#EU Grant Rate & Top 5

EUGrant <- (EU_Grant_Rates %>%
              ggplot(aes(Country, Grant)) +
              geom_bar(stat = "identity") +
              geom_text(aes(label = Grant), position = position_dodge(width=0.5), vjust=-0.25) +
              theme_classic() +
              labs(title = "EU Initial Grants vs United Kingdom", 
                   x = "Country", 
                   y = "Asylum Grants") +
              scale_y_continuous(labels = scales::comma, limits = c(0, NA)))
  

EUGrant + theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5))

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
