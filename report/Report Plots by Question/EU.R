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

Plot <- EU_stat_ |>
ggplot(aes(Country,`2022 Total`, colour = "red")) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(title = "Asylum Applications to EU and UK", 
       x = "Country", 
       y = "Asylum Applications") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))

Plot + theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5))


#Top 5 Countries

EUTop5 <- (EU_stat_ %>%
  filter(`2022 Total` > 70000) %>%
  ggplot(aes(Country,`2022 Total`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = `2022 Total`), position=position_dodge(width=0.5), vjust=-0.25) +
  theme_classic() +
  labs(title = "Countries with the Most Asylum Applications and the UK", 
       x = "Country", 
       y = "Asylum Applications") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)))

EUTop5 


