library(tidyverse)
library(asylum)
library(readxl)
source("report/brc_colours.R")
source("report/theme_brc.R")



# Download "Asylum and first time asylum applicants - annual aggregated data (tps00191)"
# Source: https://ec.europa.eu/eurostat/web/migration-asylum/asylum/database
tf <- download.file("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/tps00191.tsv.gz", destfile = ".tsv.gz")

eu_asylum_raw <- read_tsv(tf)

# Wrangling
eu_asylum <- 
  eu_asylum_raw |> 
  
  # Separate out the elements of the long single first column
  separate_wider_delim(`citizen,sex,unit,age,asyl_app,geo\\time`, delim = ",", names = c("citizen", "sex", "unit", "age", "asyl_app", "geo")) |> 
  
  filter(asyl_app == "ASY_APP") |>  # Keep only asylum applications (not breakdowns by first time or subsequent)
  filter(geo != "EU27_2020") |>     # Don't need the EU27 total
  
  mutate(across(starts_with("20"), as.integer)) |> 
  
  mutate(Nation = case_country_lookup(geo)) |> 
  arrange(desc(`2022`)) |> 
  
  select(-c(citizen:asyl_app)) |> 
  relocate(Nation)

# - Add UK totals for 2020 to latest year -
# What's the latest year in the EU dataset?
first_year <- 
  str_extract(names(eu_asylum), "20[0-9]+") |> 
  as.integer() |> 
  min(na.rm = TRUE)

most_recent_year <- 
  str_extract(names(eu_asylum), "20[0-9]+") |> 
  as.integer() |> 
  max(na.rm = TRUE)

uk_asylum <- 
  asylum::applications |> 
  group_by(Year) |> 
  summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
  filter(Year >= first_year & Year <= most_recent_year) |> 
  
  mutate(Nation = "United Kingdom", geo = "UK") |> 
  pivot_wider(names_from = Year, values_from = Applications)

eu_asylum <- 
  eu_asylum |> 
  filter(Nation != "United Kingdom") |> 
  bind_rows(uk_asylum) |> 
  arrange(desc(`2022`))




------


# EU Data
EU_stat_ <- read_excel("~/EU stat .xlsx")
View(EU_stat_)

# UKTotal 
AppsQ1T <- applications %>%
  group_by(Year) %>%
  filter(Year == 2022) %>%
  summarise(Total = sum(Applications))

# ---- EU Across 10 Years ----
EU_Stats_10_Years %>%
  group_by(Year) |> 
  slice_max(Applications, n = 10) |> 
  ggplot(aes(fill = Country, x = Year, y = Applications)) +
  geom_bar(position = "stack", stat = "identity") + 
  theme_classic() +
  labs(title = "Number of asylum applications to European countries from 2013 to 2022",
       subtitle = "Top 10 countries with the highest asylum applications over the last decade",
       x = "Year", 
       y = "Number of Asylum Applications", 
       caption = "British Red Cross analysis of EuroStat Data, MONTH 2022") +
  scale_y_continuous(labels = scales::unit_format(unit = "m", scale = 1e-6)) +
  scale_x_continuous(breaks = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) +
  scale_fill_manual(values = c(brc_colours$black_full,
                               brc_colours$blue,
                               brc_colours$teal,
                               brc_colours$green,
                               brc_colours$green_dark,
                               brc_colours$steel,
                               brc_colours$grey,
                               brc_colours$grey_fog,
                               brc_colours$duck,
                               brc_colours$sand,
                               brc_colours$claret,
                               brc_colours$red_deep,
                               brc_colours$red_earth,
                               brc_colours$red_mercer,
                               brc_colours$red_light,
                               brc_colours$red_dunant))

# Can't do "Other" for the Across 10 Year plots because of the way that the data is coded.

# ---- EU Bar Graph - 2022 Total ----
EU_Stat$Countries <- factor(EU_Stat$Countries, levels = EU_Stat$Countries[order(EU_Stat$Applications, decreasing = TRUE)])

EU_Stat %>%
  filter(Applications > 24000) %>%
  ggplot(aes(Countries, Applications, colour = brc_colours$red_dunant, show.legend = FALSE)) +
  geom_bar(stat = "identity", fill = brc_colours$red_dunant, show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Applications)), show.legend = FALSE, size = rel(3), position = position_dodge(width=1), vjust=-0.25, colour = brc_colours$black_shadow) +
  theme_classic() +
  labs(title = "Number of asylum applications to European countries, 2022",
       subtitle =  "Top 10 countries with the highest applications",
       x = "Country", 
       y = "Number of People", 
       caption = "British Red Cross analysis of EuroStat data, MONTH 2022") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 250000)) +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5))

EU_Stat |> 
  ggplot(aes(Countries, Applications, colour = brc_colours$red_dunant, show.legend = FALSE), group_by(Countries)) +
  geom_bar(stat = "identity", fill = brc_colours$red_dunant, show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Applications)), show.legend = FALSE, size = rel(3), position = position_dodge(width=1), vjust=-0.25, colour = brc_colours$black_shadow) +
  theme_classic() +
  labs(title = "Number of asylum applications to European countries, 2022",
       subtitle =  "Top 10 countries with the highest applications",
       x = "Country", 
       y = "Number of People", 
       caption = "British Red Cross analysis of EuroStat data, MONTH 2022") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 250000)) +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5))
  
# ---- Top 10 Grant Rates ----
EU_Grant_Rates %>%
  filter(Grant > 9000) %>%
  ggplot(aes(Country, Grant)) +
  geom_bar(stat = "identity", fill = brc_colours$red_dunant) +
  geom_text(aes(label = scales::comma(Grant)), show.legend = FALSE, size = rel(3), position = position_dodge(width=1), vjust=-0.25) +
  theme_classic() +
  labs(title = "Number of asylum grants across European countries in 2022",
        subtitle = "Top 10 countries with the highest number of asylum grants",
        x = "Country", 
        y = "Number of Asylum Grants", 
        caption = "British Red Cross analysis of EuroStat data, MONTH 2022") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 150000)) +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5))

# EU Grant Rate as % needs to be revised and check with Matt.
EU_Grant_Rates$Country <- factor(EU_Grant_Rates$Country, levels = EU_Grant_Rates$Country[order(EU_Grant_Rates$Grant, decreasing = TRUE)])

EU_Grant_Rates <- EU_Grant_Rates %>%
  mutate(GrantRate1 = (Grant/Total)*100) %>%
 
  view(EU_Grant_Rates)

round(data = EU_Grant_Rates::GrantRate1, digits = 0)

ceiling(EU_Grant_Rates::GrantRate1)

PerEUGrant <- (EU_Grant_Rates %>%
  ggplot(aes(Country, GrantRate)) +
  geom_bar(stat = "identity", fill = brc_colours$red_mercer) +
  # geom_text(aes(label = GrantRate), position = position_dodge(width=0.5), vjust=-0.25) +
  theme_classic() +
  labs(title = "EU Grant Rate and United Kingdom Grant Rate", 
                x = "Country", 
                y = "Grant Rate (%)") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)))

PerEUGrant + theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5))
