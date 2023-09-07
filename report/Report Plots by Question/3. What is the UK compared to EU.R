library(tidyverse)
library(asylum)
library(compositr)
source("report/brc_colours.R")
source("report/theme_brc.R")

# Source of data will update at the end of 2023? Then can update. #

# EU country lookup
case_country_lookup <- function(country_code) {
  case_when(
    country_code == "AT" ~ "Austria",
    country_code == "BE" ~ "Belgium",
    country_code == "BG" ~ "Bulgaria",
    country_code == "CH" ~ "Switzerland",
    country_code == "CY" ~ "Cyprus",
    country_code == "CZ" ~ "Czechia",
    country_code == "DE" ~ "Germany",
    country_code == "DK" ~ "Denmark",
    country_code == "EE" ~ "Estonia",
    country_code == "EL" ~ "Greece",
    country_code == "ES" ~ "Spain",
    country_code == "FI" ~ "Finland",
    country_code == "FR" ~ "France",
    country_code == "HR" ~ "Croatia",
    country_code == "HU" ~ "Hungary",
    country_code == "IE" ~ "Ireland",
    country_code == "IS" ~ "Iceland",
    country_code == "IT" ~ "Italy",
    country_code == "LI" ~ "Liechtenstein",
    country_code == "LT" ~ "Lithuania",
    country_code == "LU" ~ "Luxembourg",
    country_code == "LV" ~ "Latvia",
    country_code == "ME" ~ "Montenegro",
    country_code == "MT" ~ "Malta",
    country_code == "NL" ~ "Netherlands",
    country_code == "NO" ~ "Norway",
    country_code == "PL" ~ "Poland",
    country_code == "PT" ~ "Portugal",
    country_code == "RO" ~ "Romania",
    country_code == "SE" ~ "Sweden",
    country_code == "SI" ~ "Slovenia",
    country_code == "SK" ~ "Slovakia",
    country_code == "UK" ~ "United Kingdom",
    .default = "NOT FOUND"
  )
}

tf <- download_file("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/tps00191.tsv.gz",".tsv.gz")

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

eu_asylum <- eu_asylum |>
 pivot_longer(cols = `2011`:`2022`, names_to = "Year", values_to = "Applications")

 ---- # 1. EU Across 10 Years ----
eu_asylum |>
  group_by(Year) |> 
  slice_max(Applications, n = 10) |> 
  ggplot(aes(fill = factor(Nation), x = Year, y = Applications)) +
  geom_bar(position = "stack", stat = "identity") + 
  theme_brc() +
  labs(title = "Number of asylum applications to European countries from 2011 to 2022",
       subtitle = "Top 10 countries with the highest asylum applications since 2011",
       x = "Year", 
       y = "Number of asylum applications", 
       fill = "Nationality",
       caption = "British Red Cross analysis of EuroStat Data, June 2022") +
  scale_y_continuous(labels = scales::unit_format(unit = "m", scale = 1e-6)) +
  scale_fill_manual(values = c(brc_colours$black_full,
                               brc_colours$green,
                               brc_colours$green_dark,
                               brc_colours$red_light,
                               brc_colours$red_earth,
                               brc_colours$red_mercer,
                               brc_colours$red_deep,
                               brc_colours$blue,
                               brc_colours$teal,
                               brc_colours$steel,
                               brc_colours$grey_fog,
                               brc_colours$grey,
                               brc_colours$red_dunant))

# Cannot do an "other" chart here because of the way that the data is coded. 


 ---- # 2. EU Bar Graph - 2022 Total ----

eu_asylum |>
  filter(Year == 2022) |>
  filter(Applications > 24000) |>
  ggplot(aes(x = reorder(Nation, desc(Applications)), y = Applications, colour = brc_colours$red_dunant, show.legend = FALSE)) +
  geom_bar(stat = "identity", fill = brc_colours$red_dunant, show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Applications)), show.legend = FALSE, size = rel(3), position = position_dodge(width=1), vjust=-0.25, colour = brc_colours$black_shadow) +
  theme_brc() +
  labs(title = "Number of asylum applications to European countries, 2022",
       subtitle =  "Top 10 countries with the highest asylum applications",
       x = "Country", 
       y = "Number of applications", 
       caption = "British Red Cross analysis of EuroStat data, June 2022") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 250000), expand = c(0,NA)) +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5))


 ---- # 3. EU Grant ----

# ---- Which countries in the EU have granted the most asylum claims? ----
# Final decisions on asylum applications - annual data (tps00193)
# Source: https://ec.europa.eu/eurostat/web/migration-asylum/asylum/database

tf <- download_file("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/tps00193.tsv.gz", file_extension = ".tsv.gz")

 eu_grants_raw <- read_tsv(tf)
 
 # Wrangling
 eu_grants <- 
   eu_grants_raw |> 
  
# Separate out the elements of the long single first column
   separate_wider_delim(`unit,citizen,sex,age,decision,geo\\time`, delim = ",", names = c("unit", "citizen", "sex", "age", "decision", "geo")) |> 
   
  filter(decision == "TOTAL_POS") |>  # Keep only positive decisions
   filter(geo != "EU27_2020") |>     # Don't need the EU27 total
   
  mutate(across(starts_with("20"), as.integer)) |> 
   
   mutate(Nation = case_country_lookup(geo)) |> 
   arrange(desc(`2022`)) |> 
   
   select(-c(unit:decision)) |> 
   relocate(Nation)
 
# - Add UK totals for 2020 to latest year -
#  What's the latest year in the EU dataset?

 first_year <- 
 str_extract(names(eu_grants), "20[0-9]+") |> 
 as.integer() |> 
 min(na.rm = TRUE)
 
 most_recent_year <- 
   str_extract(names(eu_grants), "20[0-9]+") |> 
   as.integer() |> 
   max(na.rm = TRUE)
 
 uk_grants <- 
   asylum::decisions_resettlement |> 
   filter(`Case type` == "Asylum Case" & str_detect(`Case outcome group`, "Grant")) |> 
  
   group_by(Year) |> 
   summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
   filter(Year >= first_year & Year <= most_recent_year) |> 
   
   mutate(Nation = "United Kingdom", geo = "UK") |> 
   pivot_wider(names_from = Year, values_from = Decisions)

 eu_grants <- 
   eu_grants |> 
   filter(Nation != "United Kingdom") |> 
   bind_rows(uk_grants) |> 
   arrange(desc(`2022`))

 
 eu_grants <- eu_grants |>
   pivot_longer(cols = `2011`:`2022`, names_to = "Year", values_to = "Applications")


eu_grants |>
  filter(Year == 2022) |>
  filter(Applications > 9000) |>
  ggplot(aes(Nation, Applications)) +
  geom_bar(stat = "identity", fill = brc_colours$red_dunant) +
  geom_text(aes(label = scales::comma(Applications)), show.legend = FALSE, size = rel(3), position = position_dodge(width=1), vjust=-0.25) +
  theme_classic() +
  labs(title = "Number of asylum grants across European countries in 2022",
        subtitle = "Top 10 countries with the highest number of asylum grants",
        x = "Country", 
        y = "Number of Asylum Grants", 
        caption = "British Red Cross analysis of EuroStat data, June 2022") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 150000)) +
  theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5))

eu_grants |>
  group_by(Year) |> 
  slice_max(Applications, n = 5) |> 
  ggplot(aes(fill = factor(Nation), x = Year, y = Applications)) +
  geom_bar(position = "stack", stat = "identity") + 
  theme_brc() +
  labs(title = "Number of asylum grants across European countries from 2011 to 2022",
       subtitle = "Top 5 countries with the highest number of asylum grants since 2011",
       x = "Year", 
       y = "Number of asylum grants", 
       fill = "Nationality",
       caption = "British Red Cross analysis of EuroStat Data, June 2022") +
  scale_y_continuous(labels = scales::comma, expand = c(0,NA), limits = c(0,125000)) +
  scale_fill_manual(values = c(brc_colours$black_shadow,
                               brc_colours$blue,
                               brc_colours$teal,
                               brc_colours$sky,
                               brc_colours$red_light,
                               brc_colours$red_deep,
                               brc_colours$red_dunant))

