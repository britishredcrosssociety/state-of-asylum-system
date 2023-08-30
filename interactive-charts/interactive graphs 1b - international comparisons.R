# Install {compositr}, if needed
# devtools::install_github("humaniverse/compositr")

library(tidyverse)
library(compositr)
library(asylum)

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

# ---- European countries with the highest numbers of people applying for asylum ----
# Download "Asylum and first time asylum applicants - annual aggregated data (tps00191)"
# Source: https://ec.europa.eu/eurostat/web/migration-asylum/asylum/database
tf <- download_file("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/tps00191.tsv.gz", file_extension = ".tsv.gz")

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

# Turn each nation's geo code into one of Flourish's URLs for flags
eu_asylum <- 
  eu_asylum |> 
  # Manually tweak country codes to match Flourish's .svg filenames
  mutate(geo = case_when(
    geo == "UK" ~ "GB",  # United Kingdom
    geo == "EL" ~ "GR",  # Greece
    .default = geo
  )) |> 
  mutate(geo = str_glue("https://public.flourish.studio/country-flags/svg/{tolower(geo)}.svg")) |> 
  rename(`Flag URL` = geo)

# Save
eu_asylum |> 
  write_csv("data-raw/flourish/1b - International comparisons/Europe comparison - applications.csv")

# ---- International displacement ----
# Source: https://www.unhcr.org/refugee-statistics/download/?url=705UgZ
tf <- compositr::download_file("https://api.unhcr.org/population/v1/population/?limit=20&dataset=population&displayType=totals&columns%5B%5D=refugees&columns%5B%5D=asylum_seekers&columns%5B%5D=idps&columns%5B%5D=oip&columns%5B%5D=stateless&columns%5B%5D=hst&columns%5B%5D=ooc&yearFrom=2001&yearTo=2022&coo_all=true&coa_all=true&download=true&_gl=1*yos0bt*_rup_ga*MjAwNzY4ODg5OS4xNjg3ODY0NjI1*_rup_ga_EVDQTJ4LMY*MTY4ODcyNjEyMi40LjEuMTY4ODcyODUzMy4wLjAuMA..*_ga*MjAwNzY4ODg5OS4xNjg3ODY0NjI1*_ga_X2YZPJ1XWR*MTY4ODcyNjEyMi40LjEuMTY4ODcyODUzMy4wLjAuMA..", ".zip")

unzip(tf, exdir = tempdir())

flows <- read_csv(file.path(tempdir(), "population.csv"), skip = 14)

flows <- 
  flows |> 
  mutate(across(`Refugees under UNHCR's mandate`:`Others of concern`, as.integer))

flows <- 
  flows |>
  mutate(across(where(is.integer), ~replace_na(.x, 0))) |> 
  mutate(`People displaced internationally` = `Refugees under UNHCR's mandate` + `Asylum-seekers` + `Other people in need of international protection`)

# Internally displaced persons
# flows_idp <- 
#   flows |> 
#   filter(`Country of origin (ISO)` == `Country of asylum (ISO)`)

flows_international <- 
  flows |> 
  filter(`Country of origin (ISO)` != `Country of asylum (ISO)`)

flows_international |> 
  write_csv("data-raw/flourish/1b - International comparisons/international displacement.csv")
