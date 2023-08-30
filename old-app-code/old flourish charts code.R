# ---- Immigration ----
# Total applications in 2022
# migration_asylum <- 
#   asylum::applications |> 
#   filter(Year == 2022) |> 
#   filter(`Applicant type` == "Main applicant") |> 
#   summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
#   pull(Applications)

# asylum::irregular_migration |> 
#   filter(Year == 2022 & str_detect(`Method of entry`, "boat")) |>   
#   summarise(Boats = sum(`Number of detections`, na.rm = TRUE)) |> 
#   pull(Boats)

# resettlement <- 
#   asylum::decisions_resettlement |> 
#   filter(Year == 2022 & str_detect(`Case type`, "Resettlement")) |> 
#   summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
#   pull(Decisions)

# These migration figures are taken from ONS
# Source: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/internationalmigration/bulletins/longterminternationalmigrationprovisional/yearendingdecember2022
# net_migration <- 606000 
# immigration_2022 <- 1163000
# ukraine <- 114000
# bno <- 52000  # British Nationals Overseas - BN(O)

# immigration <- 
#   tribble(
#   ~Category, ~`Sub-category`, ~`Migration type`, ~`Number of people`,
#   "Immigration", "Other migration", "Other migration", (immigration_2022 - ukraine - bno - migration_asylum - resettlement),
#   "Immigration", "Other migration", "Ukraine visas", ukraine,
#   "Immigration", "Other migration", "British Nationals Overseas", bno,
#   "Immigration", "Asylum claims", "Asylum claims (not via small boats)", (migration_asylum - migration_small_boats),
#   "Immigration", "Asylum claims", "Small boat arrivals claiming asylum", migration_small_boats,
#   "Immigration", "Other migration", "Resettlement and other safe routes", resettlement
# )
# 
# # Calculate proportions
# immigration <- 
#   immigration |> 
#   mutate(`Percentage of people immigrating` = `Number of people` / sum(`Number of people`)) |> 
#   mutate(`Percentage of people immigrating` = scales::percent(`Percentage of people immigrating`, accuracy = 0.1))


# Check historical numbers of applications for the same quarter as the most recently published stats
# current_quarter <- quarter(max(asylum::applications$Date))
# 
# asylum::applications |> 
#   filter(quarter(Date) == current_quarter) |> 
#   group_by(Date) |> 
#   summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
#   arrange(desc(Applications))

# Nationality
# applications_nationality <- 
#   asylum::applications |> 
#   filter(Date >= max(Date) - dmonths(11)) |> 
#   group_by(Nationality) |> 
#   summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
#   ungroup() |> 
#   mutate(Percent = scales::percent(Applications / sum(Applications))) |> 
#   arrange(desc(Applications))
# 
# # How many applications in total?
# sum(applications_nationality$Applications)
# 
# # Top five nationalities
# applications_nationality |> 
#   slice(1:5)
# 
# # What % of the total were from the top 5 nationalities
# applications_nationality |> 
#   slice(1:5) |> 
#   summarise(Total = sum(Applications)) |> 
#   pull(Total) / sum(applications_nationality$Applications)
# 
# 
# # Sex %
# applications_sex |> 
#   mutate(Percent = scales::percent(Sex / sum(Sex)))

# ---- Initial grant rates ----
# Top ten nations, by number of grants and grant rate in the most recent year
# top_ten_nations <- 
#   grant_rates_initial_annual |> 
#   filter(Year == max(Year)) |> 
#   arrange(desc(Grant), desc(`Initial grant rate`)) |> 
#   slice(1:10) |> 
#   pull(Nationality)
# 
# grant_rates_initial_annual |> 
#   filter(Year >= max(Year) - 1) |> 
#   filter(Nationality %in% top_ten_nations) |> 
#   select(Nationality, Year, `Initial grant rate`, `Number of grants` = Grant) |> 
#   arrange(desc(`Initial grant rate`)) |> 
#   write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/initial-grant-rates-annual-recent.csv")
# 
# grant_rates_initial_annual |> 
#   group_by(Year) |> 
#   summarise(
#     Grant = sum(Grant, na.rm = TRUE),
#     Refused = sum(Refused, na.rm = TRUE)
#   ) |> 
#   ungroup() |>
#   mutate(`Initial grant rate` = Grant / (Grant + Refused)) |> 
#   select(Year, `Initial grant rate`) |> 
#   write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/initial-grant-rates-annual-total.csv")
# 
# # Make a wider version of initial grant rates quarterly data for testing in a Flourish Studio chart
# grant_rates_initial_quarterly |> 
#   select(Date, Quarter, Nationality, `Initial grant rate`) |> 
#   pivot_wider(names_from = Nationality, values_from = `Initial grant rate`) |> 
#   
#   # Move the ten nations with the highest number of grants and highest grant rates to the left, so they get shown on the chart by default
#   relocate(Date, Quarter, any_of(top_ten_nations)) |> 
#   
#   # Remove columns that contain only NAs
#   select(where(~!all(is.na(.x)))) |> 
# 
#   write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/initial-grant-rates-quarterly-wide.csv")

# ---- Grant rates ----
# Calculate initial and final grant rates from Outcomes data
grant_rates_initial_final <- 
  asylum::outcomes |>
  drop_na() |> 
  select(`Year of application`, `Granted asylum`:Refused, `Allowed appeals`:`Dismissed appeals`, `Final outcome: Grants of asylum`:`Final outcomes: Refused asylum or HP or DL or other leave`) |> 
  group_by(`Year of application`) |> 
  summarise(across(everything(), sum)) |> 
  ungroup() |> 
  
  mutate(
    `Initial grant rate` = (`Granted asylum` + `Granted HP/DL` + `Other grants`) / (`Granted asylum` + `Granted HP/DL` + `Other grants` + Refused),
    # `Appeal grant rate` = `Allowed appeals` / (`Allowed appeals` + `Dismissed appeals`),
    `Final grant rate` = (`Final outcome: Grants of asylum` + `Final outcomes: Grants of HP/DL and other`) / (`Final outcome: Grants of asylum` + `Final outcomes: Grants of HP/DL and other` + `Final outcomes: Refused asylum or HP or DL or other leave`)
  )

grant_rates_initial_final |> 
  select(`Year of application`, contains("rate")) |> 
  write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/grant rates - initial and final.csv")

# - CAPTION -
# ...

# ---- Returns ----
# How many and who have been returned
# asylum::returns |> 
#   group_by(Year, `Return type group`) |> 
#   summarise(`Number of returns` = sum(`Number of returns`, na.rm = TRUE)) |> 
#   
#   pivot_wider(names_from = `Return type group`, values_from = `Number of returns`) |> 
#   
#   write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/returns - overall.csv")
# 
# asylum::returns |> 
#   group_by(Year, Age) |> 
#   summarise(`Number of returns` = sum(`Number of returns`, na.rm = TRUE)) |> 
#   filter(Age != "Unknown") |> 
#   pivot_wider(names_from = Age, values_from = `Number of returns`) |> 
#   write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/returns - by age.csv")
# 
# asylum::returns |> 
#   group_by(Year, Sex) |> 
#   summarise(`Number of returns` = sum(`Number of returns`, na.rm = TRUE)) |> 
#   filter(!str_detect(Sex, "Unknown")) |> 
#   pivot_wider(names_from = Sex, values_from = `Number of returns`) |> 
#   write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/returns - by sex.csv")

# Top five nations, by number of returns in the most recent year
# top_five_nations <- 
#   returns_by_destination |> 
#   filter(Year == max(Year)) |> 
#   group_by(Year, `Return destination`) |> 
#   summarise(`Number of returns` = sum(`Number of returns`, na.rm = TRUE)) |> 
#   arrange(desc(`Number of returns`)) |> 
#   slice(1:5) |> 
#   pull(`Return destination`)
# 
# returns_by_destination |> 
#   group_by(`Return destination`) |> 
#   summarise(`Number of returns` = sum(`Number of returns`, na.rm = TRUE)) |> 
#   ungroup() |> 
#   arrange(desc(`Number of returns`))
# 
# # Make a wider version of returns quarterly data
# asylum::returns_by_destination |> 
#   select(Date, `Return destination`, `Number of returns`) |> 
#   
#   group_by(Date, `Return destination`) |> 
#   summarise(`Number of returns` = sum(`Number of returns`, na.rm = TRUE)) |> 
#   
#   pivot_wider(names_from = `Return destination`, values_from = `Number of returns`) |> 
#   
#   # Move the ten nations with the highest number of grants and highest grant rates to the left, so they get shown on the chart by default
#   relocate(Date, any_of(top_five_nations)) |> 
#   
#   write_csv("data-raw/flourish/1 - Who is applying for asylum in the last 12 months/returns - by destination.csv")


# ---- Which countries in the EU have granted the most asylum claims? ----
# Final decisions on asylum applications - annual data (tps00193)
# Source: https://ec.europa.eu/eurostat/web/migration-asylum/asylum/database
# tf <- download_file("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/tps00193.tsv.gz", file_extension = ".tsv.gz")
# 
# eu_grants_raw <- read_tsv(tf)
# 
# # Wrangling
# eu_grants <- 
#   eu_grants_raw |> 
#   
#   # Separate out the elements of the long single first column
#   separate_wider_delim(`unit,citizen,sex,age,decision,geo\\time`, delim = ",", names = c("unit", "citizen", "sex", "age", "decision", "geo")) |> 
#   
#   filter(decision == "TOTAL_POS") |>  # Keep only positive decisions
#   filter(geo != "EU27_2020") |>     # Don't need the EU27 total
#   
#   mutate(across(starts_with("20"), as.integer)) |> 
#   
#   mutate(Nation = case_country_lookup(geo)) |> 
#   arrange(desc(`2022`)) |> 
#   
#   select(-c(unit:decision)) |> 
#   relocate(Nation)
# 
# # - Add UK totals for 2020 to latest year -
# # What's the latest year in the EU dataset?
# first_year <- 
#   str_extract(names(eu_grants), "20[0-9]+") |> 
#   as.integer() |> 
#   min(na.rm = TRUE)
# 
# most_recent_year <- 
#   str_extract(names(eu_grants), "20[0-9]+") |> 
#   as.integer() |> 
#   max(na.rm = TRUE)
# 
# uk_grants <- 
#   asylum::decisions_resettlement |> 
#   filter(`Case type` == "Asylum Case" & str_detect(`Case outcome group`, "Grant")) |> 
#   
#   group_by(Year) |> 
#   summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
#   filter(Year >= first_year & Year <= most_recent_year) |> 
#   
#   mutate(Nation = "United Kingdom", geo = "UK") |> 
#   pivot_wider(names_from = Year, values_from = Decisions)
# 
# eu_grants <- 
#   eu_grants |> 
#   filter(Nation != "United Kingdom") |> 
#   bind_rows(uk_grants) |> 
#   arrange(desc(`2022`))
# 
# # Turn each nation's geo code into one of Flourish's URLs for flags
# eu_grants <- 
#   eu_grants |> 
#   # Manually tweak country codes to match Flourish's .svg filenames
#   mutate(geo = case_when(
#     geo == "UK" ~ "GB",  # United Kingdom
#     geo == "EL" ~ "GR",  # Greece
#     .default = geo
#   )) |> 
#   mutate(geo = str_glue("https://public.flourish.studio/country-flags/svg/{tolower(geo)}.svg")) |> 
#   rename(`Flag URL` = geo)
# 
# # Save
# eu_grants |> 
#   write_csv("data-raw/flourish/1b - International comparisons/Europe comparison - grants.csv")

# ---- What are the top 5 countries globally that currently have the most asylum seekers? ----
# Source: https://popstats.unhcr.org/refugee-statistics/download/
# Direct link to data query: https://www.unhcr.org/refugee-statistics/download/?url=5VGvnQ
tf <- compositr::download_file("https://api.unhcr.org/population/v1/asylum-applications/?limit=20&dataset=asylum-applications&displayType=totals&yearFrom=2001&yearTo=2022&coa_all=true&columns%5B%5D=procedure_type&columns%5B%5D=app_type&columns%5B%5D=app_pc&columns%5B%5D=app_size&columns%5B%5D=dec_level&columns%5B%5D=applied&sort%5Byear%5D=desc&download=true&_gl=1*4410k5*_rup_ga*MjAwNzY4ODg5OS4xNjg3ODY0NjI1*_rup_ga_EVDQTJ4LMY*MTY4Nzg2NDYyNC4xLjAuMTY4Nzg2NDYyNC4wLjAuMA..*_ga*MjAwNzY4ODg5OS4xNjg3ODY0NjI1*_ga_X2YZPJ1XWR*MTY4Nzg2NDYyNC4xLjAuMTY4Nzg2NDYyNC4wLjAuMA..", file_extension = ".zip")

unzip(tf, exdir = tempdir())

global_asylum <- read_csv(file.path(tempdir(), "asylum-applications.csv"))

global_asylum |> 
  group_by(`Country of asylum`, Year) |> 
  summarise(applied = sum(applied, na.rm = TRUE)) |> 
  ungroup() |> 
  
  pivot_wider(names_from = Year, values_from = applied) |> 
  
  mutate(across(-`Country of asylum`, as.character)) |> 
  mutate(across(-`Country of asylum`, ~ replace_na(.x, ""))) |> 
  
  write_csv("data-raw/flourish/1b - International comparisons/Global comparison.csv")

# - Caption -
flows_international |> 
  filter(Year == max(Year)) |> 
  group_by(`Country of asylum`) |> 
  summarise(`People displaced internationally` = sum(`People displaced internationally`, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(Proportion = `People displaced internationally` / sum(`People displaced internationally`)) |> 
  arrange(desc(`People displaced internationally`)) |> 
  mutate(Cumulative = cumsum(Proportion))

# flows_idp_total <- 
#   flows_idp |> 
#   group_by(Year) |> 
#   summarise(`People displaced internally` = sum(`IDPs of concern to UNHCR`, na.rm = TRUE))
# 
# flows_international_total <- 
#   flows_international |> 
#   group_by(Year) |> 
#   summarise(`People displaced internationally` = sum(`People displaced internationally`))
# 
# left_join(flows_idp_total, flows_international_total) |> 
#   mutate(`IDP %` = `People displaced internally` / (`People displaced internally` + `People displaced internationally`)) |> 
#   ggplot(aes(x = Year, y = `IDP %`)) +
#   geom_line()


# ---- Family reunion by age and sex ----
# Age/sex pyramid
family_reunion_age_sex <- 
  asylum::family_reunion |> 
  # Filter applications within the last 12 months
  filter(Date >= max(Date) - dmonths(11)) |> 
  group_by(Age, Sex) |> 
  summarise(`Visas granted` = sum(`Visas granted`, na.rm = TRUE)) |> 
  
  filter(Age != "Unknown") |> 
  filter(Sex != "Unknown Sex") |> 
  
  pivot_wider(names_from = Sex, values_from = `Visas granted`) |> 
  mutate(Female = Female * -1) |> 
  
  arrange(match(Age, c("Under 18", "18-29", "30-49", "50-69", "70+"))) 

family_reunion_age_sex |> 
  write_csv("data-raw/flourish/2a - Safe routes/2a - Family reunion - by age and sex.csv")

# - Caption -
# % within age groups
family_reunion_age_sex |> 
  mutate(Female = Female * -1) |> 
  mutate(Proportion_female = Female / (Female + Male))

# % overall
family_reunion_age_sex |> 
  mutate(Female = Female * -1) |> 
  ungroup() |> 
  summarise(
    Female = sum(Female),
    Male = sum(Male)
  ) |> 
  mutate(Proportion_female = Female / (Female + Male))


# ---- How many people have been and are in detention in the last 12 months? ----
# leaving_detention <- 
#   asylum::people_leaving_detention |> 
#   # Filter visas within the last 12 months
#   filter(Date >= max(Date) - dmonths(11)) |> 
#   group_by(`Reason for leaving detention`) |> 
#   summarise(`Number of people` = sum(Leaving, na.rm = TRUE)) |> 
#   
#   mutate(`Reason for leaving detention` = case_when(
#     `Reason for leaving detention` == "Bailed (IJ)" ~ "Bailed (First-tier Tribunal)",
#     `Reason for leaving detention` == "Bailed (SoS)" ~ "Bailed (Secretary of State)",
#     `Reason for leaving detention` == "Granted LTE/LTR" ~ "Granted leave to enter/remain",
#     `Reason for leaving detention` == "Other" ~ "Other",
#     `Reason for leaving detention` == "Returned" ~ "Removed"
#   )) |> 
#   
#   mutate(`Detention status` = "Left detention in last 12 months") |> 
#   relocate(`Detention status`)
# 
# in_detention <- 
#   asylum::people_in_detention |> 
#   filter(Date == max(Date)) |> 
#   summarise(`Number of people` = sum(People, na.rm = TRUE)) |> 
#   
#   mutate(
#     `Detention status` = "In detention, as of March 2023",
#     `Reason for leaving detention` = "In detention",
#   ) |> 
#   relocate(`Detention status`, `Reason for leaving detention`)
# 
# bind_rows(leaving_detention, in_detention) |> 
#   # mutate(`Reason for leaving detention` = replace_na(`Reason for leaving detention`, "")) |> 
#   arrange(desc(`Number of people`)) |> 
#   write_csv("data-raw/flourish/2b - Detention/2b - detention - totals.csv")
# 
# # - Caption -
# leaving_detention |> 
#   mutate(Proportion = `Number of people` / sum(`Number of people`)) |> 
#   mutate(Proportion_cumulative = cumsum(Proportion))

# sum(leaving_detention$`People leaving detention`)
# sum(in_detention$`People in detention`)

# ---- Who is in immigration detention (nationality, age, gender w/ focus on number of women in detention- and pregnant women) ----
# - Nationality -
people_in_detention_by_nationality <- 
  asylum::people_in_detention |> 
  filter(Date == max(Date)) |> 
  group_by(Nationality) |> 
  summarise(`People in detention` = sum(People)) |> 
  ungroup() |> 
  arrange(desc(`People in detention`))

people_in_detention_by_nationality |>
  slice(1:20) |> 
  write_csv("data-raw/flourish/2b - Detention/2b - detention - by nationality.csv")

# - Caption -
people_in_detention_by_nationality |> 
  mutate(Proportion = `People in detention` / sum(`People in detention`)) |> 
  mutate(Proportion_cumulative = cumsum(Proportion))
round(438/1591 * 100, 0)

# - Old grant rate code -
# grant_rates_at_appeal <- 
#   asylum::appeals_determined |> 
#   group_by(Year, Nationality, Region, Outcome) |> 
#   summarise(`Appeals determined` = sum(`Appeals determined`, na.rm = TRUE)) |> 
#   ungroup() |> 
#   
#   pivot_wider(names_from = Outcome, values_from = `Appeals determined`) |> 
#   
#   mutate(
#     Allowed = replace_na(Allowed, 0),
#     Dismissed = replace_na(Dismissed, 0),
#     Withdrawn = replace_na(Withdrawn, 0)
#   ) |> 
#   
#   # Calculate grant rate - don't include withdrawn cases, since they're not
#   # included in calculations of initial grant rates
#   mutate(`Grant rate at appeal` = Allowed / (Allowed + Dismissed))
# 
# # Overall grant rates at appeal
# grant_rates_at_appeal_overall <- 
#   grant_rates_at_appeal |> 
#   group_by(Year) |> 
#   summarise(
#     Allowed = sum(Allowed, na.rm = TRUE),
#     Dismissed = sum(Dismissed, na.rm = TRUE)
#   ) |> 
#   mutate(`Grant rate at appeal` = Allowed / (Allowed + Dismissed)) |> 
#   select(Year, `Grant rate at appeal`)
# 
# grant_rates_at_appeal_overall |> 
#   write_csv("data-raw/flourish/3b - Decision-making/grant rates at appeal.csv")
# 
# # - Combine initial and appeal grant rates into a single dataframe/file
# # Initial grant rates
# grant_rates_initial_annual |>
#   group_by(Year) |>
#   summarise(
#     Grant = sum(Grant, na.rm = TRUE),
#     Refused = sum(Refused, na.rm = TRUE)
#   ) |>
#   ungroup() |>
#   mutate(`Initial grant rate` = Grant / (Grant + Refused)) |>
#   select(Year, `Initial grant rate`) |> 
#   
#   # Merge grant rates at appeal
#   left_join(grant_rates_at_appeal_overall) |> 
#   drop_na() |> 
#   write_csv("data-raw/flourish/3b - Decision-making/initial and appeal grant rates.csv")
# 
# 
# grant_rates_at_appeal_year <- 
#   grant_rates_at_appeal |> 
#     group_by(Year) |> 
#     summarise(
#       Allowed = sum(Allowed, na.rm = TRUE),
#       Dismissed = sum(Dismissed, na.rm = TRUE)
#     ) |> 
#     ungroup()
# 
# grant_rates_initial_year <- 
#   grant_rates_initial_annual |>
#   group_by(Year) |>
#   summarise(
#     Grant = sum(Grant, na.rm = TRUE),
#     Refused = sum(Refused, na.rm = TRUE)
#   ) |>
#   ungroup()
# 
# grant_rates_initial_year |> 
#   left_join(grant_rates_at_appeal_year) |> 
#   drop_na() |> 
#   mutate(`Final grant rate` = (Grant + Allowed) / (Grant + Refused + Allowed + Dismissed))

# - Grant rates by nationality -
# Top five nations, by number of grants and grant rate in the most recent year
# top_five_nations <- 
#   grant_rates_at_appeal |> 
#   filter(Year == max(Year)) |> 
#   arrange(desc(`Grant rate at appeal`), desc(Allowed)) |> 
#   slice(1:5) |> 
#   pull(Nationality)

# Make a wider version of initial grant rates quarterly data for testing in a Flourish Studio chart
# grant_rates_at_appeal |> 
#   select(Year, Nationality, `Grant rate at appeal`) |> 
#   pivot_wider(names_from = Nationality, values_from = `Grant rate at appeal`) |> 
#   
#   # Move the five nations with the highest number of grants and highest grant rates to the left, so they get shown on the chart by default
#   relocate(Year, any_of(top_five_nations)) |> 
#   
#   write_csv("data-raw/flourish/3b - Decision-making/grant rates at appeal - by nationality.csv")

# Asylum support payments offcuts
# What proportion of people receiving Section 95 support only receive subsistence?
# support_received_recently |> 
#   filter(`Support Type` == "Section 95") |> 
#   mutate(Proportion = People / sum(People))

# asylum::support_received |> 
#   filter(`Support Type` == "Section 98") |> 
#   filter(str_detect(`Accommodation Type`, "Hotel")) |> 
#   group_by(Date, `Support Type`, `Accommodation Type`) |> 
#   summarise(People = sum(People)) |> 
#   ungroup()

