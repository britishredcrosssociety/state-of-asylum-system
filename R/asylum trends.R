library(tidyverse)
library(asylum)
library(ggrepel)

nations <- c("Afghanistan", "Eritrea", "Iran", "Sudan", "Syria")
years <- c(2019, 2022)

# People resettled
sum_resettled <- 
  asylum::decisions_resettlement |> 
  filter(Nationality %in% nations, Year %in% years, `Case type` == "Resettlement Case") |> 
  group_by(Nationality, Year) |> 
  summarise(`Resettled` = sum(Decisions))

# Family reunion visas
sum_reunion <- 
  family_reunion |> 
  filter(Nationality %in% nations, Year %in% years) |> 
  group_by(Nationality, Year) |> 
  summarise(`Family reunion` = sum(`Visas granted`))

# Channel crossings
sum_crossings <- 
  irregular_migration |> 
  filter(Nationality %in% nations, Year %in% years, `Method of entry` == "Small boat arrivals") |> 
  group_by(Nationality, Year) |> 
  summarise(`Channel crossings` = sum(`Number of detections`))

# Asylum claims
sum_applications <- 
  asylum::applications |> 
  filter(Nationality %in% nations, Year %in% years, `Applicant type` == "Main applicant") |> 
  group_by(Nationality, Year) |> 
  summarise(`Asylum applications` = sum(Applications))

# Combine data
asylum_trends <- 
  sum_resettled |> 
  left_join(sum_reunion) |> 
  left_join(sum_crossings) |> 
  left_join(sum_applications)

write_csv(asylum_trends, "analysis/asylum-destitution/asylum-trends.csv")

asylum_trends |> 
  pivot_longer(cols = Resettled:`Asylum applications`) |> 
  
  mutate(label = if_else(Year == 2022, name, NA_character_)) |> 
  
  filter(Nationality == "Afghanistan") |> 
  ggplot(aes(x = Year, y = value, group = name)) +
  geom_line(aes(colour = name)) +
  geom_point(aes(colour = name)) +
  
  geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE) +
  
  scale_y_continuous(labels = scales::comma) +
  
  theme_minimal() +
  theme(legend.position = "none") +
  
  labs(
    y = "Number of people"
  )

# ---- Asylum claims from Albania ----
albania <- 
  asylum::applications |> 
  filter(Nationality == "Albania" & `Applicant type` == "Main applicant") |> 
  select(Date, Nationality, Location = `Location of application`, Applications) |> 
  group_by(Location, Date) |> 
  summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
  ungroup()

albania |> 
  ggplot(aes(x = Date, y = Applications, group = Location)) +
  geom_line(aes(colour = Location), linewidth = 1.5) +
  theme_classic() +
  labs(
    title = "Number of people claiming asylum from Albania"
  )

albania |> 
  mutate(Year = year(Date)) |> 
  group_by(Year) |> 
  summarise(Applications = sum(Applications)) |> 
  ungroup() |>  

  arrange(Year) |>   
  mutate(percent_change = (Applications - lag(Applications)) / lag(Applications)) |> 
  
  ggplot(aes(x = Year, y = percent_change)) +
  geom_line(linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  labs(
    title = "Year-on-year % change in people claiming asylum from Albania"
  )

# Within each year, which quarter has the highest number of applications?
albania |> 
  mutate(
    Year = year(Date),
    Quarter = quarter(Date)
  ) |> 
  
  group_by(Location, Year) |> 
  slice_max(Applications, n = 1) |> 
  ungroup() |> 
  
  count(Location, Quarter)

# ---- "Irregular" arrivals, highlighting those from Albania ----
irregular_albania <- 
  asylum::irregular_migration |> 
  group_by(Date, `Method of entry`, Nationality) |> 
  summarise(Detections = sum(`Number of detections`, na.rm = TRUE)) |> 
  ungroup() |> 
  
  mutate(label = if_else(Nationality == "Albania", "red", "grey90"))

irregular_albania |> 
  filter(Nationality != "Albania") |> 
  
  ggplot(aes(x = Date, y = Detections, group = Nationality)) +
  geom_line(colour = "grey90") +
  geom_line(data = irregular_albania |> filter(Nationality == "Albania"), colour = "red") +
  facet_wrap(~`Method of entry`, scales = "free_y") +
  scale_colour_identity() +
  scale_y_continuous(labels = scales::comma) +
  theme_classic() +
  labs(
    title = "Number of \"irregular\" arrivals detected at the UK border, \nby method of entry",
    subtitle = "Red lines show arrivals from Albania; grey lines are other nations."
  )
