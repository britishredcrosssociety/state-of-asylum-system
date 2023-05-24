library(tidyverse)
library(asylum)
library(plotly)

# Calculate initial grant rates and final grant rates by region and nationality
grant_rates <- 
  asylum::outcomes |> 
  mutate(
    `Total final outcomes` = `Final outcome: Grants of asylum` + 
                             `Final outcomes: Grants of HP/DL and other` +
                             `Final outcomes: Refused asylum or HP or DL or other leave` +
                             `Final outcome: Withdrawn application`
  ) |> 
  mutate(
    `Initial grant rate` = (`Granted asylum` + `Granted HP/DL` + `Other grants`) / `Initial decisions`,
    `Final grant rate` = (`Final outcome: Grants of asylum` + `Final outcomes: Grants of HP/DL and other`) / (`Total final outcomes` - `Final outcome: Withdrawn application`)
  ) |> 
  select(Year = `Year of application`, Region = `Geographical region`, Nationality, `Initial grant rate`, `Final grant rate`) |> 
  pivot_longer(cols = contains("grant"), names_to = "Period", values_to = "Rate")

# Calculate total grant rates
grant_rates_total <- 
  outcomes |> 
  filter(Nationality != "Total") |> 
  mutate(
    `Total final outcomes` = `Final outcome: Grants of asylum` + 
      `Final outcomes: Grants of HP/DL and other` +
      `Final outcomes: Refused asylum or HP or DL or other leave` +
      `Final outcome: Withdrawn application`
  ) |> 
  
  group_by(`Year of application`) |> 
  summarise(
    `Initial decisions` = sum(`Initial decisions`),
    `Granted asylum` = sum(`Granted asylum`),
    `Granted HP/DL` = sum(`Granted HP/DL`),
    `Other grants` = sum(`Other grants`),
    `Total final outcomes` = sum(`Total final outcomes`),
    `Final outcome: Grants of asylum` = sum(`Final outcome: Grants of asylum`),
    `Final outcomes: Grants of HP/DL and other` = sum(`Final outcomes: Grants of HP/DL and other`),
    `Final outcome: Withdrawn application` = sum(`Final outcome: Withdrawn application`)
  ) |> 
  ungroup() |> 
  
  mutate(
    `Initial grant rate` = (`Granted asylum` + `Granted HP/DL` + `Other grants`) / `Initial decisions`,
    `Final grant rate` = (`Final outcome: Grants of asylum` + `Final outcomes: Grants of HP/DL and other`) / (`Total final outcomes` - `Final outcome: Withdrawn application`)
  ) |> 
  
  select(Year = `Year of application`, `Initial grant rate`, `Final grant rate`) |> 
  pivot_longer(cols = contains("grant"), names_to = "Period", values_to = "Rate")

# Copy totals into grant_rates
grant_rates <- 
  bind_rows(
    grant_rates,
    grant_rates_total |> mutate(Region = "Total", Nationality = "Total")
  ) |> 
  mutate(Period = factor(Period, levels = c("Initial grant rate", "Final grant rate")))

# ---- Plot average grant rates by region ----
# Check distribution of grant rates
grant_rates |> 
  filter(Period == "Initial grant rate" & Region == "Middle East" & Year == 2019) |> 
  ggplot(aes(x = Rate)) +
  geom_histogram(binwidth = .1)

plt_rates_region <- 
  grant_rates |> 
  
  group_by(Year, Period, Region) |> 
  summarise(
    Rate_mean = mean(Rate, na.rm = TRUE),
    Rate_median = median(Rate, na.rm = TRUE),
    Rate_sd = sd(Rate, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  
  # filter(Region %in% c("Total", "Middle East")) |> 
  
  ggplot(aes(x = Year, y = Rate_median, group = Region)) +
  # geom_ribbon(aes(ymin = Rate_median - Rate_sd, ymax = Rate_median + Rate_sd, fill = Region), alpha = 0.2) +
  geom_line(aes(colour = Region)) +
  facet_wrap(~Period) +
  theme_classic() +
  theme(
    legend.position = "bottom"
  )

ggplotly(plt_rates_region)

# ---- Plot by (selected) nationalities ----
plt_rates_nation <- 
  grant_rates |> 
  # filter(Region %in% c("Asia Central", "Middle East")) |> 
  filter(Nationality %in% c("Afghanistan", "Iran", "Libya", "Syria")) |> 
  
  ggplot(aes(x = factor(Year), y = Rate, group = Nationality)) +
  geom_line(aes(colour = Nationality), alpha = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~Period) +
  theme_classic() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    x = "Year",
    y = "Grant rate"
  )

ggplotly(plt_rates_nation)

# ---- Grant rate stats/summaries ----
# What was the average grant rate for Syrians during 2014-2021 (the same time period the Vulnerable Persons Resettlement Scheme was running)?
grant_rates |> 
  filter(Nationality == "Syria" & Year >= 2014) |> 
  group_by(Period) |> 
  summarise(
    Rate_mean = mean(Rate, na.rm = TRUE),
    Rate_median = median(Rate, na.rm = TRUE),
    rate_max = max(Rate, na.rm = TRUE)
  )

# Similarly, calculate average rates for Libyans since grants spiked in 2017
grant_rates |> 
  filter(Nationality == "Libya" & Year >= 2017) |> 
  group_by(Period) |> 
  summarise(
    Rate_mean = mean(Rate, na.rm = TRUE),
    Rate_median = median(Rate, na.rm = TRUE),
    rate_max = max(Rate, na.rm = TRUE)
  )
