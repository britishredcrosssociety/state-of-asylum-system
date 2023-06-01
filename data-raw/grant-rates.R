library(tidyverse)
library(asylum)

grant_rates_initial_quarterly <- asylum::grant_rates_initial_quarterly

usethis::use_data(grant_rates_initial_quarterly, overwrite = TRUE)

# Top five nations, by number of grants and grant rate in the most recent year
top_ten_nations <- 
  grant_rates_initial_annual |> 
  filter(Year == max(Year)) |> 
  arrange(desc(Grant), desc(`Initial grant rate`)) |> 
  slice(1:10) |> 
  pull(Nationality)

# Make a wider version of initial grant rates quarterly data for testing in a Flourish Studio chart
grant_rates_initial_quarterly |> 
  select(Date, Quarter, Nationality, `Initial grant rate`) |> 
  pivot_wider(names_from = Nationality, values_from = `Initial grant rate`) |> 
  
  # Move the ten nations with the highest number of grants and highest grant rates to the left, so they get shown on the chart by default
  relocate(Date, Quarter, any_of(top_ten_nations)) |> 
  
  write_csv("data-raw/initial-grant-rates-quarterly-wide.csv")


grant_rates_initial_annual |> 
  filter(Year >= max(Year) - 2) |> 
  filter(Nationality %in% top_ten_nations) |> 
  select(Nationality, Year, `Initial grant rate`, `Number of grants` = Grant) |> 
  write_csv("data-raw/initial-grant-rates-annual-recent.csv")



grant_rates_initial_annual |> 
  group_by(Year) |> 
  summarise(
    Grant = sum(Grant, na.rm = TRUE),
    Refused = sum(Refused, na.rm = TRUE)
  ) |> 
  ungroup() |>
  mutate(`Initial grant rate` = Grant / (Grant + Refused)) |> 
  select(Year, `Initial grant rate`) |> 
  write_csv("data-raw/initial-grant-rates-annual-total.csv")
  
  # ggplot(aes(x = Year, y = `Initial grant rate`)) +
  # geom_line() +
  # scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  # labs(
  #   title = "Initial grant rate for all asylum applications"
  # )  
  # 


