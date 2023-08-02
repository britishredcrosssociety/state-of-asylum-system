library(tidyverse)
library(asylum)
source("report/brc_colours.R")
source("report/theme_brc.R")

# ---- QUESTION: What is the cost and productivity of the home office? 

# ---- Cost & Productivity ----
view(asylum_costs_and_productivity) 

# ---- Cost of the System ---- 
ggplot(asylum_costs_and_productivity) +
  geom_col(aes(x = `Financial Year`, y = `Total Asylum Costs`), colour = brc_colours$red_dunant, fill = brc_colours$red_dunant) +
  geom_text(aes(x = `Financial Year`, y = `Total Asylum Costs`, label = scales::comma(`Total Asylum Costs`)), vjust = -0.5, show.legend = FALSE, size = rel(3)) +
  theme_classic() +
  scale_y_continuous(labels = scales::unit_format(unit = "B", scale = 1e-9)) +
  labs(title = "Cost of the asylum system, 2010/11 to 2021/22", 
       subtitle = "In 2022, the cost of the system surpassed 2 billion.",
       x = "Financial Year",
       y = " Total Cost in Billions (£)",
       caption = "British Red Cross analysis of Home Office data, from financial year 2010/11 to 2021/22")

# ---- Productivity ---- 
Productivity <- asylum_costs_and_productivity %>%
  select(`Financial Year`, Productivity) %>%
  filter(`Financial Year` > "2010/11")

Productivity |>
  ggplot(aes(`Financial Year`, Productivity), group = 1) +
  geom_line(colour = "red", group = 1) +
  geom_point(alpha = 0.5, colour = "red") +
  geom_text(aes(label = scales::comma(Productivity)), show.legend = FALSE, size = rel(3)) +
  theme_classic() +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title = "Asylum caseworker productivity from 2011/12 to 2021/22", 
       subtitle = "Average number of substantive interviews or decisions completed by asylum caseworkers each month",
       x = "Financial Year",
       y = "Productivity", 
       caption = "British Red Cross analysis of Home Office data, financial year 2011/12 to 2021/22")
