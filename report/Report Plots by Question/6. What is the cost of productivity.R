library(tidyverse)
library(asylum)
source("report/brc_colours.R")
source("report/theme_brc.R")

# ---- QUESTION: What is the cost and productivity of the home office? 

# ---- 3. Cost of the System ---- 
ggplot(asylum_costs_and_productivity, aes(x = `Financial Year`, y = `Total Asylum Costs`)) +
  geom_col(colour = brc_colours$red_dunant, fill = brc_colours$red_dunant) +
  geom_text(aes(label = scales::dollar(`Total Asylum Costs`, prefix = "£")), vjust = -0.5, show.legend = FALSE, size = rel(3)) +
  theme_brc() +
  scale_y_continuous(labels = scales::dollar_format(prefix = "£", suffix = "b", scale = 1e-9, accuracy = 0.1), limit = c(0, 4500000000), expand = c(0, NA)) +
  labs(title = "Annual cost of the asylum system from 2010/11 to 2022/23",
       subtitle = "In the financial year 2022/23, the cost of the asylum system cost nearly 4 billion (£)", 
       x = "Financial year",
       y = " Total cost (£ billions)",
       caption = "British Red Cross analysis of Home Office data, financial year 2010/11 to 2022/23")

# ---- Productivity ---- 
Productivity <- asylum_costs_and_productivity %>%
  select(`Financial Year`, Productivity) %>%
  filter(`Financial Year` > "2010/11")

Productivity |>
  ggplot(aes(`Financial Year`, Productivity), group = 1) +
  geom_line(colour = brc_colours$red_dunant, group = 1) +
  geom_point(alpha = 0.4, colour = brc_colours$red_dunant, size = 6) +
  geom_text(aes(label = scales::comma(Productivity)), show.legend = FALSE, size = rel(3)) +
  theme_brc() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 20), expand = c(0, NA)) +
  labs(title = "Asylum caseworker productivity over the last decade", 
       subtitle = "Average number of substantive interviews or decisions completed by asylum caseworkers each month",
       x = "Financial Year",
       y = "Productivity", 
       caption = "British Red Cross analysis of Home Office data, financial year 2011/12 to 2022/23")
