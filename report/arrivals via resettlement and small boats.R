library(tidyverse)
library(asylum)

source("report/brc_colours.R")
source("report/theme_brc.R")

resettlement_grants_without_evacuation <- 
  asylum::decisions_resettlement |> 
  filter(`Case type` == "Resettlement Case" & `Case outcome group` == "Grant of Protection") |> 
  filter(!str_detect(`Case outcome`, "Relocation - ARAP")) |> 
  filter(!str_detect(`Case outcome`, "Resettlement - ACRS Pathway 1")) |> 
  filter(!str_detect(`Case outcome`, "Resettlement - ACRS Pathway 3"))

# ---- People arriving in the UK through resettlement ----
resettlement_by_quarter <- 
  resettlement_grants_without_evacuation |> 
  group_by(Date) |> 
  summarise(resettlement = sum(Decisions, na.rm = TRUE)) |> 
  ungroup() |> 
  
  # Manually add missing rows - no resettlement during lockdowns
  add_row(Date = ymd("2020-06-30"), resettlement = 0) |>
  add_row(Date = ymd("2020-09-30"), resettlement = 0) |>
  
  arrange(Date)

boats <- 
  asylum::irregular_migration |> 
  filter(str_detect(`Method of entry`, "boat")) |> 
  group_by(Date) |> 
  summarise(`small boats` = sum(`Number of detections`, na.rm = TRUE))

resettlement_by_quarter |> 
  left_join(boats) |> 
  drop_na() |> 
  
  pivot_longer(cols = -Date, names_to = "Type", values_to = "Value") |> 
  
  ggplot(aes(x = Date, y = Value, group = Type)) +
  geom_line(aes(colour = Type), linewidth = 1.2) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c(brc_colours$red_dunant, brc_colours$teal)) +
  theme_brc() +
  labs(
    title = "People arriving via resettlement routes and small boats",
    x = NULL,
    y = "Number of people",
    colour = "People arriving by",
    caption = "British Red Cross analysis of Home Office data"
  )

ggsave("report/output/arrivals via resettlement routes and small boats.png", width = 200, height = 150, units = "mm")
