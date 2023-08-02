library(tidyverse)
library(asylum)
source("report/brc_colours.R")
source("report/theme_brc.R")

# ---- QUESTION: HOW MANY CLAIMS HAVE BEEN DEEMED INADMISSABLE? ---- 

# ---- Inadmissible ----
# To discuss with the team if this revised graph is better?
Inadmissable <- inadmissibility_cases_considered %>%
  select(Date, Year, Stage, Cases, Quarter) %>%
  group_by(Date, Year, Quarter, Stage) %>%
  summarise(TotalCases = sum(Cases))

Inadmissable |>
  ggplot(aes(Date, TotalCases)) +
  geom_line(aes(group = Stage, colour = Stage)) +
  geom_point(aes(colour = Stage), size = 1) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_color_manual(values = c(get_brc_colours()$teal, get_brc_colours()$green, get_brc_colours()$red, get_brc_colours()$blue_light, get_brc_colours()$black)) +
  scale_fill_manual(values = c(get_brc_colours()$teal, get_brc_colours()$green, get_brc_colours()$red, get_brc_colours()$blue_light, get_brc_colours()$black)) +
  theme_classic() +
  guides(fill=guide_legend(nrow=6,byrow=TRUE), color = guide_legend(nrow=6,byrow=TRUE))+
  theme(
    legend.position = "right",
    #  legend.box = "vertical",
    #  legend.margin = margin(),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 12)
  ) +
  labs(
    title = "Inadmissable Cases by Stage",
    x = "6 Month Intervals",
    y = "Number of Cases",
    caption = "British Red Cross Mock Analyses."
  )

# ---- Inadmissable Sankey ----
#  Create sample data
# InadmissibleforSankey <- data.frame(inadmissibility_cases_considered,
#                                     Year = c("2021", "2022", "2023"),
#                                     stage = c("Stage 1", "Stage 2", "Stage 1", "Stage 2", "Stage 1"),
#                                     count = c(100, 70, 80, 50, 120)
# )
# 
# #  Create Sankey diagram
# sankey(data, from = country, to = stage, weight = count)
