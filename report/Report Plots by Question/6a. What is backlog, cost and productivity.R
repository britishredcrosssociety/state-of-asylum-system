library(tidyverse)
library(asylum)
source("report/brc_colours.R")
source("report/theme_brc.R")

# ---- QUESTION: HOW LONG IS IT TAKING FOR DECISIONS TO BE MADE? ---- 
# ---- Time for initial decision analysis ----
# From the transparency data, we can see how many cases were completed for appeals and for initial decision using asylum::asylum_work_in_progress

backlog_total <- 
  asylum::awaiting_decision |> 
  mutate(Stage = case_when(
    Duration == "More than 6 months" ~ "Pending initial decision (more than 6 months)",
    Duration == "6 months or less" ~ "Pending initial decision (6 months or less)",
    Duration == "N/A - Further review" ~ "Pending further review"
  )) |> 
  group_by(Date, Stage) |> 
  summarise(Backlog = sum(Applications)) 
  
backlog_total |>
  filter(Stage != "Pending further review") |>
  ggplot(aes(fill = Stage, x = Date, y = Backlog)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_brc() + 
  labs(title = "Number of people waiting for an initial decision on their asylum claim from 2010 to 2023", 
       x = "Year",
       y = "Number of people", 
       caption = "British Red Cross analysis of Home Office data, June 2010 to March 2023") + 
  scale_y_continuous(labels = scales::comma, limits = c(0, 200000), expand = c(0, NA)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_fill_manual(values = c(brc_colours$red_earth,
                               brc_colours$red_dunant))


# ---- 2. Nationalities waiting for initial decision in 2022/23 ----

backlog_nationality <- 
  asylum::awaiting_decision |> 
  mutate(Stage = case_when(
    Duration == "More than 6 months" ~ "Pending initial decision (more than 6 months)",
    Duration == "6 months or less" ~ "Pending initial decision (6 months or less)",
    Duration == "N/A - Further review" ~ "Pending further review"
  )) |> 
  group_by(Date, Stage, Nationality) |> 
  summarise(Backlog = sum(Applications))


backlog_nationality |>
  filter(Stage == "Pending initial decision (more than 6 months)") |>
  filter(Date == "2023-03-31") |>
  filter(Backlog > 3400) |>
  ggplot(aes(x = reorder(Nationality, desc(Backlog)), y = Backlog)) +
  geom_col(colour = brc_colours$red_dunant, fill = brc_colours$red_dunant, show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Backlog)), show.legend = FALSE, size = rel(3), position = position_dodge(width=1), vjust=-0.25, colour = brc_colours$black_shadow) +
  theme_brc() + 
  labs(title = str_wrap("Number of people waiting for an initial decision on their asylum claim by nationality for year ending March 2023"), 
       subtitle =  "Top 10 nationalities waiting over 6 months for an initial decision",
       x = "Nationality",
       y = "Number of people", 
       caption = "British Red Cross analysis of Home Office data, March 2023") + 
  scale_y_continuous(labels = scales::comma, limits = c(0, 20000), expand = c(0, NA)) 

# ---- Grant, Refusals and Withdrawals ----

resettlement_total %>%
  #filter(`Case type` != "Resettlment Case") %>%
  ggplot(aes(fill = `Case outcome group`, y = Total, x = Year)) + 
  geom_bar(position="stack", stat="identity") +
  theme_brc() +
  labs(title = "Number of grants, refusals or withdrawls of asylum applications at initial decision from 2001 to 2023",
       subtitle = "Inital decision on asylum application grouped by case outcome",
       x = "Year", 
       y = "Number of decisions", 
       caption = "British Red Cross analysis of Home Office data, March 2001 to March 2023") +
  scale_x_continuous(breaks = c(2001:2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 150000)) + 
  scale_fill_manual(values = c(brc_colours$red_dunant,
                               brc_colours$red_light,
                               brc_colours$red_earth,
                               brc_colours$red_deep))

resettlement_total |>
  filter(`Case type` == "Resettlement Case") |>
  ggplot(aes(x = Year, y = Total)) + 
  geom_bar(position="stack", stat="identity", fill = brc_colours$red_dunant, colour = brc_colours$red_dunant) +
  theme_brc() +
  labs(title = "Number of grants of protection for resettlement cases from 2010 to 2023",
       #subtitle = "Inital decision on asylum application grouped by case outcome",
       x = "Year", 
       y = "Number of grants", 
       caption = "British Red Cross analysis of Home Office data, until January 2023") +
  scale_x_continuous(breaks = c(2010:2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 20000), expand = c(0, NA)) + 
  scale_fill_manual(values = c(brc_colours$red_dunant,
                               brc_colours$red_light,
                               brc_colours$red_earth,
                               brc_colours$red_deep))

decisions_resettlement %>%
  filter(`Case type` == "Resettlement Case") %>%
  group_by(Year) %>%
  summarise(Total = sum(Decisions)) %>%
  ggplot(aes(Year, Total)) +
  geom_bar(stat = "identity")