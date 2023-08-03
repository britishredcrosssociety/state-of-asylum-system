library(tidyverse)
library(asylum)
source("report/brc_colours.R")
source("report/theme_brc.R")

# ---- QUESTION: WHAT IS THE ASYLUM GRANT RATE AT INITIAL DECISION AND APPEAL? ----

# ---- Grant Rate and Appeals ----
grant_rate_by_year <- grant_rates_initial_annual |>
  select(Year, Grant, Refused) |>
  group_by(Year) |>
  summarise(TGrant = sum(Grant),TRefused = sum(Refused))

grant_rate_by_year <- grant_rate_by_year |>
  mutate(TotalCases = TGrant + TRefused) |>
  mutate(GrantRate = TGrant / TotalCases)

grant_rate_by_year |>
  ggplot(aes(Year, GrantRate)) +
  geom_line(aes(colour = brc_colours$red_dunant), show.legend = NULL) +
  geom_point(aes(size = GrantRate, alpha = 0.4, colour = brc_colours$red_dunant), show.legend = FALSE) +
  geom_text(aes(label = scales::percent(GrantRate, accuracy = 1)), show.legend = FALSE, size = rel(3)) + 
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, NA)) +
  scale_x_continuous(breaks = c(2001:2023)) +
  theme_brc() +
  guides(fill=guide_legend(nrow=6,byrow=TRUE), color = guide_legend(nrow=6,byrow=TRUE))+
  theme(
    legend.position = "right",
    #  legend.box = "vertical",
    #  legend.margin = margin(),
    plot.title.position = "plot") +  
  labs(
    title = "Average asylum grant rate at initial decision from 2001 to 2023 ",
    subtitle = "Proportion of initial decisions which resulted in a grant of protection or other leave",
    x = "Year",
    y = "Grant rate (%)",
    caption = "British Red Cross analysis of Home Office data, March 2001 to March 2023")


# ---- Appeals ----
appeals_total <- appeals_lodged |>
  select(Date, Nationality, Year,`Appeals lodged`) |>
  group_by(Date, Year, Nationality) |>
  summarise(Total = sum(`Appeals lodged`))

appeals_total |>
  group_by(Year) |>
  summarise(Total = sum(Total)) |>
  ggplot(aes(Year, Total)) +
  geom_line(aes(colour = brc_colours$red_dunant), show.legend = FALSE) +
  geom_point(aes(size = Total, alpha = 0.5, colour = brc_colours$red_dunant), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(3)) + 
  scale_x_continuous(breaks = c(2010:2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 15000), expand = c(0, NA)) +
  theme_brc() +
  labs(
    title = "Number of asylum appeals lodged from 2010 to 2023",  
    subtitle = "Number of appeals lodged with the first-tier tribunal where an asylum claim has been refused at initial decision",
    x = "Year",
    y = "Number of appeals",
    caption = "British Red Cross analysis of Home Office data, March 2010 to March 2023")

# ---- Appeals Lodged by Nationality in 2023 ----
appeals_2023 <- appeals_total |>
  filter(Year > "2022") |>
  group_by(Nationality) |>
  summarise(Total2023 = sum(Total))

appeals_2023$Nationality <- factor(appeals_2023$Nationality, levels = appeals_2023$Nationality[order(appeals_2023$Total2023, decreasing = TRUE)])
  
appeals_2023 |>
  filter(Total2023 > 31) |>
  ggplot(aes(x = Nationality, y = Total2023)) +
  geom_col(fill = brc_colours$red_dunant, show.legend = NULL) +
  geom_text(aes(label = scales::comma(Total2023)), show.legend = FALSE, size = rel(3), position=position_dodge(width=0.5), vjust=-0.25) +
  theme_brc() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.5)) +
  labs(title = "Number of appeals lodged by nationalities for year ending March 2023 ", 
       subtitle = "Top 10 countries with appeals lodged at the First-Tier Tribunal",
       x = "Nationality", 
       y = "Number of appeals", 
       caption = "British Red Cross analysis of Home Office Data, March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 200), expand = c(0,NA))



####Find where this is- does not belong here???
# ---- # Outcomes of Initial Decision Bar Graph#  ----
ResettlmentTotal %>%
  filter(`Case type` != "Resettlment Case") %>%
  ggplot(aes(fill = `Case outcome group`, y = RTotal, x = Year)) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  labs(title = "Number of grants, refusals or withdrawls of asylum applications at initial decision from 2001 to 2023",
       subtitle = "Inital decision on asylum application grouped by case outcome",
       x = "Year", 
       y = "Number of Decisions", 
       caption = "British Red Cross analysis of Home Office data, until January, 2023") +
  scale_x_continuous(breaks = c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 150000)) + 
  scale_fill_manual(values = c(brc_colours$red_dunant,
                               brc_colours$red_light,
                               brc_colours$red_earth,
                               brc_colours$red_deep))
