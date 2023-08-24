library(tidyverse)
library(asylum)
source("report/brc_colours.R")
source("report/theme_brc.R")

# ---- QUESTION: WHAT IS THE ASYLUM GRANT RATE AT INITIAL DECISION AND APPEAL? ----

# ---- 1. Appeals ----
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
    subtitle = "Decisions on asylum claims should be made quickly and should be right the first time",
    x = "Year",
    y = "Number of appeals",
    caption = "British Red Cross analysis of Home Office data, March 2010 to March 2023")

# ---- 2. Appeals Lodged by Nationality in 2023 ----
appeals_2023 <- appeals_total |>
  filter(Date >= max(Date) -dmonths(11)) |>
  group_by(Nationality) |>
  summarise(Total2023 = sum(Total))

appeals_2023$Nationality <- factor(appeals_2023$Nationality, levels = appeals_2023$Nationality[order(appeals_2023$Total2023, decreasing = TRUE)])

appeals_2023 |>
  slice_max(Total2023, n = 10) |>
  ggplot(aes(x = Nationality, y = Total2023)) +
  geom_col(fill = brc_colours$red_dunant, show.legend = NULL) +
  geom_text(aes(label = scales::comma(Total2023)), show.legend = FALSE, size = rel(3), position=position_dodge(width=0.5), vjust=-0.25) +
  theme_brc() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.5)) +
  labs(title = "Number of appeals lodged by nationalities from 2022 to 2023 ", 
       subtitle = "Top 10 countries with appeals lodged at the First-Tier Tribunal",
       x = "Nationality", 
       y = "Number of appeals", 
       caption = "British Red Cross analysis of Home Office Data, March 2022 to March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 15000), expand = c(0,NA))


# ---- 3. Grant Rate ----
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

 ---- # 4. Asylum Returns ----

view(returns_asylum)

return_asylum <- returns_asylum |> 
  pivot_longer(cols = `Enforced returns`:`Refused entry at port and subsequently departed`, names_to = "Return Method", values_to = "People") 

return_asylum <- return_asylum |>
  pivot_longer(cols = Category, names_to = "Category", values_to = "Return related")
  
return_asylum |>
  filter(`Return related` == "Asylum-related") |>
  filter(Nationality != "Other") |>
  ggplot(aes(fill = `Return Method`, x = reorder(Nationality, desc(People), sum), y = People)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_brc() +
  labs(title = "Nationality of asylum-related returns by method of return as of March 2023",
       x = "Nationality",
       y = "Number of returned people",
       caption = "British Red Cross analysis of Home Office data, year ending March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 1000), expand = c(0,NA)) +
  scale_fill_manual(values = c(brc_colours$red_light,
                               brc_colours$red_deep,
                               brc_colours$red_mercer))

 ---- # 5. Return by category ----
return_asylum |>
  filter(Nationality != "Other") |>
  group_by(Nationality, `Return related`) |>
  summarise(Total = sum(People)) |>
  ggplot(aes(fill = `Return related`, x = reorder(Nationality, desc(Total), sum), y = Total)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_brc() +
  labs(title = "Number of asylum-related and non-asylum related returns as of March 2023",
       x = "Nationality",
       y = "Number of people returned",
       caption = "British Red Cross analysis of Home Office data, year ending March 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 8000), expand = c(0,NA)) +
  scale_fill_manual(values = c(brc_colours$red_dunant,
                               brc_colours$red_earth))
  
---- # 6. Initial and final grant rates ----
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

grant_rates_initial_final <- grant_rates_initial_final |>
  pivot_longer(cols = `Initial grant rate`:`Final grant rate`, names_to = "Initial or Final", values_to = "Grant Rate")

grant_rates_initial_final |>
  ggplot(aes(`Year of application`, `Grant Rate`)) +
  geom_line(aes(colour = `Initial or Final`)) +
  geom_point(aes(size = `Grant Rate`, alpha = 0.4, colour = `Initial or Final`), show.legend = FALSE) +
  geom_text(aes(label = scales::percent(`Grant Rate`, accuracy = 1)), show.legend = FALSE, size = rel(4)) + 
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, NA)) +
  scale_x_continuous(breaks = c(2001:2023)) +
  theme_brc() +
  labs(
    title = "Average asylum grant rate at initial decision and final decision from 2004 to 2021",
    #subtitle = "Proportion of initial decisions which resulted in a grant of protection or other leave",
    x = "Year",
    y = "Grant rate (%)",
    caption = "British Red Cross analysis of Home Office data, March 2004 to March 2021") +
  scale_color_manual(values = c(brc_colours$red_dunant,
                                brc_colours$teal))

---- # 7. Grant Rate Initial and Appeals ----

#Creating new data frame of appeals lodged and grant rate at initial decision. 
appeals_by_year <- appeals_lodged |>
  group_by(Year) |>
  summarise(Total = sum(`Appeals lodged`))

grant_2010_onwards <- grant_rate_by_year |>
  filter(Year > "2009")

appeals_and_grant <- data.frame(grant_2010_onwards,appeals_by_year$Total)

appeals_and_grant <- appeals_and_grant |>
  rename("Appeals" = appeals_by_year.Total)

appeals_and_grant$GrantRate <- (100*appeals_and_grant$GrantRate)

# Plot of Appeals lodged x grant rate 

ggplot(appeals_and_grant) +
  geom_col(aes(x = Year, y = `Appeals`), fill = brc_colours$red_dunant, colour = brc_colours$red_dunant) +
  geom_line(aes(x = Year, y = 100*`GrantRate`), stat = "identity", colour = brc_colours$black_shadow) +
  geom_text(aes(x = Year, y = `Appeals`, label = scales::comma(`Appeals`)), show.legend = FALSE, size = rel(2.5), vjust = -1) +
  theme_brc() +
  labs(title = "Grant rate at initial decision and number of appeals lodged from 2010 to 2023",
       x = "Year",
       y = "Number of appeals lodged",
       caption = "British Red Cross analysis of Home Office data, March 2010 to March 2023") +
  scale_y_continuous(labels = scales::comma, 
                     limits = c(0,15000), 
                     expand = c(0,NA),
                     sec.axis=sec_axis(
    ~.*0.0001,name="Initial grant rate", labels=scales::percent)) +
  scale_x_continuous(breaks = c(2010:2023))
#scales::rescale()??
  