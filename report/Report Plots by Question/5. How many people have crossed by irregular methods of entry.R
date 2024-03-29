library(tidyverse)
library(asylum)
source("report/brc_colours.R")
source("report/theme_brc.R")

# ---- QUESTION:HOW MANY PEOPLE HAVE ARRIVED THROUGH "IRREGULAR" ENTRY?  
smallboat <- irregular_migration |>
  filter(`Method of entry`== "Small boat arrivals") 
  
# ---- 1. Nationalities Small Boat ----
SmallBoatNat <- smallboat |>
  filter(Date >= max(Date) -dmonths(11)) |>
  select(Year, Nationality, `Number of detections`) |>
  group_by(Nationality) |>
  summarise(Total = sum(`Number of detections`)) |> 
  ungroup()

SmallBoatNat$Nationality <- factor(SmallBoatNat$Nationality, levels = SmallBoatNat$Nationality[order(SmallBoatNat$Total, decreasing = TRUE)])

SmallBoatNat |>
  slice_max(Total, n = 11) |> 
  filter(Nationality != "Not currently recorded") |>
  ggplot(aes(Nationality, Total)) +
  geom_col(fill = brc_colours$red_dunant, show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(3),vjust = -1, colour = brc_colours$black_shadow) +
  #coord_flip() +
  theme_brc() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5)) +
  labs(title = "Top 10 nationalities detected crossing the Channel in small boats",
       #subtitle = "Top 10 nationalities detected crossing the channel",
       x = "Nationalities", 
       y = "Number of people detected", 
       caption = "British Red Cross analysis of Home Office data, June 2022 to June 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 15000), expand = c(0,NA)) 

view(smallboat)

# ---- 2. Small Boat x Asylum Applications ----
SmallboatAsylum <-
  {  
  small_boat_asylum_applications |>
  select(Year, `Age Group`, Sex, Region, Applications, `Applicant type`) |>
  filter(`Applicant type` != "N/A - No asylum application")
  }

SmallboatAsylum |>
  select(Year, `Age Group`, Applications, `Applicant type`) |>
  #filter("Asylum application" == "Asylum application raised") |> 
  group_by(Year) |> 
  summarise(Total = sum(Applications)) |> 
  ggplot(aes(Year, Total)) +
  geom_point(aes(colour = brc_colours$red_dunant, size = 3), alpha = 0.5, show.legend = FALSE) +
  geom_line(aes(colour = brc_colours$red_dunant), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total)), show.legend = FALSE, size = rel(3)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 50000)) +
  theme_brc() +
  labs(title = str_wrap("Number of asylum applications from people crossing the Channel in small boats from 2018 to 2023"),
       x = NULL, 
       y = "Number of applications", 
       caption = "British Red Cross analysis of Home Office data, March 2018 to June 2023. Earliest available Home Office data is from 2018")

# ---- 2b. Asylum and small boat Q2 to Q2 ----

small_boat_asylum_annual <- 
  asylum::small_boat_asylum_applications |> 
  group_by(Date) |> 
  summarise(Total = sum(Applications, na.rm = TRUE))

# Use the `rolling_annual_sum()` function to calculate the total number of applications
# for the year ending June 2023, June 2022, June 2021 etc.
small_boat_asylum_most_recent_quarter <- 
  small_boat_asylum_annual |> 
  rolling_annual_sum(Total)

View(small_boat_asylum_most_recent_quarter)

small_boat_asylum_most_recent_quarter |>
  ggplot(aes(Date, RollingSum)) +
  geom_point(aes(colour = brc_colours$red_dunant, size = 3), alpha = 0.5, show.legend = FALSE) +
  geom_line(aes(colour = brc_colours$red_dunant), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(RollingSum)), show.legend = FALSE, size = rel(3)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 50000)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_brc() +
  labs(title = str_wrap("Number of asylum applications from people crossing the Channel in small boats from 2019 to 2023"),
       x = NULL, 
       y = "Number of applications", 
       caption = "British Red Cross analysis of Home Office data, June 2019 to June 2023. Earliest available Home Office data is from 2018")



# ---- 3. Irregular Migration by Method of Entry ----  
irregular_migration |>
  select(Year, `Method of entry`, `Number of detections`) |>
  group_by(Year, `Method of entry`) |>
  summarise(TotalbyMethod = sum(`Number of detections`)) |>
  ggplot(aes(Year, TotalbyMethod), group = `Method of entry`) +
  geom_point(aes(group = `Method of entry`, size = 3, alpha = 0.5, colour = `Method of entry`), show.legend = FALSE) +
  geom_line(aes(group = `Method of entry`, colour = `Method of entry`)) +
  geom_text(aes(label = scales::comma(TotalbyMethod)), vjust = .5, position = position_dodge(width = 1), show.legend = FALSE, size = rel(2)) +
  theme_brc() + 
  labs(title =  "Number of people detected by 'irregular' methods of entry from 2018 to 2023",
       x = "Year",
       y = "Number of people detected", 
       caption = "British Red Cross analysis of Home Office data, March 2018 to June 2023. Earliest available home office data is from 2018") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 50000), expand = c(0,NA)) +
  scale_colour_manual(values = c(brc_colours$teal,
                                 brc_colours$green_dark,
                                 brc_colours$black_shadow,
                                 brc_colours$red_dunant))

# ---- 4. Age and Sex 2022 ----
smallboat$`Age Group` <- factor(smallboat$`Age Group`, levels=c('40 and over', '25 to 39', '18 to 24', '17 and under'))

smallboat |>
  filter(`Age Group` != "Not currently recorded") |>
  filter(Sex != "Unknown") |>
  filter(Date >= max(Date) -dmonths(11)) |>
  select(Date, Year, Sex, `Age Group`, Nationality, `Number of detections`) |>
  group_by(Year, `Age Group`, Sex) %>%
  summarise(Total = sum(`Number of detections`)) |>
  ggplot(aes(fill = `Age Group`, x = Sex, y = Total)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_brc() +
  labs(title = "Small boat Channel crossings by age and sex for year ending June 2023",
       x = "Sex", 
       y = "Number of people detected",
       caption = "British Red Cross analysis of Home Office data, June 2022 to June 2023") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 40000), expand = c(0, NA)) +
  scale_fill_manual(values = c(brc_colours$red_deep,
                               brc_colours$red_earth,
                               brc_colours$red_mercer,
                               brc_colours$red_light))

# ---- 5. Small Boat x Quarter ----
Smallboatbyquarter <- smallboat |> 
  select(Year, Quarter, `Number of detections`) |>
  group_by(Year, Quarter) |>
  summarise(TotalQ = sum(`Number of detections`))

view(Smallboatbyquarter)

Smallboatbyquarter |>
  ggplot(aes(fill = factor(Quarter), x = Year, y = TotalQ)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_brc() +
  labs(title = "Number of people crossing the Channel by quarter from 2018 to 2023", 
       subtitle = "The greatest number of detections are seen in the third quarter of each year",
       x = "Year", 
       y = "Number of people detected",
       fill = "Quarter",
       caption = "British Red Cross analysis of Home Office data, January 2018 to June 2023. Earliest available Home Office data is from January 2018") +
  scale_x_continuous(breaks = c(2018:2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 50000), expand = c(0, NA)) +
  scale_fill_manual(values = c(brc_colours$red_deep,
                               brc_colours$red_earth,
                               brc_colours$red_mercer,
                               brc_colours$red_light))
