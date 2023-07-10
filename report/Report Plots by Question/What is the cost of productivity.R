----#QUESTION: What is the cost and productivity of the home office? 

----#Cost & Productivity#----
view(asylum_costs_and_productivity) 

#Cost of the System 

ggplot(asylum_costs_and_productivity) +
  geom_col(aes(x = `Financial Year`, y = `Total Asylum Costs`), colour = brc_colours$red_dunant, fill = brc_colours$red_dunant) +
  geom_text() +
  theme_classic() +
  scale_y_continuous(labels = unit_format(unit = "B", scale = 1e-9)) +
  labs(title = "Cost of the Asylum System", 
       subtitle = "In 2022, the cost of the system reached 2 billion.",
       x = NULL,
       y = " Total Cost in Billions (£)",
       caption = "British Red Cross Analyses of Home Office Data, March 2023")

#Ask designers to add the value in the last year as geomtext coming up as messy#

#Productivity 
Productivity <- asylum_costs_and_productivity %>%
  select(`Financial Year`, Productivity) %>%
  filter(`Financial Year` > "2010/11")

ProductivityGraph <- (Productivity |>
  ggplot(aes(`Financial Year`, Productivity), group = 1) +
  geom_line(colour = "red", group = 1) +
  geom_point(alpha = 0.5, colour = "red") +
  theme_classic() +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title = "Productivity of Asylum Case Workers", 
       subtitle = "Productivity of home office measured as the average number of cases complted by asylum decision makers per month each year", 
       x = NULL,
       y = "Productivity", 
       caption = "British Red Cross Analyses of Home Office Data, March, 2023"))

ProductivityGraph + geom_text(aes(label = scales::comma(Productivity)), show.legend = FALSE, size = rel(3)) 
