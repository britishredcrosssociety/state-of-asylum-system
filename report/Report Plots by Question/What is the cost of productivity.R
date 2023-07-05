----#QUESTION: What is the cost and productivity of the home office? 

----#Cost & Productivity#----
view(asylum_costs_and_productivity) 

#Cost of the System 
asylum_costs_and_productivity |>
  ggplot(aes(`Financial Year`, `Total Asylum Costs`), group = 1) +
  geom_line(group = 1, colour = "red") +
  geom_point(alpha = 0.5, colour = "red") +
  theme_classic() +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title = "Total Asylum System Cost", x = "Financial Year", y = "Cost")


#Productivity 
Productivity <- asylum_costs_and_productivity %>%
  select(`Financial Year`, Productivity) %>%
  filter(`Financial Year` > "2010/11")

Productivity |>
  ggplot(aes(`Financial Year`, Productivity), group = 1) +
  geom_line(colour = "red", group = 1) +
  geom_point(alpha = 0.5, colour = "red") +
  theme_classic() +
  labs(title = "Productivity of Asylum System", 
       y = "Productivity", 
       caption = "BRC Analyses of HO Data, March, 2023")

