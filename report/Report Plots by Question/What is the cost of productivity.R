----#QUESTION: What is the cost and productivity of the home office? 

  ----#TO BE REVISED CODE#----
----#Cost & Productivity- to be revised#----
view(asylum_costs_and_productivity) 

asylum_costs_and_productivity |>
  ggplot(aes(`Financial Year`, `Total Asylum Costs`)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title = "Total Asylum System Cost", x = "Financial Year", y = "Cost")
