----#QUESTION: What is the scale of human trafficking in the UK and what is the provision of support to survivors?#----

----#Referral to NRM:----
nrm_referrals <- 
  bind_rows(
    asylum::nrm_referrals_2023_q1,
    asylum::nrm_referrals_2022_q4,
    asylum::nrm_referrals_2022_q3,
    asylum::nrm_referrals_2022_q2
  )

ByFirstResponder <- nrm_referrals |> 
  filter(`Exploitation type` == "Total" & `Age at exploitation` == "Total" & Gender == "Total" & Nationality == "Total") |> 
  filter(
    (`First responder type` == "Government agency" & `First responder` != "Government agency total") |
      (`First responder type` != "Government agency" & str_detect(`First responder`, "total"))
  ) |> 
  
  mutate(`First responder type` = if_else(`First responder type` == "Government agency", `First responder`, `First responder type`)) |> 
  
  
  group_by(`First responder type`) |> 
  summarise(People = sum(People)) 


#NRM by group#

NRMTotal <- nrm_referrals %>%
  group_by(`First responder`, `Exploitation type`, `Age at exploitation`, Gender, Nationality) %>%
  summarise(Total = sum(People))

#NRM by Age in 2022/23#

NRMAge <- nrm_referrals |> 
  filter(`Age at exploitation` != "Total" & Nationality == "Total") |> 
  filter(Gender != "Total") |> 
  
  group_by(`Age at exploitation`, Gender) |> 
  summarise(Total = sum(People)) 

NRMAge |>
  ggplot(aes(fill = `Age at exploitation`, y = Total, x = Gender)) +
  geom_bar(position ="stack", stat="identity") + 
  theme_classic() +
  labs(title = "NRM Referrals by Age and Sex, 2022/23", 
       x = "Sex", 
       y = "Number of Referrals") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))

#NRM by Nationality- tO REVISE ON HOW TO SHOW. 

nrm_referrals <- nrm_referrals |> 
  filter(Nationality != "Total") |> 
  filter(str_detect(`First responder`, "total")) |> 
  
  group_by(Nationality) |> 
  summarise(People = sum(People)) |> 
  ungroup() |> 
  
  arrange(desc(People)) 

nrm_referrals |>
  ggplot(aes(x = Nationality, y = People)) +
  geom_point(alpha = 0.5)

PositiveConclusive <- nrm_conclusive_grounds %>%
  select(Year, Quarter, `Adult (18 or over) - Positive conclusive grounds`, `Child (17 or under) - Positive conclusive grounds`, `Age not specified or unknown - Positive conclusive grounds`) 

PC <- PositiveConclusive %>%
  filter(Quarter == "Total") |>
  ggplot(aes(x = Year)) +
  geom_line(aes(x = Year, y = `Adult (18 or over) - Positive conclusive grounds`), colour = "red", show.legend = TRUE) +
  geom_line(aes(x = Year, y = `Child (17 or under) - Positive conclusive grounds`), colour = "blue", show.legend = TRUE) +
  geom_line(aes(x = Year, y = `Age not specified or unknown - Positive conclusive grounds`), colour = "black", show.legend = TRUE) +
  theme_classic() +
  labs(title = "Number of People who Recieved Positive Conclusive Grounds", 
       x = "Year", 
       y = "Number of People") +
  scale_x_continuous(breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  
  PC + scale_color_manual(name = "Age Groups", 
                          values = (c("Adult (18 or over) - Positive conclusive grounds" = "red", 
                                      "Child (17 or under) - Positive conclusive grounds" = "blue", 
                                      "Age not specified or unknown - Positive conclusive grounds" = "black"))) + 
  labs(color = 'Age Groups')

#To fix legend 

#NRM Reasonable Grounds# 

view(nrm_reasonable_grounds)

ReasonableGrounds <- nrm_reasonable_grounds %>%
  select(Year, Quarter, `Adult (18 or over) - Positive reasonable grounds`, `Child (17 or under) - Positive reasonable grounds`, `Age not specified or unknown - Positive reasonable grounds`) 

RGPlot <- ReasonableGrounds %>%
  filter(Quarter == "Total") |>
  ggplot(aes(x = Year)) +
  geom_line(aes(x = Year, y = `Adult (18 or over) - Positive reasonable grounds`), colour = "red") +
  geom_line(aes(x = Year, y = `Child (17 or under) - Positive reasonable grounds`), colour = "blue") +
  geom_line(aes(x = Year, y = `Age not specified or unknown - Positive reasonable grounds`), colour = "black") +
  theme_classic() +
  labs(title = "Number of People who Recieved Positive Reasonable Grounds", 
       x = "Year", 
       y = "Number of People") +
  scale_x_continuous(breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  
  RGPlot <- scale_colour_manual("", 
                                breaks = c("Adult (18 or over) - Positive reasonable grounds", "Child (17 or under) - Positive reasonable grounds", "Age not specified or unknown - Positive reasonable grounds"),
                                values = c("red", "green", "blue")) 
xlab(" ") +
  scale_y_continuous("Temperatura (C)", limits = c(-10,40)) + 
  labs(title="TITULO")

#Fix the legend

----##NRM Duty to Notify#----

view(nrm_duty_to_notify_2022_q2)

nrm_duty_to_notify <- bind_rows(
  asylum::nrm_duty_to_notify_2023_q1,
  asylum::nrm_duty_to_notify_2022_q4,
  asylum::nrm_duty_to_notify_2022_q3,
  asylum::nrm_duty_to_notify_2022_q2
)

DTNTotal <- nrm_duty_to_notify %>%
  group_by(Nationality) %>%
  filter(Nationality != "Total")

DTNTotal <- DTNTotal %>%
  group_by(Nationality) %>%
  summarise(Total2 = sum(Total))

DTNTotal %>%
  filter(Total2 > 100) %>%
  arrange(desc(Total2)) %>%
  ggplot(aes(Nationality, Total2)) +
  geom_point(aes(size = Total2, alpha = 0.5, colour = "red"), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Total2)), show.legend = FALSE, size = rel(3)) +
  theme_classic() +
  labs(title = "Duty to Notitify by Nationalities (Top 10)", 
       x = "Nationalities", 
       y = "Number of People") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))
