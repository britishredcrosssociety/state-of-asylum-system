grant_rate_by_year |>
  filter(Year > 2016) |>
  summarise(Total = sum(TGrant))

view(grant_rate_by_year)

view(decisions_resettlement)

Protected <- 
  {  
  decisions_resettlement |>
  filter(`Case outcome group` == "Grant of Protection") |>
  group_by(Year, `Case outcome`) |>
  summarise(Total = sum(Decisions)) |> view()
  }

Protected |>
  filter(Year > 2015) |> view()