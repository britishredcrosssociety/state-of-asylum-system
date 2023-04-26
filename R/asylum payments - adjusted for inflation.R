library(tidyverse)
library(lubridate)

# Load consumer price index from https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/d7bt/mm23
cpi <- read_csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/d7bt/mm23", skip = 8, col_names = c("Date", "CPI"))

cpi <- 
  cpi |> 
  filter(str_length(Date) > 7) |>   # Keep year + month rows
  mutate(Date = ym(Date)) |> 
  filter(Date >= ym("2000-04"))  # Match asylum payments data dates

# Asylum support payments for single adults aged 25+
# Manually taken by tracing back through amendments to The Asylum Support Regulations 2000
# Start here: https://www.legislation.gov.uk/uksi/2022/78/contents/made
asylum_payments <- 
  tribble(
    ~Date, ~Weekly_nominal,
    "2000-04", 36.54,
    "2002-04", 37.77,
    "2003-04", 38.26,
    "2005-04", 39.34,
    "2006-04", 40.22,
    "2007-04", 41.41,
    "2009-07", 42.16,
    "2010-04", 42.62,
    "2015-08", 36.95,
    "2018-02", 37.75,
    "2021-02", 39.63,
    "2022-02", 40.85
  ) |> 
  mutate(Date = ym(Date))

# Use December 2021's CPI for reindexing
cpi_dec_2021 <- 
  cpi |> 
  filter(Date == ym("2021-12")) |> 
  pull(CPI)

# Use April 2000 for reindexing
cpi_apr_2000 <- 
  cpi |> 
  filter(Date == ym("2000-04")) |> 
  pull(CPI)

real_asylum_payments <- 
  cpi |> 
  # Re-index CPI
  mutate(
    CPI_dec_2021 = CPI / cpi_dec_2021,
    CPI_apr_2000 = CPI / cpi_apr_2000
  ) |> 
  
  # Merge nominal asylum payments
  left_join(asylum_payments) |> 
  fill(Weekly_nominal, .direction = "down") |> 
  
  # Calculate real asylum payments
  mutate(
    `Real asylum payment (2021 GBP)` = Weekly_nominal / CPI_dec_2021,
    `Real asylum payment (2015 GBP)` = Weekly_nominal / (CPI / 100),
    `Real asylum payment (2000 GBP)` = Weekly_nominal / CPI_apr_2000,
    
    # This is an equivalent way to calculate asylum payments in 2021 pounds (or another year of our choosing)
    # `Real asylum payment (2021 GBP)` = Weekly_nominal * (cpi_dec_2021 / CPI)
  )

real_asylum_payments |> 
  ggplot(aes(x = Date, y = `Real asylum payment (2021 GBP)`)) +
  geom_line(size = 1.1) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.subtitle = element_text(colour = "#737373")
  ) +
  labs(
    title = "Asylum support payments",
    subtitle = "Adjusted for inflation; 2021 pounds",
    x = NULL, 
    y = NULL
  )

ggsave("analysis/winter-risk-profiles/asylum-payments.png", width = 70, height = 70, units = "mm")

# Replot with nominal pay
real_asylum_payments |> 
  rename(`Nominal asylum payment` = Weekly_nominal) |> 
  pivot_longer(cols = c(`Nominal asylum payment`, `Real asylum payment (2015 GBP)`)) |> 
  
  ggplot(aes(x = Date, y = value)) +
  geom_line(aes(colour = name), size = 1.1) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title.position = "plot",
    plot.subtitle = element_text(colour = "#737373")
  ) +
  labs(
    title = "Asylum support payments",
    subtitle = "Adjusted for inflation; 2015 pounds",
    x = NULL, 
    y = NULL,
    colour = NULL
  )

real_asylum_payments |> 
  rename(
    `Nominal payment` = Weekly_nominal,
    `Inflation-adjusted\n(in 2000 GBP)` = `Real asylum payment (2000 GBP)`
  ) |> 
  pivot_longer(cols = c(`Nominal payment`, `Inflation-adjusted\n(in 2000 GBP)`)) |> 
  
  ggplot(aes(x = Date, y = value)) +
  geom_line(aes(colour = name), size = 1.1) +
  
  geom_text(
    data = tribble(
      ~Date, ~value, ~Label,
      ym("2017-01"), 36, "Nominal payment",
      ym("2009-01"), 28.5, "Inflation-adjusted\n(in 2000 GBP)"
    ),
    aes(label = Label, colour = Label)
  ) +
  
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
  scale_colour_manual(values = c("#af8dc3", "#7fbf7b")) +
  
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.subtitle = element_text(colour = "#737373")
  ) +
  labs(
    title = "Asylum support payments",
    # subtitle = "Adjusted for inflation; 2000 pounds",
    x = NULL, 
    y = NULL,
    colour = NULL
  )

ggsave("analysis/winter-risk-profiles/asylum-payments-real-nominal.png", width = 70, height = 70, units = "mm")
