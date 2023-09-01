library(tidyverse)
library(zoo)

#' Function to calculate the rolling year-ending sum of a variable in a data.frame 
#' based on the most recently available quarter in the data.
#' 
#' @param df The data.frame containing your data
#' @param variable The name of the column you want to calculate a rolling annual sum for
#' 
rolling_annual_sum <- function(df, variable) {
  # Calculate the first quarter that needs to be in the data, based on the most recent quarter
  # e.g. if the most recent data is for Q2 in the maximum year, then the first entry should be for Q3 in the minimum year
  first_quarter_to_filter <- quarter(max(df$Date)) + 1
  first_year_in_data <- year(min(df$Date))
  first_date_to_filter <- as.Date(as.yearqtr(paste(first_year_in_data, first_quarter_to_filter), format = "%Y %q"), frac = 1)
  
  df |> 
    filter(Date >= first_date_to_filter) |> 
    arrange(Date) |> 
    mutate(RollingSum = rollapplyr({{ variable }}, width = 4, FUN = sum, fill = NA)) |> 
    filter(row_number() %% 4 == 0) |> 
    select(Date, RollingSum)
}
