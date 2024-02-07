# Code for mock plot- IMA, NABA and IMB data 

# All data is provisional. 

# There was an issue with using is.na in r or using replace_na for this dataset when it comes to data wrangling. Please revise. 
# May be the way the data is formatted for this provisional data? Had to manually go through and add 0 and remove -/NA. 

#Source : https://www.gov.uk/government/statistics/statistics-relating-to-the-illegal-migration-bill#documents

library(readxl)

statistics_relating_to_the_illegal_migration_act_data_tables_to_dec_2023 <- read_excel("C:/Users/JennyR/Downloads/statistics-relating-to-the-illegal-migration-act-data-tables-to-dec-2023.xlsx", 
                                                                                         sheet = "IMB_02", skip = 3, n_max = 33)
View(statistics_relating_to_the_illegal_migration_act_data_tables_to_dec_2023)

IMA_backlog <- statistics_relating_to_the_illegal_migration_act_data_tables_to_dec_2023

# Rename the column to "Date"
IMA_backlog <- IMA_backlog |>
  rename("Date" = "As at...")

# Change character to date in the dataset to read better. 
IMA_backlog$Date <- dmy(IMA_backlog$Date)

#Filter out records prior to March 2022
IMA_backlog <-   IMA_backlog |> filter(Date >= "2022-03-31")


# save the data as a csv to upload to Flourish. 
IMA_backlog |>
  write_csv("data-raw/flourish/7 - other and mock/mock IMA.csv")

