library(tidyverse)
library(asylum)

grant_rates_initial_quarterly <- asylum::grant_rates_initial_quarterly

usethis::use_data(grant_rates_initial_quarterly, overwrite = TRUE)
