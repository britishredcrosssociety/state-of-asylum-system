library(tidyverse)
library(asylum)

inadmissibility_cases_considered <- asylum::inadmissibility_cases_considered

usethis::use_data(inadmissibility_cases_considered, overwrite = TRUE)
