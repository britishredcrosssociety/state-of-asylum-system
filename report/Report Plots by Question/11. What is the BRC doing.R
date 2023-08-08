library(tidyverse)
library(asylum)
library(readxl) 
  RS_Actions_June_22_to_23 <- readxl::read_excel("C:/Users/MathuraKugan/OneDrive - British Red Cross Society/RS Actions June 22 to 23.xlsx") %>%
 
  view(RS_Actions_June_22_to_23) 

RS_Actions_June_22_to_23 <- 
  
  RS_Actions_June_22_to_23 %>%  
  group_by(Birth_Date) %>%
  summarise(UAM = sum(UnaccompaniedMinor)) %>% view()
  
##Review this with Matt on how to group the data
    
RS_Actions_June_22_to_23 <- RS_Actions_June_22_to_23 %>%
    select("Action_Date",
           "Action_Category",
           "Project_Type",
           "Response_Type",
           "Main_CountryofOrigin",
           "Main_Gender",
           "Main_Immigration_Status")


# Question: How many people have we supported through refugee support and anti-trafficking services? 


# Question: Who have we supported (age, gender, nationality)?

# Question: How have we supported people in the last 12 months? 