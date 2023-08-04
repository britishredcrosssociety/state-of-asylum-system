library(readxl) %>%
  RS_Actions_June_22_to_23 <- read_excel("C:/Users/MathuraKugan/OneDrive - British Red Cross Society/RS Actions June 22 to 23.xlsx") %>%
  View(RS_Actions_June_22_to_23) 
    
RS_Actions_June_22_to_23 <- RS_Actions_June_22_to_23 %>%
    select("Action_Date",
           "Action_Category",
           "Project_Type",
           "Response_Type",
           "Main_CountryofOrigin",
           "Main_Gender",
           "Main_Immigration_Status")


# Question: How many people have we supported through refugee support and anti-trafficking services? 

RS_Actions_June_22_to_23 %>%
  group_by(Action_Date, Response_Type) %>%
  summarise(Total = sum())

# Question: Who have we suppoedted (age, gender, nationality)?

# Question: How have we supported people in the last 12 months? 