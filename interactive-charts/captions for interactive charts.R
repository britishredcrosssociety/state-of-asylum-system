library(tidyverse)
library(asylum)

# ---- Calculations for captions ----

# ---- 1. asylum applications and backlog ----
# 1. could we please have 'Asylum backlog has increased by x% and the asylum applications has increased by x% over the last 5 years.'

# Change in backlog 
changeinbacklog <- (awaiting_decision |>
  group_by(Date) |>
  summarise(Total = sum(Applications))) 

# In June 2017, there were 31,359 applications in the backlog. In June 2023, there were 172,758 in the backlog. 
# In the 5 years, there has been a 450% change over the last five years. 

((172758 - 31359)/31359)*100 
# = 450.905

# Change in backlog no dependent decisions
changeinbacklognodependents <- (awaiting_decision |>
                                  filter(`Applicant type`!= "Dependant") |>
                                  group_by(Date) |>
                                  summarise(Total = sum(Applications)))

# In June 2017, there were 23,416 main applications in the backlog. In June 2023, there were 	
# 133,607 main applicants in the backlog. In the last 5 years, there has been a 470% change 
# in the number of main applicants in the asylum backlog. 

((133607 - 23416)/23416)*100 
# = 470.5799


# ---- Change in applications from June 2017 to June 2023 ----

changeinapplications <- (applications |>
  group_by(Date) |>
  summarise(Total = sum(Applications)))

# In June 2017, there were 8039 applications made for asylum, in June 2023, there were 19377 applications made. 

((19377-8039)/8039)*100

# = 141.0374

changeinapplicationsnod <- (applications |>
                           filter(`Applicant type` != "Dependant") |>
                           group_by(Date) |>
                           summarise(Total = sum(Applications)))

# In June 2017, there were 6234 applications made for asylum by main applicants. In June 2023, there were 15698 applications by main applicants for asylum. 

((15698 - 6234)/6234)*100

# 151.8126




# ---- 2. NRM referrals by age and sex ----
# could we please have 'X% of referrals were female and x% were male in the last 12 months'

nrm_referral_sex_age <- (nrm_referrals |>
  filter(`Age at exploitation` != "Total") |>
  filter(Gender != "Total") |>
  filter(Nationality != "Total")|>
  filter(`Age at exploitation` != "Not specified or unknown"))

nrm_referral_sex_age |>
  group_by(Gender, `Age at exploitation`) |>
  summarise(Total = sum(People)) |> view()

#Female
#2937

#Male
#10974

#Not Recorded
#1

#Other
#12

1143 + 1671 + 5305 + 4841
# = 12960 

# Female % of NRM for 12 months 

((1143 + 1671)/12960)*100
# = 21.7% of NRM referrals over the last 12 months were female. 

((5305 + 4841)/12960)*100 
# = 78.3% of NRM referrals over the last 12 months were male 


# ---- 3. Positive reasonable grounds ----
#could we please say 'There has been an x% decrease in the number of positive reasonable grounds decisions for adults in the second quarter of 2023 compared to the first quarter.'

((461 - 1011)/1011)*100

# = -54.4
# There has been a 54% decrease in the number of positive reasonable ground decisions for adults from the first quarter of 2023 to the second quarter. 

# ---- 4. Positive conclusive grounds
# There has been an x% decrease in the number of positive conclusive grounds decisions for adults in the second quarter of 2023 compared to the first quarter.'

((695 - 807)/807)*100
 
# = -13.87856
# There has been a 14% decrease in the number of positive conclusive ground decisions for adults in the second quarter of 2023 compare to the first quarter. 

# ---- 5. What is the number of people coming over by small boat, resettlement, family reunion 

arrival_all <- (arrivals_all |>
                  pivot_longer(cols = `People arriving via small boat and claiming asylum`:`People resettled`,
                               names_to = "Arrival method",
                               values_to = "People"))

arrival_all <- arrival_all |>
  select(`Arrival method`, People)

Groupedbymethod <- arrival_all |>
  group_by(`Arrival method`) |>
  summarise(Total = sum(People)) |> view()

Groupedbymethod |>
summarise(TotalG = sum(Total))

# = 102966

# Numbers: 
#Family reunion visas granted
(4671/102966)*100

# = 4.5364%

#People arriving via other routes and claiming asylum
(61222/102966)*100

# = 59.45846

#People arriving via small boat and claiming asylum
(36168/102966)*100

# = 35.12616% 

#People resettled
(905/102966)*100

# = 0.8789 %