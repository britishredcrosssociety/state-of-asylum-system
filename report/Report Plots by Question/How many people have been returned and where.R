library(tidyverse)
library(asylum)
source("report/brc_colours.R")
source("report/theme_brc.R")

# ---- QUESTION: HOW MANY PEOPLE HAVE BEEN RETURNED? AND TO WHERE? ----
# ---- Returned ----
# Returned Total
ReturnTotal <- returns %>%
  select(Year, `Number of returns`) %>%
  group_by(Year) %>%
  summarise(TotalReturn = sum(`Number of returns`))

ReturnTotal |>
  ggplot(aes(Year, TotalReturn)) +
  geom_line(aes(colour = "red"), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(TotalReturn)), show.legend = FALSE, size = rel(3)) +
  scale_x_continuous(breaks = c(2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  theme_brc() +
  labs(title = 'Total number of returns per year', 
       x = NULL, 
       y = 'Number of returns', 
       caption = 'British Red Cross analysis of UK Home Office data. Mock analyses')

# Returns Quarterly
ReturnsQ <- returns %>%
  select(Year, Quarter, `Number of returns`) %>%
  group_by(Year, Quarter) %>%
  summarise(TotalQReturn = sum(`Number of returns`))

ReturnsQ %>%
  filter(Year > 2021) |>
  ggplot(aes(Quarter, TotalQReturn)) +
  geom_line(aes(colour = "red"), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(TotalQReturn)), show.legend = FALSE, size = rel(3)) +
  scale_x_continuous(breaks = c(1, 2, 3, 4)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  theme_brc() +
  labs(title = 'Total numbers of returns per quarter', 
       x = 'Quarter', 
       y = 'Number of returns', 
       caption = 'British Red Cross analysis of UK Home Office data. Mock analyses')

#  Can examine what nationalities we see at different quarters, but may just be a better question to pose to team.# 