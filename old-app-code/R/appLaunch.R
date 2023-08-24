library(asylum)
library(dplyr)
library(DT)
library(ggplot2)
library(gridlayout)
library(plotly)
library(shiny)
library(stringr)

appLaunch <- function() {
  shinyApp(ui, server)
}
