library(asylum)
library(dplyr)
library(ggplot2)
library(gridlayout)
library(plotly)
library(shiny)

appLaunch <- function() {
  shinyApp(ui, server)
}
