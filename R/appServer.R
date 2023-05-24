server <- function(input, output, session) {
  # Call module server functions
  applicationsPlotServer("applicationsPlot")
  grantRatesPlotServer("grantRatesPlot")
  awaitingDecisionPlotServer("awaitingDecisionPlot")
  
}
