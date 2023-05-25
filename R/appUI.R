ui <- function(request) {
  fluidPage(
    includeCSS("inst/www/styles.css"),
    tags$head(tags$title("State of the Asylum System - Prototype")),
    
    # ---- Header ----
    fluidRow(
      div(
        style="vertical-align:middle;padding-top:10px",
        column(2, img(src = "www/brc-logo.svg")),
        column(10, h3("State of the Asylum System - Prototype"))
      )
    ),
    
    # ---- Intro ----
    fluidRow(
      column(2),
      column(
        8,
        tags$p(
          tags$span(class = "phase-banner", "ALPHA"),
          "This is a prototype for exploring potential content for State of the System. There may be bugs, and these graphs need to be given BRC branding. New graphs will be added soon."
        )
      ),
      column(2)
    ),
    
    # ---- Who is applying for asylum in the UK and who has been granted protection? ----
    fluidRow(
      column(1),
      column(
        10,
        tags$h3(tags$b("Who is applying for asylum in the UK and who has been granted protection?")),
        
        fluidRow(
          applicationsPlotUI("applicationsPlot")
        ),
        
        fluidRow(
          grantRatesPlotUI("grantRatesPlot")
        ),
        
        fluidRow(
          awaitingDecisionPlotUI("awaitingDecisionPlot")
        ),
        
        fluidRow(
          inadmissibilityClaimsTableUI("inadmissibilityClaimsTable")
        ),
        
        # Each graph should have its own fluidRow()
        fluidRow(
          IrregularMigrationPlotUI("IrregularMigrationPlot")
          # The new graph would go here
        )
      ),
      column(1)
    )
  )
}
