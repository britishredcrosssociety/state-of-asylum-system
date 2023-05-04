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
          "This is a prototype for exploring potential content for State of the System - new graphs will be added soon."
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
        
        applicationsPlotUI("applicationsPlot")
        # New graphs for this section would go here
      ),
      column(1)
    )
  )
}
