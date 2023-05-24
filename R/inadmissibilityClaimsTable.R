inadmissibilityClaimsTableUI <- function(id) {
  tagList(
    tags$h4("How many claims have been deemed inadmissible?"),
    
    DTOutput(
      NS(id, "table"),
      height = "100%"
    )
  )
}

inadmissibilityClaimsTableServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderDT({
      inadmissibility_cases_considered |> select(-Date)
    })
  })
}
