grantRatesPlotUI <- function(id) {
  tagList(
    tags$h4("What is the asylum grant rate at initial decision and appeal?"),
    
    # Let the user choose one or more nations/nationalities
    selectizeInput(
      NS(id, "selectNation"),
      label = "Choose one or more nations",
      choices = sort(unique(grant_rates_initial_quarterly$Nationality)),
      multiple = TRUE
    ),
    
    # This is the plot to output
    plotlyOutput(
      NS(id, "plot"),
      height = "100%"
    )
  )
}

grantRatesPlotServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlotly({
      
      # Set up a blank variable to store the ggplot
      plt_rates <- NULL
      
      # Check if the user has selected any nations/nationalities
      if (length(input$selectNation) == 0){
        
        # User hasn't selected any nations/nationalities, so plot total grant rates over time
        plt_rates <- 
          grant_rates_initial_quarterly |> 
          group_by(Date) |> 
          summarise(
            Grant = sum(Grant, na.rm = TRUE),
            Refused = sum(Refused, na.rm = TRUE)
          ) |> 
          ungroup() |>
          mutate(`Initial grant rate` = Grant / (Grant + Refused)) |> 
          
          ggplot(aes(x = Date, y = `Initial grant rate`)) +
          geom_line() +
          scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
          labs(
            title = "Initial grant rate for all asylum applications"
          )
        
      } else {
        # User has selected nations/nationalities, so plot only those
        plt_rates <- 
          grant_rates_initial_quarterly |> 
          filter(Nationality %in% input$selectNation) |> 
          
          ggplot(aes(x = Date, y = `Initial grant rate`, group = Nationality)) +
          geom_line(aes(colour = Nationality)) +
          scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
          labs(
            title = "Initial grant rates for selected countries"
          )
      }
      
      # Convert to an interactive plot
      ggplotly(plt_rates)
    })
  })
}
