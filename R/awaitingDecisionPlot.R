awaitingDecisionPlotUI <- function(id) {
  tagList(
    tags$h4("How long is it taking for decisions to be made?"),
    
    # Let the user choose one or more nations/nationalities
    selectizeInput(
      NS(id, "selectNation"),
      label = "Choose a nation",
      choices = c("Grand total", sort(unique(asylum::applications$Nationality))),
      multiple = FALSE
    ),
    
    # This is the plot to output
    plotlyOutput(
      NS(id, "plot"),
      height = "100%"
    )
  )
}

awaitingDecisionPlotServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlotly({
      
      # Set up a blank variable to store the ggplot
      plt <- NULL
      
      awaiting_decision <- 
        asylum::awaiting_decision |> 
        mutate(Duration = case_when(
          Duration == "N/A - Further review" ~ "Pending further review",
          Duration == "6 months or less" ~ "Pending initial decision (6 months or less)",
          Duration == "More than 6 months" ~ "Pending initial decision (more than 6 months)"
        ))
      
      # Check if the user has selected any nations/nationalities
      if (input$selectNation == "Grand total") {
        
        total_awaiting <- 
          awaiting_decision |> 
          filter(Date == max(Date)) |> 
          summarise(Total = sum(Applications)) |> 
          pull(Total)
        
        # User hasn't selected any nations/nationalities, so plot totals over time
        plt <- 
          awaiting_decision |>  
          group_by(Date, Duration) |> 
          summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
          ungroup() |> 
          
          ggplot(aes(x = Date, y = Applications, group = Duration)) +
          geom_line(aes(colour = Duration)) +
          scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
          labs(
            y = "Number of applicants awaiting an initial decision ",
            title = str_glue("{scales::comma(total_awaiting)} asylum applicants are currently awaiting an initial decision")
          )
        
      } else {
        # User has selected nations/nationalities, so plot only those
        total_awaiting_in_nation <- 
          awaiting_decision |> 
          filter(Date == max(Date), Nationality %in% input$selectNation) |> 
          summarise(Total = sum(Applications)) |> 
          pull(Total)
        
        plt <- 
          awaiting_decision |> 
          filter(Nationality %in% input$selectNation) |> 
          group_by(Date, Duration) |> 
          summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
          ungroup() |> 
          
          ggplot(aes(x = Date, y = Applications, group = Duration)) +
          geom_line(aes(colour = Duration)) +
          scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
          labs(
            y = "Number of applicants awaiting an initial decision ",
            title = str_glue("{scales::comma(total_awaiting_in_nation)} asylum applicants from {input$selectNation} are currently awaiting an initial decision")
          )
      }
      
      # Convert to an interactive plot
      ggplotly(plt)
    })
  })
}
