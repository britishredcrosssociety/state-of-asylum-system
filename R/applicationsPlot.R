applicationsPlotUI <- function(id) {
  tagList(
    tags$h4("Who is applying for asylum?"),
    
    # Let the user choose one or more nations/nationalities
    selectizeInput(
      NS(id, "selectNation"),
      label = "Choose one or more nations",
      choices = sort(unique(asylum::applications$Nationality)),
      multiple = TRUE
    ),
    
    # This is the plot to output
    plotlyOutput(
      NS(id, "plot"),
      height = "100%"
    )
  )
}

applicationsPlotServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlotly({
      
      # Set up a blank variable to store the ggplot
      plt_applications <- NULL
      
      # Check if the user has selected any nations/nationalities
      if (length(input$selectNation) == 0){
        
        # User hasn't selected any nations/nationalities, so plot total applications over time
        plt_applications <- 
          asylum::applications |> 
          group_by(Date) |> 
          summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
          ungroup() |> 
          
          ggplot(aes(x = Date, y = Applications)) +
          geom_line()
        
      } else {
        # User has selected nations/nationalities, so plot only those
        plt_applications <- 
          asylum::applications |> 
          filter(Nationality %in% input$selectNation) |> 
          group_by(Date, Nationality) |> 
          summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
          ungroup() |> 
          
          ggplot(aes(x = Date, y = Applications, group = Nationality)) +
          geom_line(aes(colour = Nationality))
      }
      
      # Convert to an interactive plot
      ggplotly(plt_applications)
    })
  })
}
