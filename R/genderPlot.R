GenderPlotUI <- function(id) {
  tagList(
    tags$h4("Who is applying for asylum?"),
    
    # Let the user choose one or more genders
    selectizeInput(
      NS(id, "selectSex"),
      label = "Choose one or more genders",
      choices = sort(unique(asylum::applications$Sex)),
      multiple = TRUE
    ),
    
    # This is the plot to output
    plotlyOutput(
      NS(id, "plot"),
      height = "100%"
    )
  )
}

GenderPlotServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlotly({
      
      # Set up a blank variable to store the ggplot
      plt_gender <- NULL
      
      # Check if the user has selected any nations/nationalities
      if (length(input$selectSex) == 0){
        
        # User hasn't selected any nations/nationalities, so plot total applications over time
        plt_gender <- 
          asylum::applications |> 
          group_by(Date) |> 
          summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
          ungroup() |> 
          
          ggplot(aes(x = Date, y = Applications)) +
          geom_line() +
          scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
          labs(
            y = "Total applications",
            title = "Total number of asylum applications"
          )
        
      } else {
        # User has selected nations/nationalities, so plot only those
        plt_gender <- 
          asylum::applications |> 
          filter(Sex %in% input$selectSex) |> 
          group_by(Date, Sex) |> 
          summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
          ungroup() |> 
          
          ggplot(aes(x = Date, y = Applications, group = Sex)) +
          geom_line(aes(colour = Sex)) +
          scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
          labs(
            title = "Number of asylum applications for selected gender"
          )
      }
      
      # Convert to an interactive plot
      ggplotly(plt_gender)
    })
  })
}
