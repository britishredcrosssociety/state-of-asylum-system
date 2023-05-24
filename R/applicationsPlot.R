applicationsPlotUI <- function(id) {
  tagList(
    tags$h4("Who is applying for asylum?"),
    
    # Using fluidRow to wrap the selectizeInput elements
    fluidRow(
      # Using column function to define each selectizeInput's space
      column(4,
             selectizeInput(
               NS(id, "selectNation"),
               label = "Choose one or more nations",
               choices = sort(unique(asylum::applications$Nationality)),
               multiple = TRUE
             )
      ),
      
      column(4,
             selectizeInput(
               NS(id, "selectAge"),
               label = "Choose one or more age groups",
               # choices = unique(asylum::applications$Age),
               choices = c("Under 18", "18-29", "30-49", "50-69", "70+", "Unknown"),
               multiple = TRUE
             )
      ),
      
      column(4,
             selectizeInput(
               NS(id, "selectSex"),
               label = "Choose one or more sexes",
               # choices = sort(unique(asylum::applications$Sex)),
               choices = c("Female", "Male", "Unknown Sex"),
               multiple = TRUE
             )
      )
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
      
      # Which 
      tmp_applications <- 
        asylum::applications |> 
        select(Date, Nationality, Age, Sex, Applications) |> 
        group_by(Date)
      
      if (length(input$selectNation) > 0) {
        tmp_applications <- 
          tmp_applications |> 
          filter(Nationality %in% input$selectNation) |> 
          group_by(Nationality, .add = TRUE)
      }
      
      if (length(input$selectAge) > 0) {
        tmp_applications <- 
          tmp_applications |> 
          filter(Age %in% input$selectAge) |> 
          group_by(Age, .add = TRUE)
      }
      
      if (length(input$selectSex) > 0) {
        tmp_applications <- 
          tmp_applications |> 
          filter(Sex %in% input$selectSex) |> 
          group_by(Sex, .add = TRUE)
      }
      
      tmp_applications <- 
        tmp_applications |> 
        summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
        ungroup()
      
      plt_applications <- 
        tmp_applications |> 
        ggplot(aes(x = Date, y = Applications)) +
        
        { if (length(input$selectNation) > 0) geom_line(aes(group = Nationality, colour = Nationality)) else geom_line() } +
        
        { if (length(input$selectAge) > 0 & length(input$selectSex) == 0) facet_grid(Age ~ .) } +
        { if (length(input$selectAge) == 0 & length(input$selectSex) > 0) facet_grid(. ~ Sex) } +
        { if (length(input$selectAge) > 0 & length(input$selectSex) > 0) facet_grid(Age ~ Sex) } +
        
        scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
        labs(
          title = "Number of asylum applications for selected countries"
        )
      
      # # Check if the user has selected any nations/nationalities
      # if (length(input$selectNation) == 0) {
      #   
      #   # User hasn't selected any nations/nationalities, so plot total applications over time
      #   plt_applications <- 
      #     asylum::applications |> 
      #     group_by(Date) |> 
      #     summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
      #     ungroup() |> 
      #     
      #     ggplot(aes(x = Date, y = Applications)) +
      #     geom_line() +
      #     scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
      #     labs(
      #       y = "Total applications",
      #       title = "Total number of asylum applications"
      #     )
      #   
      # } else {
      #   # User has selected nations/nationalities, so plot only those
      #   plt_applications <- 
      #     asylum::applications |> 
      #     filter(Nationality %in% input$selectNation) |> 
      #     group_by(Date, Nationality) |> 
      #     summarise(Applications = sum(Applications, na.rm = TRUE)) |> 
      #     ungroup() |> 
      #     
      #     ggplot(aes(x = Date, y = Applications, group = Nationality)) +
      #     geom_line(aes(colour = Nationality)) +
      #     scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
      #     labs(
      #       title = "Number of asylum applications for selected countries"
      #     )
      # }
      
      # Convert to an interactive plot
      ggplotly(plt_applications)
    })
  })
}
