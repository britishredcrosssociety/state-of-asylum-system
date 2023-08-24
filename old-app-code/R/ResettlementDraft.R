ResettlementPlotUI <- function(id) {
  tagList(
    tags$h4("Who is being resettled?"),
    
    # Using fluidRow to wrap the selectizeInput elements
    fluidRow(
      # Using column function to define each selectizeInput's space
      column(4,
             selectizeInput(
               NS(id, "selectNation"),
               label = "Choose one or more nations",
               choices = sort(unique(asylum::decisions_resettlement$Nationality)),
               multiple = TRUE
             )
      ),
      
      column(4,
             selectizeInput(
               NS(id, "selectAge"),
               label = "Choose one or more age groups",
               #choices = unique(asylum::decisions_resettlement$Age),
               choices = c("Under 18", "18-29", "30-49", "50-69", "70+", "Unknown"),
               multiple = TRUE
             )
      ),
      
      column(4,
             selectizeInput(
               NS(id, "selectSex"),
               label = "Choose one or more sexes",
               #choices = sort(unique(asylum::decisions_resettlement$Sex)),
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

ResettlementPlotServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlotly({
      
      # Set up a blank variable to store the ggplot
      plt_resettlement <- NULL
      
      # Which 
      tmp_resettlement <- 
        asylum::decisions_resettlement |> 
        select(Date, Nationality, Age, Sex, Decisions) |> 
        group_by(Date)
      
      if (length(input$selectNation) > 0) {
        tmp_resettlement <- 
          tmp_resettlement |> 
          filter(Nationality %in% input$selectNation) |> 
          group_by(Nationality, .add = TRUE)
      }
      
      if (length(input$selectAge) > 0) {
        tmp_resettlement <- 
          tmp_resettlement |> 
          filter(Age %in% input$selectAge) |> 
          group_by(Age, .add = TRUE)
      }
      
      if (length(input$selectSex) > 0) {
        tmp_resettlement <- 
          tmp_resettlement |> 
          filter(Sex %in% input$selectSex) |> 
          group_by(Sex, .add = TRUE)
      }
      
      tmp_resettlement <- 
        tmp_resettlement |> 
        summarise(Decisions = sum(Decisions, na.rm = TRUE)) |> 
        ungroup()
      
      plt_resettlement <- 
        tmp_resettlement |> 
        ggplot(aes(fill = `Case outcome group`,y = Decisions, x = Year)) +
        
        { if (length(input$selectNation) > 0) geom_bar(aes(position="stack", stat="identity", group = Nationality, colour = Nationality)) else geom_bar() } +
        
        { if (length(input$selectAge) > 0 & length(input$selectSex) == 0) facet_grid(Age ~ .) } +
        { if (length(input$selectAge) == 0 & length(input$selectSex) > 0) facet_grid(. ~ Sex) } +
        { if (length(input$selectAge) > 0 & length(input$selectSex) > 0) facet_grid(Age ~ Sex) } +
        
        scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
        labs(
          title = "Number of asylum applications"
        )
      
      ggplotly(plt_resettlement)
    })
  })
}