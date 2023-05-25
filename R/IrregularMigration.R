IrregularMigrationPlotUI <- function(id) {
  tagList(
    tags$h4("Irregular Migration to the UK"),
    
    # Using fluidRow to wrap the selectizeInput elements
    fluidRow(
      # Using column function to define each selectizeInput's space
      column(4,
             selectizeInput(
               NS(id, "selectMethod"),
               label = "Choose one or more methods of entry",
               choices = sort(unique(asylum::irregular_migration$`Method of entry`)),
               multiple = TRUE
             )
      ),
      
      column(4,
             selectizeInput(
               NS(id, "selectAgeGroup"),
               label = "Choose one or more age groups",
               #choices = unique(asylum::irregular_migration$`Age Group`),
               choices = c("Under 18", "18-29", "30-49", "50-69", "70+", "Unknown"),
               multiple = TRUE
             )
      ),
      
      column(4,
             selectizeInput(
               NS(id, "selectSex"),
               label = "Choose one or more sexes",
               #choices = sort(unique(asylum::irregular_migration$Sex)),
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

IrregularMigrrationPlotServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlotly({
      
      # Set up a blank variable to store the ggplot
      plt_irregularmigration <- NULL
      
      # Which 
      tmp_irregularmigration <- 
        asylum::irregular_migration |> 
        select(Date,`Age Group`, Sex, `Method of entry`, `Number of detections`) |> 
        group_by(Date)
      
      if (length(input$selectMethod) > 0) {
        tmp_irregularmigration <- 
          tmp_irregularmigration |> 
          filter(`Method of entry` %in% input$selectMethod) |> 
          group_by(`Method of entry`, .add = TRUE)
      }
      
      if (length(input$selectAgeGroup) > 0) {
        tmp_irregularmigration <- 
          tmp_irregularmigration |> 
          filter(`Age Group` %in% input$selectAgeGroup) |> 
          group_by(`Age Group`, .add = TRUE)
      }
      
      if (length(input$selectSex) > 0) {
        tmp_irregularmigration <- 
          tmp_irregularmigration |> 
          filter(Sex %in% input$selectSex) |> 
          group_by(Sex, .add = TRUE)
      }
      
      tmp_irregularmigration <- 
        tmp_irregularmigration |> 
        summarise(Detections = sum(`Number of detections`, na.rm = TRUE)) |> 
        ungroup()
      
      plt_irregularmigration <- 
        tmp_irregularmigration |> 
        ggplot(aes(x = Date, y = Detections)) +
        
        { if (length(input$selectMethod) > 0) geom_line(aes(group = `Method of entry`, colour = `Method of entry`)) else geom_line() } +
        
        { if (length(input$selectAgeGroup) > 0 & length(input$selectSex) == 0) facet_grid(AgeGroup ~ .) } +
        { if (length(input$selectAgeGroup) == 0 & length(input$selectSex) > 0) facet_grid(. ~ Sex) } +
        { if (length(input$selectAgeGroup) > 0 & length(input$selectSex) > 0) facet_grid(AgeGroup ~ Sex) } +
        
        scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
        labs(
          title = "Irregular Migration to the UK"
        )
      
      # Convert to an interactive plot
      ggplotly(plt_irregularmigration)
    })
  })
}
