source("functions.R", local=FALSE)

# Server variable of shiny.
server <- function(input, output) {
  
  # Handle a graph for a categorical variable.
  output$variable_vs_survival <- renderPlot({
      
      plots <- categorical_charts(input$variable, input$prob_vs_total)
      
      if (input$lower_chart == "remove") 
          plots[1]
      else if (input$lower_chart == "right") 
          grid.arrange(grobs=plots, ncol=2)
      else if (input$lower_chart == "below") 
          grid.arrange(grobs=plots, nrow=2)
  })
}