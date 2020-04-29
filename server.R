source("functions.R", local=FALSE)

# Server variable of shiny.
server <- function(input, output) {
  
  # Reactive event for variable selection.
  survival_stats <- eventReactive(input$variable, {
    train_set %>%
      select(input$variable, Survived) %>%
      filter(complete.cases(.), !!as.name(input$variable) != "") %>%
      group_by_at(input$variable) %>%
      summarize(total = n(), totalSurvived = sum(Survived == 1), 
                totalDead = total-totalSurvived,
                survivalProbability = round(totalSurvived/total, 2),
                deathProbability = round(totalDead/total), 2) %>%
      select(-"2") # IDK why, but a column named "2" is added automatically...
  })
  
  # Reactive event for age width selection.
  observeEvent(input$bins_for_age, {
    train_set <<- mutate(train_set, AgeCat = 
                          mapply(determine_age_category, age = Age, bin = input$bins_for_age))
  })
  
  # Reactive event for fare width selection.
  observeEvent(input$bins_for_fare, {
    train_set <<- mutate(train_set, FareCat = 
                          mapply(determine_fare_category, fare = Fare, bin = input$bins_for_fare))
  })
  
  # Handle a graph for a categorical variable.
  output$variable_vs_survival <- renderPlot({
    
    variable <- input$variable
    type_plots <- input$type_plots
    
    # Reactive function to compute survival statistics and allow user to modify paramters.
    survival_stats <- survival_stats()
    
    # Output the plots
    if (type_plots == "nums" || type_plots == "nums_w_props") 
      p <- produce_chart_numbers(input$variable, input$type_plots, survival_stats) 
    if (type_plots == "props" || type_plots == "props_w_nums") 
      p <- produce_chart_probabilities(input$variable, input$type_plots, survival_stats) 
    p
  })
  
  # Output the survival_stats table as part of the graphs and plots interface.
  output$survival_table <- renderTable({
    survival_stats()
  })
  
  output$dimensionality_reduction_plot <- renderPlot({
    if (input$type_reduction == "principle_comps")
      p <- plot_PCA(input$variable)
    if (input$type_reduction == "umap")
      p <- plot_UMAP(input$variable)
    p
  })
  
  output$dimensionality_reduction_table <- renderTable({
    if (input$type_reduction == "principle_comps")
      t <- dataframe_PCA(input$variable)
    if (input$type_reduction == "umap")
      t <- dataframe_UMAP(input$variable)
    t
  })
  
  # Help tab for plots and interface.
  output$help_plots <- renderText({
    print("Instructions Coming Soon!")
  })
  
  output$purchase_ticket <- renderText({
    purchase_ticket(input$pclass, input$user_sex, input$user_age, input$user_fare, 
                    input$user_parch, input$user_sibSp, input$ML_algorithm)
  })
  
  # Help tab for ML ticket purchase.
  output$help_ML <- renderText({
    print("Instructions Coming Soon!")
  })
  
  output$statistical_analysis <- renderText({
    print(input$which_maths)
  })
  
  output$about <- renderText({
    print(input$about)
  })
}