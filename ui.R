source("functions.R", local=FALSE)

# UI aspect of shiny.
ui <- fluidPage(
  
  titlePanel("Interactive Titanic Survival Analysis"),
  
  # Using a navigation menu for the following pages: 
    # Plots, Ticket Purchase, Statistical Analysis, About
  
  navbarPage("Navigation",
             
    tabPanel("Plots and Data",
      sidebarLayout(
        sidebarPanel(
          # Variables to plot.
          checkboxGroupInput(inputId="variable", label="Select Features", 
                             choices=vars_to_colnames, selected="Sex"),
          numericInput(inputId = "bins_for_age", label = "Width of an Age Category (check/uncheck Age Box)", value = 5),
          numericInput(inputId = "bins_for_fare", label = "Width of an Fare Category (check/uncheck Fare Box)", value = 10),
          radioButtons(inputId = "outlier", label = "Outliers", selected = "outliers_yes",
                       choices = c("Include" = "outliers_yes", "Remove" = "outliers_no"))
        ),
        # Main panel to display graphs.
        mainPanel(
          tabsetPanel(type = "tabs",
            tabPanel("Plot Survival vs Features", 
              radioButtons(inputId = "type_plots", label = "Statistics to View", choices = display_stats, selected = "nums"),
              plotOutput("variable_vs_survival")),
            
            tabPanel("Survival Tables", 
              tableOutput("survival_table")),
            
            tabPanel("Dimensionality Reduction Analysis",
              radioButtons(inputId = "type_reduction", selected = "principle_comps",
                           label = "Type of Reduction (selection from left side panel will be removed from analysis)", 
                           choices = c("Principle Comonents" = "principle_comps", "UMAP" = "umap")),
              plotOutput("dimensionality_reduction")),
            
            tabPanel("Help", 
              textOutput("help_plots"))
          )
        )
      )
    ),
    
    tabPanel("Purchase a Ticket",
       sidebarLayout(
         sidebarPanel(
           radioButtons(inputId = "user_pclass", label = "Choose Your Class", selected = "user_no_pclass",
                        choices = c("First" = "First", "Second" = "Second", "Third" = "Third", "Do Not Include" = "user_no_pclass")),
           numericInput(inputId = "user_fare", value = -1, label = "Price of Ticket 
                        (Third Class: $7-$70, Second Class: $11-$74, First Class: $26-$512)+"),
           radioButtons(inputId = "user_sex", label = "What is Your Sex?", selected = "user_no_sex", 
                        choices = c("Male" = "Male", "Female" = "Female", "Do Not Include" = "user_no_sex")),
           numericInput(inputId = "user_age", label = "What is Your Age", value = -1),
           numericInput(inputId = "user_sibSp", value = -1, 
                        label = "Number of siblings/spouses that will acompany you?"),
           numericInput(inputId = "user_parch", value = -1, 
                        label = "Number of parents/children that will acompany you?")
         ),
         # Main panel to display for Ticket purchase.
         mainPanel(
           tabsetPanel(type = "tabs",
              tabPanel("ML Ticket Purchase", 
                radioButtons(inputId = "ML_algorithm", label = "Choose Your ML Algorithm", selected = "reg_lin",
                             choices = c("Linear Regression" = "reg_lin", "Polynomial Regression" = "reg_pol")),
                textOutput("purchase_ticket")),
                       
              tabPanel("Help",
                textOutput("help_ML"))
            )
          )
       )
    ),
    
    tabPanel("Statistical Analysis",
      sidebarLayout(
        sidebarPanel(
          selectizeInput(inputId = "which_maths", label = "Statistics to Analyze", 
                         choices = c("hello" = "hello"))
        ),
        mainPanel(
          textOutput("statistical_analysis")
        )
      )
    ),
    
    tabPanel("About",
      sidebarLayout(
        sidebarPanel(
          selectizeInput(inputId = "about", label = "Statistics to Analyze", 
                         choices = c("hello" = "hello"))
          ),
        mainPanel(
         textOutput("about")
        )
      )
    )
  )
)