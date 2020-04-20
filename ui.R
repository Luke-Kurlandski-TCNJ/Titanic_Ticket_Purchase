source("functions.R", local=FALSE)

# UI aspect of shiny.
ui <- fluidPage(
  
  titlePanel("All Aboard the Titanic!"),
  
  sidebarLayout(
    # Side bar to display user selection.
    sidebarPanel(
      # User selects which variables to plot examine.
      selectizeInput(inputId="variable", 
                     label="Plot Survival vs Variable",
                     choices=c("Sex" = "Sex",
                               "Age" = "AgeCat",
                               "Number of Siblings/Spouses" = "SibSp",
                               "Number of Parents/Children" = "Parch",
                               "Lone Traveller" = "Alone",
                               "Ticket" = "Ticket", #FIXME: implement or remove
                               "Fare" = "FareCat",
                               "Cabin" = "Cabin", #FIXME: implement or remove
                               "Port Embarked From" = "Embarked"),
                     selected=c("Pclass"),
                     multiple=FALSE),
      # User selects how to examine selected variable.
      selectizeInput(inputId="prob_vs_total", 
                     label="Number/Probability of Survival on Upper Chart", 
                     choices=c("Number" = "totalSurvived", "Probability" = "survivalProbability"),
                     selected=c("Totals"),
                     multiple=FALSE),
      # User selects whether or not to view other Chart
      selectizeInput(inputId="lower_chart", 
                     label="Display Pure Probabilities", 
                     choices=c("Below" = "below", "Right" = "right", "Remove Chart" = "remove"),
                     selected=c("Yes"),
                     multiple=FALSE)
    ),
    
    # Main panel to display graphs.
    mainPanel(
      plotOutput("variable_vs_survival")
    )
  )
)