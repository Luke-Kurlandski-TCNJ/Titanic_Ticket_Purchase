options(stringsAsFactors=FALSE)

library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)
library(tidyr)
library(tibble)
#library(naniar)

theme_set(theme_bw())

# Apply an age category to an age.
determine_age_category <- function(age) {
    if (is.na(age) || age=="")
        return(NA)
    else
        return(age %/% 5)
}

# Apply a fare category to a fare.
determine_fare_category <- function(fare) {
  if (is.na(fare) || fare=="")
    return(NA)
  else
    return(fare %/% 10)
}

# Determine whether a passeneger embarked alone or not.
determine_alone <- function(parents_children, siblings_spouses) {
    if (parents_children == 0 && siblings_spouses == 0)
        return(TRUE)
    else 
        return(FALSE)
}

# Data set of all passengers on the titanic.
train_set <- read.csv("datasets/train.csv") %>% 
    #replace_with_na_all(condition = ~.x == "")
    mutate(Alone = mapply(determine_alone, SibSp, Parch)) %>%
    mutate(AgeCat = mapply(determine_age_category, Age)) %>%
    mutate(FareCat = mapply(determine_fare_category, Fare))
View(train_set)
# Separate based upon the type of data.
variables_categorical <- c("Pclass", "Sex", "Ticket", 
                           "Cabin", "Embarked", "Cabin",
                           "Ticket", "AgeCat", "Alone",
                           "SibSp", "Parch", "FareCat")
vars_to_colnames <- c("Sex"="Sex", "Age" = "AgeCat", 
                     "Number of Siblings/Spouses" = "SibSp", 
                     "Number of Parents/Children" = "Parch",
                     "Lone Traveller" = "Alone",
                     "Ticket" = "Ticket", #FIXME: implement or remove
                     "Fare" = "FareCat", "Cabin" = "Cabin", #FIXME: implement or remove
                     "Port Embarked From" = "Embarked")
vars_to_colnames

display_vars <- c("Sex", "Number of Siblings/Spouses", "Number of Parents/Children",
                  "Ticket", "Cabin", "Port Embarked From", "Lone Traveller", "Age", "Fare")
data_vars <- colnames(select(train_set, -PassengerId, -Survived, -Age, -Name, -Fare))


# Output a chart when a categorical variable is selected.
categorical_chart <- function(variable, which_plot) {
    # Determine statistics for each group of the variable.
    survival_stats <- train_set %>%
        select(variable, Survived) %>%
        filter(complete.cases(.), !!as.name(variable) != "") %>%
        group_by_at(variable) %>%
        summarize(total=n(), 
                  totalSurvived=sum(Survived==1), 
                  totalDead=total-totalSurvived,
                  survivalProbability=totalSurvived/total,
                  deathProbability=totalDead/total)
    View(survival_stats)
    # Colors for the graph.
    colors <- c("Total" = "red1", "Survived" = "orange1")
    p <- ggplot(survival_stats) +
        geom_bar(aes(x=!!as.name(variable), y=total, fill="Total"), 
                 stat="identity", position="dodge")    + 
        geom_bar(aes(x=!!as.name(variable), y=totalSurvived, fill="Survived"), 
                stat="identity", position="dodge")   + 
        geom_text(aes(x=!!as.name(variable), y=total, label=total), 
                  position=position_dodge(width=0.9), vjust=-0.25) +
        geom_text(aes(x=!!as.name(variable), y=totalSurvived, label=totalSurvived), 
                  position=position_dodge(width=0.9), vjust=-0.25) +
        labs(x=display_vars[match(variable, vars_to_colnames)], y="Number of Passengers", color = "Legend")  +
        scale_fill_manual(name="Legend", values = colors)
    print(p)
}

# UI aspect of shiny.
ui <- fluidPage(

    titlePanel("All Aboard the Titanic!"),
    
    sidebarLayout(
        # Side bar to display user selection.
        sidebarPanel(
            # User selects which variables to plot examine.
            selectizeInput(inputId="variable", 
                           label="Plot Survival vs Variable",
                           choices=c("Sex"="Sex",
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
            selectizeInput(inputId="which_plot", 
                           label="Associated Survival Plots w Variable", 
                           choices=c("Total" = "total", 
                                     "Total Survived" = "totalSurvived", 
                                     "Total Dead" = "totalDead",
                                     "Survival Probability" = "survivalProbability", 
                                     "Death Probability" = "deathProbability"),
                           selected=c("Total Survived", "Survival Probability"),
                           multiple=FALSE)
        ),
        
        # Main panel to display graphs.
        mainPanel(
            plotOutput("variable_vs_survival")
        )
    )
)

# Server variable of shiny.
server <- function(input, output) {
    
    output$variable_vs_survival <- renderPlot({

        # Handle a graph for a categorical variable.
        if (input$variable %in% variables_categorical)
            categorical_chart(input$variable, input$which_plot)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
