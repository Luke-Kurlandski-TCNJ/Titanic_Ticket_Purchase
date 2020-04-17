options(stringsAsFactors=FALSE)

library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)
library(gridExtra)
library(tidyr)
library(tibble)
#library(naniar)

theme_set(theme_bw())

# Apply an age category to an age.
determine_age_category <- function(age) {
    if (is.na(age) || age=="")
        return(NA)
    else
        return(5 * (age %/% 5))
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

# Determine statistics for each group of the variable.
calculate_survival_stats <- function(variable) {
  
    survival_stats <- train_set %>%
        select(variable, Survived) %>%
        filter(complete.cases(.), !!as.name(variable) != "") %>%
        group_by_at(variable) %>%
        summarize(total = n(), totalSurvived = sum(Survived==1), 
                  totalDead = total-totalSurvived,
                  survivalProbability = round(totalSurvived/total, 2),
                  deathProbability = round(totalDead/total), 2)
    
    return(survival_stats)
}

# Create the chart for the number of passengers.
passenger_number_chart <- function(variable, prob_vs_total, survival_stats) {
  
    colors <- c("Total" = "red1", "Survived" = "orange1")
    p <- ggplot(survival_stats) +
        geom_bar(aes(x=!!as.name(variable), y=total, fill="Total"), 
                 stat="identity", position="dodge")    + 
        geom_bar(aes(x=!!as.name(variable), y=totalSurvived, fill="Survived"), 
                 stat="identity", position="dodge")   + 
        geom_text(aes(x=!!as.name(variable), y=total, label=total), 
                  position=position_dodge(width=0.9), vjust=-0.25) +
        geom_text(aes(x=!!as.name(variable), y=totalSurvived, 
                      label=!!as.name(prob_vs_total)), 
                  position=position_dodge(width=0.9), vjust=-0.25) +
        labs(x=display_vars[match(variable, vars_to_colnames)], y="Number of Passengers", color = "Legend")  +
        scale_fill_manual(name="Legend", values = colors)
    
    return(p)
}

passenger_probability_chart <- function(variable, prob_vs_total, survival_stats) {
  
    colors <- c("Survival Probability" = "green1", "Death Probability" = "yellow1")
    p <- ggplot(survival_stats) + 
        geom_bar(aes(x=!!as.name(variable), y=survivalProbability, fill="Survival Probability"), 
                  stat="identity", position="dodge") + 
        geom_text(aes(x=!!as.name(variable), y=survivalProbability, label=survivalProbability), 
                  position=position_dodge(width=0.9), vjust=-0.25) +
        labs(x=display_vars[match(variable, vars_to_colnames)], y="Number of Passengers", color = "Legend")  +
        scale_fill_manual(name="Legend", values = colors)
    
    return(p)
}

# Output a chart when a categorical variable is selected.
categorical_charts <- function(variable, prob_vs_total) {
    
    survival_stats <- calculate_survival_stats(variable)
    p1 <- passenger_number_chart(variable, prob_vs_total, survival_stats)
    p2 <- passenger_probability_chart(variable, prob_vs_total, survival_stats)
    
    return(list(p1, p2))
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
vars_to_colnames <- c("Sex"="Sex",  
                      "Number of Siblings/Spouses" = "SibSp", 
                      "Number of Parents/Children" = "Parch",
                      "Ticket" = "Ticket", #FIXME: implement or remove
                      "Cabin" = "Cabin", #FIXME: implement or remove
                      "Port Embarked From" = "Embarked", "Lone Traveller" = "Alone", "Age" = "AgeCat", "Fare" = "FareCat")
vars_to_colnames

display_vars <- c("Sex", "Number of Siblings/Spouses", "Number of Parents/Children",
                  "Ticket", "Cabin", "Port Embarked From", "Lone Traveller", "Age", "Fare")
data_vars <- colnames(select(train_set, -PassengerId, -Survived, -Age, -Name, -Fare))

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

# Server variable of shiny.
server <- function(input, output) {
    
    output$variable_vs_survival <- renderPlot({

        # Handle a graph for a categorical variable.
        plots <- categorical_charts(input$variable, input$prob_vs_total)
        #plots2 <- categorical_chart(input$variable, input$prob_vs_total)
        #print(plots[2])
        str(plots)
        if (input$lower_chart == "remove")
            plots[1]
        else if (input$lower_chart == "right")
            grid.arrange(grobs=plots, ncol=2)
        else if (input$lower_chart == "below")
            grid.arrange(grobs=plots, nrow=2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
