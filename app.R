options(stringsAsFactors=FALSE)

library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)
library(tidyr)

theme_set(theme_bw())

# Data set of all passengers on the titanic
train_set <- read.csv("datasets/train.csv")
#View(train_set)
#print(is.na(train_set))
#print(anyNA(train_set))
print(train_set$Embarked)

# Separate based upon the type of data. Remove irrelevant columns.
train_set_categorical <- train_set %>%
    select(Survived, Pclass, Sex, Ticket, Cabin, Embarked) #FIXME: examine cabin and ticket
train_set_numerical <- train_set %>%
    select(Survived, Age, SibSp, Parch, Fare)

# UI aspect of shiny.
ui <- fluidPage(

    titlePanel("All Aboard the Titanic!"),
    
    sidebarLayout(
        # Side bar to display user selection.
        sidebarPanel(
            # User selects which aspects to plot examine.
            selectizeInput(inputId="aspect", 
                           label="Plot Survival vs Aspect",
                           choices=colnames(select(train_set, -Survived, -PassengerId)),
                           selected=c("Pclass"),
                           multiple=FALSE),
            # User selects how to examine selected aspect.
            selectizeInput(inputId="which_plot", 
                           label="Associated Survival Plots w Aspect", 
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
            plotOutput("aspect_vs_survival")
        )
    )
)

# Server aspect of shiny.
server <- function(input, output) {
    
    output$aspect_vs_survival <- renderPlot({
        # The data user selects to graph.
        aspect <- input$aspect
        which_plot <- input$which_plot
        # Handle a graph for a categorical variable.
        if (aspect %in% colnames(train_set_categorical))
            # Select the associated aspect, and Survived columns.
            set <- select(train_set_categorical, aspect, Survived)
            # Determine statistics for each group.
            set <- set %>%
                # Drop NA and "" values.
                filter(complete.cases(.), !!as.name(aspect) != "") %>%
                # Group by the relevant aspect.
                group_by_at(aspect) %>%
                # Generate a summary table of survival information.
                summarize(total=n(), 
                          totalSurvived=sum(Survived==1), 
                          totalDead=total-totalSurvived,
                          survivalProbability=totalSurvived/total,
                          deathProbability=totalDead/total)
            View(set)
            # Output the graph.
            p <- ggplot(set, aes(x=!!as.name(aspect), y=!!as.name(which_plot))) +
              geom_bar(stat="identity") +
              labs(x=aspect, y=which_plot)
            print(p)
            
        if (aspect %in% colnames(train_set_numerical))
            set <- train_set_numerical
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
