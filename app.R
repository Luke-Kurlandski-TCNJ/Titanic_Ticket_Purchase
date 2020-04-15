options(stringsAsFactors=FALSE)

library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)

theme_set(theme_bw())

# Data set of all passengers on the titanic
train_set <- read.csv("datasets/train.csv")

# Separate based upon the type of data.
train_set_categorical <- train_set %>%
    select(Survived, Pclass, Name, Sex, Ticket, Cabin, Embarked)
train_set_numerical <- train_set %>%
    select(Survived, Age, SibSp, Parch, Fare)

# UI aspect of shiny.
ui <- fluidPage(

    titlePanel("All Aboard the Titanic!"),

    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId="aspect", label="View Plots",
                           choices=unique(c(colnames(train_set_categorical), colnames(train_set_numerical))),
                           selected=c("Pclass"),
                           multiple=FALSE)#,
            
            #selectizeInput(inputId="x", label="y", choices=c(1,2,3), multiple=TRUE)
        ),

        mainPanel(
            plotOutput("aspect_vs_survival")
        )
    )
)

# Server aspect of shiny.
server <- function(input, output) {
    
    output$aspect_vs_survival <- renderPlot({
        # The aspect user selects to graph.
        aspect <- input$aspect
        # Handle a graph for a categorical variable.
        if (aspect %in% colnames(train_set_categorical))
            # Select the associated aspect, and Survived columns.
            set <- select(train_set_categorical, aspect, Survived)
            # Determine statistics for each group.
            set <- set %>%
                group_by_at(aspect) %>%
                summarize(total=n(), 
                          totalSurvived=sum(Survived==1), 
                          totalDead=total-totalSurvived,
                          survivalProbability=totalSurvived/total)
            # Output the graph.
            p1 <- ggplot(set, aes(x=!!as.name(aspect), y=total)) +
              geom_bar(stat="identity") +
              labs(x=aspect, y="Total Number of Passengers")
            p2 <- ggplot(set, aes(x=!!as.name(aspect), y=totalSurvived)) +
              geom_bar(stat="identity") +
              labs(x=aspect, y="Survived Passengers")
            p3 <- ggplot(set, aes(x=!!as.name(aspect), y=survivalProbability)) +
              geom_bar(stat="identity") +
              labs(x=aspect, y="Survival Probability")
            print(p1)
            print(p2)
            print(p3)
            
        if (aspect %in% colnames(train_set_numerical))
            set <- train_set_numerical
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
