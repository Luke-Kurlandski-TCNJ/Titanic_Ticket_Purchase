# Custom configurations based upon preference.
options(stringsAsFactors=FALSE)
#theme_set(theme_bw())

# Libraries in use throughout project.
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)
library(tidyr)
library(tibble)
library(dslabs)
library(caret)
library(gbm)

# Apply an age category to an age.
determine_age_category <- function(age, bin = 5) {
  #bin <- 5; #FIXME: comment out
  if (is.na(age) || age=="")
    return(NA)
  else
    return(bin * (age %/% bin))
}

# Apply a fare category to a fare.
determine_fare_category <- function(fare, bin = 10) {
  #bin <- 10; #FIXME: comment out
  if (is.na(fare) || fare=="")
    return(NA)
  else
    return(bin * (fare %/% bin))
}

# Determine whether a passeneger embarked alone or not.
determine_alone <- function(parents_children, siblings_spouses) {
  if (parents_children == 0 && siblings_spouses == 0)
    return("Alone")
  else 
    return("Not Alone")
}

# Data set of all passengers on the titanic.
train_set <- read.csv("train.csv") %>% 
  mutate(Alone = mapply(determine_alone, SibSp, Parch)) %>%
  mutate(AgeCat = mapply(determine_age_category, age = Age, bin = 5)) %>%
  mutate(FareCat = mapply(determine_fare_category, fare = Fare, bin = 10)) 
  #mutate(AgeCat = mapply(determine_age_category, age = Age)) %>%
  #mutate(FareCat = mapply(determine_fare_category, fare = Fare))
#FIXME: replace all empty cells with NA, and fix mutate()

# Useful vector mapping the options to display to their column names.
vars_to_colnames <- c("Sex"="Sex", "Number of Siblings/Spouses" = "SibSp", 
                      "Number of Parents/Children" = "Parch", "Class" = "Pclass",
                      #"Ticket" = "Ticket", "Cabin" = "Cabin", 
                      "Port Embarked From" = "Embarked", 
                      "Lone Traveller" = "Alone", "Age" = "AgeCat", "Fare" = "FareCat")

# Similarily, map the options of display statistics to variable names.
display_stats <- c("Number of Passengers Survived" = "nums", "Numbers with Proportions" = "nums_w_props", 
                   "Proportions of Passengers that Survived " = "props", "Proportions with Numbers" = "props_w_nums")

# The display variables used for user choice.
display_vars <- c("Sex", "Number of Siblings/Spouses", "Number of Parents/Children", "Class",
                  #"Ticket", "Cabin", 
                  "Port Embarked From", "Lone Traveller", "Age", "Fare")

# The corresponding data variables of the train_set used throughout program.
data_vars <- colnames(select(train_set, -PassengerId, -Survived, -Age, -Name, -Fare))



# Determine statistics for each group of the variable.
calculate_survival_stats <- function(variable) {
  
  survival_stats <- train_set %>%
    select(variable, Survived) %>%
    filter(complete.cases(.), !!as.name(variable) != "") %>%
    group_by_at(variable) %>%
    summarize(total = n(), totalSurvived = sum(Survived==1), 
              totalDead = total-totalSurvived,
              survivalProbability = round(totalSurvived/total, 2),
              deathProbability = round(totalDead/total,2))
  
  return(survival_stats)
}

# Produce the charts for numbers of passengers vs variable.
produce_chart_numbers <- function(variable, statistic, survival_stats) {
  
  colors <- c("Total (N)" = "red1", "Survived (s/p)" = "orange1")
  
  p <- ggplot(survival_stats) +
    # Graphs.
    geom_bar(aes(x = !!as.name(variable), y = total, fill = "Total (N)"), 
             stat = "identity", position = "dodge")    + 
    geom_bar(aes(x = !!as.name(variable), y = totalSurvived, fill = "Survived (s/p)"), 
             stat = "identity", position = "dodge")   + 
    # Handle faceting
             {if (length(variable) > 1)
               facet_wrap(variable) } +
    # Numeric values on top of bars.
    geom_text(aes(x = !!as.name(variable), y = total, label = paste("N=", total)), 
              position = position_dodge(width=0.9), vjust=-0.25) +
              {if (statistic == "nums") 
                geom_text(aes(x = !!as.name(variable), y = totalSurvived, label = paste("s=", totalSurvived)), 
                          position = position_dodge(width=0.9), vjust=-0.25) } +
                          {if (statistic == "nums_w_props") 
                            geom_text(aes(x = !!as.name(variable), y = totalSurvived, label = paste("p=", survivalProbability)), 
                                      position = position_dodge(width=0.9), vjust=-0.25) } +
    # Categorical labels and color.
    labs(x = display_vars[match(variable, vars_to_colnames)], y = "Number of Passengers", color = "Legend")  +
    scale_fill_manual(name = "Legend", values = colors)
  
  return(p)
}

# Produce the charts for survival probability vs variable.
produce_chart_probabilities <- function(variable, statistic, survival_stats) {
  
  colors <- c("Survival Probability (s/p)" = "red1")
  
  p <- ggplot(survival_stats) + 
    # Graphs.
    geom_bar(aes(x = !!as.name(variable), y = survivalProbability, fill="Survival Probability (s/p)"), 
             stat="identity", position="dodge") + 
    # Handle faceting
             {if (length(variable) > 1)
               facet_wrap(variable) } +
    # Numeric values on top of bars.
               {if (statistic == "props") 
                 geom_text(aes(x = !!as.name(variable), y = survivalProbability, label = paste("p=", survivalProbability)), 
                           position = position_dodge(width=0.9), vjust=-0.25) } +
                           {if (statistic == "props_w_nums") 
                             geom_text(aes(x = !!as.name(variable), y = survivalProbability, label = paste("s=", totalSurvived)), 
                                       position = position_dodge(width=0.9), vjust=-0.25) } +
    # Categorical labels and color.
    labs(x = display_vars[match(variable, vars_to_colnames)], y = "Probibility of Survival", color = "Legend")  +
    scale_fill_manual(name = "Legend", values = colors)
  
  return(p)
}

prep_PCA <- function(feat) {
  
  # Remove Categorical Data from selection and modify the names of AgeCat, FareCat
  if ("Sex" %in% feat) 
    feat[which(feat == "Sex")] <- NA
  if ("Embarked" %in% feat) 
    feat[which(feat == "Embarked")] <- NA
  if ("Alone" %in% feat) 
    feat[which(feat == "Alone")] <- NA
  if ("AgeCat" %in% feat)
    feat[which(feat == "AgeCat")] <- "Age" 
  if ("FareCat" %in% feat) 
    feat[which(feat == "FareCat")] <- "Fare"
  
  # Use all features if user not select enough.
  if (length(na.omit(feat)) < 2) 
    feat <- c("Age", "Fare", "SibSp", "Parch", "Pclass", "Survived")
  feat <- na.omit(feat)
  
  # Select the user-chosen data.
  set <- na.omit(select(train_set, c(Age, Fare, SibSp, Parch, Pclass, Survived)))
  set <- select(set, c(feat, "Survived"))
  set$Survived <- factor(x = set$Survived, levels = c(1,0), labels = c("Lived", "Died"))
  
  return(set)
}

dataframe_PCA <- function(feat) {
  
  # Get the prepared set from function.
  set <- prep_PCA(feat)
  set1 <- select(set, -Survived)
  
  # Calculate the PCAs and proportional variances.
  set.pca <- prcomp(x = set1, scale. = TRUE)
  prop_var <- round(set.pca$sdev ^ 2 / sum(set.pca$sdev ^ 2), digits = 3)
  headers <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8")
  headers <- head(headers, length(prop_var))
  #df <- data.frame("PC" = headers, "Proportion of Deviation" = prop_var)
  df <- data.frame(1)
  for (i in 1:length(headers)) {
    df[headers[i]] = prop_var[i]
  }
  df <- select(df, -X1)
  return(df)
}

# Return plot of principle component analysis.
plot_PCA <- function(feat, y_pca, x_pca) {
  
  # Get the prepared set from function.
  set <- prep_PCA(feat)
  set1 <- select(set, -Survived)
  
  # Calculate the PCAs and proportional variances.
  set.pca <- prcomp(x = set1, scale. = TRUE)
  pcPlus <- mutate(as.data.frame(set.pca$x), Survived = set$Survived)
  prop_var <- round(set.pca$sdev ^ 2 / sum(set.pca$sdev ^ 2), digits = 3)
  
  # Create and return the PCA plot.
  p <- ggplot(pcPlus, aes(x = !!as.name(paste("PC", x_pca, sep="")), 
                          y = !!as.name(paste("PC", y_pca, sep="")), 
                          col = Survived)) +
    geom_point() + 
    labs(x = paste0("PC", x_pca, "(", prop_var[x_pca], ")"), 
         y = paste0("PC", y_pca, "(", prop_var[y_pca], ")")) +
    theme(legend.position="top")
  
  return(p)
}

python_ML <- function(embarked, pclass, sex, age, fare, parch, sibSp, ML_algorithm, num_folds) {
  
  # Modify the names of paramters if not selected
  if (pclass != "user_no_pclass") {pclass = as.integer(pclass)}
  else {pclass = NULL}
  
  if (sex == "user_no_sex") {sex = NULL}
  if (age < 0) {age = NULL}
  if (fare < 0) {fare = NULL}
  if (parch < 0) {parch = NULL}
  if (sibSp < 0) {sibSp = NULL}
  
  df <- data_frame(Pclass = pclass, Sex = sex, Age = age, Embarked = embarked,
                   Alone = ifelse(pclass == 0 && parch == 0, "Alone", "Not Alone"))
  
  #source_python("titanic_ML.py")
  #result <- caller_from_R(ML_algorithm, embarked, pclass, sex, age, fare, parch, sibSp, num_folds)
  
  objControl <- caret::trainControl(method='cv', number=num_folds, returnResamp='none', 
                             summaryFunction = twoClassSummary, classProbs = TRUE)
  
  tt_set <- train_set %>%
    select(Pclass, Sex, Age, Embarked, Alone, Survived) %>%
    mutate(Survived = ifelse(Survived == 1, "yes", "no"))
  tt_set$Sex <- factor(tt_set$Sex)
  tt_set$Embarked <- factor(tt_set$Embarked)
  tt_set$Alone <- factor(tt_set$Alone)
  Y_survived <- factor(tt_set$Survived)
  tt_set <- select(tt_set, -Survived)

  #View(tt_set)
  #View(Y_survived)
  
  objModel <- caret::train(tt_set, Y_survived, 
                    method='gbm', 
                    trControl=objControl,  
                    metric = "ROC",
                    preProc = c("center", "scale"))
  
  predictions <- predict(object=objModel, df, type='prob')
  #print(postResample(pred=predictions, obs=tt_set))
  
  return(predictions)
}

purchase_ticket <- function(embarked, pclass, sex, age, fare, parch, sibSp, ML_algorithm, num_folds) {
  
  result = python_ML(embarked, pclass, sex, age, fare, parch, sibSp, ML_algorithm, num_folds)
  
  paste("The ML Algorithm has spoken. There is a", round(result[1,2],2), "% chance of you surviving!")
}

cross_validation <- function(embarked, pclass, sex, age, fare, parch, sibSp, ML_algorithm, num_folds) {
  result = python_ML(embarked, pclass, sex, age, fare, parch, sibSp, ML_algorithm, num_folds)
  df <- data.frame(matrix(unlist(result[2]), nrow=length(result[2]), byrow=T),stringsAsFactors=FALSE)
  df
}

calc_observed_vals <- function(feat) {
  # Get the data.
  df <- calculate_survival_stats(feat) %>%
    select(-total, -deathProbability, -survivalProbability)
  # Shape in two way table.
  df <- as.data.frame(t(as.matrix(df)))
  names(df) <- as.character(unlist(df[1,]))
  df <- df[-1,] 
  df <- rownames_to_column(df, " ")
  df[1,1] = "lived"
  df[2,1] = "died"
  
  return(df)
}

calc_expected_vals <- function(feat) {
  # Get the data.
  DF <- calculate_survival_stats(feat) 
  equalProb = sum(DF$totalSurvived) / sum(DF$total)
  df <- DF %>%
    select(-totalSurvived, -deathProbability, -survivalProbability) %>%
    mutate(lived = round(equalProb*total, 0), died = round((1 - equalProb) * total), 0) %>%
    select(lived, died)
  
  # Shape in two way table.
  df <- as.data.frame(t(as.matrix(df)))
  names(df) <- as.character(unlist(DF[feat]))
  df <- rownames_to_column(df, " ")
  
  return(df)
}

get_chi_squared <- function(feat) {
  e <- calc_expected_vals(feat)[-1]
  o <- calc_observed_vals(feat)[-1]
  chisq = 0
  for (i in 1:ncol(e)) {
    for (j in 1:nrow(e)) {
      chisq = chisq + (as.numeric(as.character(o[j, i])) - as.numeric(as.character(e[j, i])))^2 / as.numeric(as.character(e[j, i]))
    }
  }
  return(c(chisq, nrow(e), ncol(e)))
}

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
                          numericInput(inputId = "bins_for_fare", label = "Width of an Fare Category (check/uncheck Fare Box)", value = 10)
                        ),
                        # Main panel to display graphs.
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Survival vs Features, Plots", 
                                               radioButtons(inputId = "type_plots", label = "Statistics to View", choices = display_stats, selected = "nums"),
                                               plotOutput("variable_vs_survival")),
                                      
                                      tabPanel("Survival vs Features, Table", 
                                               tableOutput("survival_table")),
                                      
                                      tabPanel("Principle Component Analysis",
                                               numericInput(inputId = "y_pca", label = "PCA on Y Axis", value = 2),
                                               numericInput(inputId = "x_pca", label = "PCA on X Axis", value = 1),
                                               plotOutput("dimensionality_reduction_plot"), 
                                               tableOutput("dimensionality_reduction_table")),
                                      
                                      tabPanel("Help", 
                                               textOutput("help_plots"))
                          )
                        )
                      )
             ),
             
             tabPanel("Purchase a Ticket",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(inputId = "user_embarked", label = "Where will you embark from?", selected = "C", 
                                       choices = c("Cherbourg" = "C", "Queenstown" = "Q", "Southampton" = "S")),
                          radioButtons(inputId = "user_sex", label = "What is Your Sex?", selected = "Male", 
                                       choices = c("Male" = "Male", "Female" = "Female")),
                          radioButtons(inputId = "user_pclass", label = "Choose Your Class", selected = "3",
                                       choices = c("First" = "1", "Second" = "2", "Third" = "3")),
                          numericInput(inputId = "user_fare", value = 7, label = "Price of Ticket 
                                       (Third Class: $7-$70, Second Class: $11-$74, First Class: $26-$512)+"),
                          numericInput(inputId = "user_age", label = "What is Your Age", value = 25),
                          numericInput(inputId = "user_sibSp", value = 0, 
                                       label = "Number of siblings/spouses that will acompany you?"),
                          numericInput(inputId = "user_parch", value = 0, 
                                       label = "Number of parents/children that will acompany you?")
                          ),
                        # Main panel to display for Ticket purchase.
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("ML Ticket Purchase", 
                                               radioButtons(inputId = "ML_algorithm", label = "Choose Your ML Algorithm", 
                                                            choices = c("Percent Binary Classification" = "binary_classifier"), selected = "binary_classifier"),
                                               numericInput(inputId = "num_folds", label = "How many folds for cross validation?", value = 3),
                                               textOutput("purchase_ticket"),
                                               imageOutput("img1")),
                                      
                                      tabPanel("Thoughts",
                                               textOutput("help_ML"),
                                               imageOutput("img3"))
                          )
                        )
                      )
  ),
  
  tabPanel("Statistical Analysis",
           sidebarLayout(
             sidebarPanel(
               selectizeInput(inputId = "which_feature", label = "Feature for Statistical Inference", 
                              choices = c("Sex"="Sex", "Class" = "Pclass",
                                          "Port Embarked From" = "Embarked", 
                                          "Lone Traveller" = "Alone"), selected="Sex")
             ),
             mainPanel(
               tabsetPanel(type = "tabs",
                           tabPanel("Assumptions",
                                    textOutput("analysis1")),
                           tabPanel("Hypothesis",
                                    textOutput("analysis2"), 
                                    verbatimTextOutput("analysis3"), 
                                    textOutput("analysis4"), 
                                    verbatimTextOutput("analysis5")
                           ),
                           tabPanel("Two Way Tables",
                                    textOutput("analysis6"),
                                    tableOutput("observed_values"),
                                    textOutput("analysis7"),
                                    tableOutput("expected_values")
                           ),
                           tabPanel("Test Statistic",
                                    textOutput("analysis8"),
                                    verbatimTextOutput("analysis9"),
                                    verbatimTextOutput("analysis10"),
                                    plotOutput("chi_plot")
                           ),
                           tabPanel("Conclusions",
                                    textOutput("analysis11")
                           )
               )
             )
           )
  ),
  
  tabPanel("About",
           verticalLayout(
             mainPanel(
               textOutput("author"),
               textOutput("about"),
               imageOutput("img4")
             )
           )
  )
             )
  )

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
                deathProbability = round(totalDead/total, 2))
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
    plot_PCA(input$variable, input$y_pca, input$x_pca)
  })
  
  output$dimensionality_reduction_table <- renderTable({
    dataframe_PCA(input$variable)
  })
  
  # Help tab for plots and interface.
  output$help_plots <- renderText({
    print("For plotting survival vs features or producing the associating table, select one or more features
          from the left hand side. Towards the bottom of the side panel, there are two numerical inputs for 
          selecting bin widths. Choosing a bin width of 5 for age will group people ages 0-5 into an 
          age category of 0; people 5-10 into an age category of 5; and so on. This is nessecary to produce 
          the categorical plots because the dataset I am working with is relatively small. For the principle 
          component analysis, select two or more categorical variables to use them in the PCA process. By 
          default, all features will be used unless you select two or more categorical features.")
  })
  
  output$purchase_ticket <- renderText({
    purchase_ticket(input$user_embarked, input$user_pclass, input$user_sex, input$user_age, input$user_fare, 
                    input$user_parch, input$user_sibSp, input$ML_algorithm, input$num_folds)
  })
  
  output$img1 <- renderImage({
    list(src = "gravestone.png",
         contentType = 'image/png',
         width = 650,
         height = 433)
  }, deleteFile = FALSE)
  
  output$img2 <- renderImage({
    list(src = "life.png",
         contentType = 'image/png',
         width = 496 * 1.5,
         height = 250 * 1.5)
  }, deleteFile = FALSE)
  
  output$img3 <- renderImage({
    list(src = "overfitting.png",
         contentType = 'image/png',
         width = 700,
         height = 500)
  }, deleteFile = FALSE)
  
  output$img4 <- renderImage({
    list(src = "titanic.png",
         contentType = 'image/png',
         width = 800,
         height = 450)
  }, deleteFile = FALSE)
  
  output$cross_validation <- renderTable({
    cross_validation(input$user_embarked, input$user_pclass, input$user_sex, input$user_age, input$user_fare, 
                     input$user_parch, input$user_sibSp, input$ML_algorithm, input$num_folds)
  })
  
  # Help tab for ML ticket purchase.
  output$help_ML <- renderText({
    print("Binary classification may not be the best algorithm to use for this problem, but it was something
          I was able to acomplish. At first, I used all of the features in the classification. However, I 
          realized that this led to extremely poor results. For example, at age 20 the algorithm might
          decide you die, at age 21 you live, and age 22 you die. Additionally, the cross validation accuracies
          were all over the place. I believe it was over fitting numeric data. Therefore,
          I streamlined the data to only consider Sex, Class, Port Embarked, and whether or not you are Alone.")
  })
  
  output$analysis1 <- renderText({
    paste("The titanic is one shipwreck among many throughout history. 
          Let us assume that every shipwreck throughout the course of history is identical to the tianic
          and those who died on the titanic are a mere samll sample of total shipwreck deaths. In this
          scenario, our sample, the titanic data, is relativly small compared to all of those who have died
          at sea in the past. Thus, we may use statistics to make inference about whether or not death is fair 
          in shipwrecks.")
  })
  
  output$analysis2 <- renderText({
    if (input$which_feature == "Sex") {feat <- "Sex"}
    else if (input$which_feature == "Pclass") { feat <- "Class" }
    else if (input$which_feature == "Embarked") {feat <- "Port of Embarkation"}
    else if (input$which_feature == "Alone") {feat <- "Being a Lone Traveller"}
    paste("Using a Chi Square test for independence, we ask ourselves whether or not there is a relation 
          between", feat, "and survival. We set a rigorous alpha of .01. Our null hypothesis assumes there is not, that is the proportion, p,
          of those who survived among each subgroup is the same:")
  })
  
  output$analysis3 <- renderText({
    if (input$which_feature == "Sex" || input$which_feature == "Alone") {third <- ""}
    else {third <- " == p3"}
    paste("H0 : p1 == p2", third, sep="")
  })
  
  output$analysis4 <- renderText({
    paste("While our alternative hypothesis assumes the opposite:")
  })
  
  output$analysis5 <- renderText({
    if (input$which_feature == "Sex" || input$which_feature == "Alone") {third <- ""}
    else {third <- " != p3"}
    paste("H1 : p1 != p2", third, sep="")
  })
  
  output$analysis6 <- renderText({
    paste("We construct a two way table of observed values:")
  })
  
  output$observed_values <- renderTable({
    calc_observed_vals(input$which_feature)
  })
  
  output$analysis7 <- renderText({
    paste("We construct a two way table of expected values:")
  })
  
  output$expected_values <- renderTable({
    calc_expected_vals(input$which_feature)
  })
  
  output$analysis8 <- renderText({
    chisq <- get_chi_squared(input$which_feature)
    df = (chisq[2] - 1)*(chisq[3] - 1)
    paste("All expected values are greater than 5. We caluclate the test statsistic and p-val for", 
          df, "degrees of freedom")
  })
  
  output$analysis9 <- renderText({
    chisq <- get_chi_squared(input$which_feature)
    paste("chi^2 =", round(chisq[1], 2))
  })
  
  output$analysis10 <- renderText({
    chisq <- get_chi_squared(input$which_feature)
    df = (chisq[2] - 1)*(chisq[3] - 1)
    paste("p-val =", pchisq(chisq[1], df=df, lower.tail = FALSE))
  })
  
  output$chi_plot <- renderPlot({
    chisq <- get_chi_squared(input$which_feature)
    df = (chisq[2] - 1)*(chisq[3] - 1)
    curve(dchisq(x, df = 5), from = 0, to = 50)
  })
  
  output$analysis11 <- renderText({
    if (input$which_feature == "Sex") {feat <- "Sex"}
    else if (input$which_feature == "Pclass") { feat <- "Class" }
    else if (input$which_feature == "Embarked") {feat <- "Port of Embarkation"}
    else if (input$which_feature == "Alone") {feat <- "Being a Lone Traveller"}
    paste("Given the test statistic is far into the rejection region and the pval is much below alpha, we
          feel confident concluding that", feat, "does play a role in your chance of surviving a shipwreck.")
  })
  
  output$author <- renderText({
    print("Copyright 2020, Luke Kurlandski, All Rights Reserved.")
  })
  
  output$about <- renderText({
    print("This is an interactive application that allows users to understand who lived and who died as 
          a consequence of the destruction of the titanic. The datasets come from Kaggle: 
          https://www.kaggle.com/c/titanic/data. In this application, users can view plots of features vs
          survival statistics, purchase a ticket aboard the titanic and let an machine learning algorithm 
          determine whether or not they will survive, and examine a statistical procedure to determine whether
          a particular feature influences survival or not.")
  })
}

# Run the Shiny application .
shinyApp(ui = ui, server = server)

