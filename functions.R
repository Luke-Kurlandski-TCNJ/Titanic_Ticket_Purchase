library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)
library(gridExtra)
library(tidyr)
library(tibble)
library(reticulate)

# Data set of all passengers on the titanic.
train_set <- read.csv("datasets/train.csv") %>% 
  mutate(Alone = mapply(determine_alone, SibSp, Parch)) %>%
  mutate(AgeCat = mapply(determine_age_category, age = Age, bin = 5)) %>%
  mutate(FareCat = mapply(determine_fare_category, fare = Fare, bin = 10)) 
  #FIXME: replace all empty cells with NA

# Useful vector mapping the options to display to their column names.
vars_to_colnames <- c("Sex"="Sex", "Number of Siblings/Spouses" = "SibSp", 
                      "Number of Parents/Children" = "Parch", 
                      #"Ticket" = "Ticket", "Cabin" = "Cabin", 
                      "Port Embarked From" = "Embarked", 
                      "Lone Traveller" = "Alone", "Age" = "AgeCat", "Fare" = "FareCat")

# Similarily, map the options of display statistics to variable names.
display_stats <- c("Number of Passengers Survived" = "nums", "Numbers with Proportions" = "nums_w_props", 
                   "Proportions of Passengers that Survived " = "props", "Proportions with Numbers" = "props_w_nums")

# The display variables used for user choice.
display_vars <- c("Sex", "Number of Siblings/Spouses", "Number of Parents/Children",
                  #"Ticket", "Cabin", 
                  "Port Embarked From", "Lone Traveller", "Age", "Fare")

# The corresponding data variables of the train_set used throughout program.
data_vars <- colnames(select(train_set, -PassengerId, -Survived, -Age, -Name, -Fare))

# Apply an age category to an age.
determine_age_category <- function(age, bin = 5) {
  if (is.na(age) || age=="")
    return(NA)
  else
    return(bin * (age %/% bin))
}

# Apply a fare category to a fare.
determine_fare_category <- function(fare, bin = 10) {
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

purchase_ticket <- function(pclass, sex, age, fare, parch, sibSp, ML_algorithm) {
  source_python("titanic_ML.py")
  result <- caller_from_R(pclass, sex, age, fare, parch, sibSp, ML_algorithm, train_set)
  print(paste("The ML Algorithm has spoken: ", result))
}