library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)
library(gridExtra)
library(tidyr)
library(tibble)

# Data set of all passengers on the titanic.
train_set <- read.csv("datasets/train.csv") %>% 
  mutate(Alone = mapply(determine_alone, SibSp, Parch)) %>%
  mutate(AgeCat = mapply(determine_age_category, Age)) %>%
  mutate(FareCat = mapply(determine_fare_category, Fare))
  #FIXME: replace all empty cells with NA

# Separate based upon the type of data.
variables_categorical <- c("Pclass", "Sex", "Ticket", "Cabin", "Embarked", "Cabin", 
                           "Ticket", "AgeCat", "Alone", "SibSp", "Parch", "FareCat")
vars_to_colnames <- c("Sex"="Sex", "Number of Siblings/Spouses" = "SibSp", 
                      "Number of Parents/Children" = "Parch", "Ticket" = "Ticket", 
                      "Cabin" = "Cabin", "Port Embarked From" = "Embarked", 
                      "Lone Traveller" = "Alone", "Age" = "AgeCat", "Fare" = "FareCat")

display_vars <- c("Sex", "Number of Siblings/Spouses", "Number of Parents/Children",
                  "Ticket", "Cabin", "Port Embarked From", "Lone Traveller", "Age", "Fare")

data_vars <- colnames(select(train_set, -PassengerId, -Survived, -Age, -Name, -Fare))

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