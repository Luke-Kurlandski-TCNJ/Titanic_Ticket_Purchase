options(stringsAsFactors=FALSE)

library(dplyr)
library(lubridate)
library(ggplot2)
library(tibble)
library(shiny)

theme_set(theme_bw())

# Data set of all passengers on the titanic
train_set <- read.csv("datasets/train.csv")

# Separate based upon the type of data.
train_set_categorical <- train_set %>%
  select(Survived, Pclass, Name, Sex, Ticket, Cabin, Embarked)
train_set_numerical <- train_set %>%
  select(Survived, Age, SibSp, Parch, Fare)

aspect = "Pclass"
print("Here")
# Select the associated aspect, and Survived columns.
set <- select(train_set_categorical, aspect, Survived)
print("Here")
# Determine statistics for each group.
set <- set %>%
  group_by_at(aspect) %>%
  summarize(total=n(), 
            totalSurvived=sum(Survived==1), 
            totalDead=total-totalSurvived,
            survivalProbability=totalSurvived/total)
print("Here")
View(set)
print("Here")
p <- ggplot(set, aes(x=Pclass, y=total)) +
  geom_bar(stat="identity")
print(p)
print("Here")

