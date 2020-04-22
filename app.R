# Luke Kurlandski
# CSC275: Intro to Data Science 
# Final Project
# April 2020
# Description: the runner app for the Titanic data science project.

# Custom configurations based upon preference.
options(stringsAsFactors=FALSE)
theme_set(theme_bw())

# Libraries in use throughout project.
library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)
library(gridExtra)
library(tidyr)
library(tibble)
library(stringr)

# Import the other scripts, sharing global environment.
source("ui.R", local=FALSE)
source("server.R", local=FALSE)
source("functions.R", local=FALSE)

# Run the Shiny application .
shinyApp(ui = ui, server = server)