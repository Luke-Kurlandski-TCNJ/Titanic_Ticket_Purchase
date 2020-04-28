# Luke Kurlandski
# CSC275: Intro to Data Science 
# Final Project
# April 2020
# Description: the runner app for the Titanic data science project.

# To run this app: open zshell, conda activate, open anaconda Rstudio and Spyder, in Rstudio shell: conda deactivate then conda activate

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