##Cleaning Leftover Data
rm(list=ls())

my.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my.dir)

###Load packages
library(shiny)
library(tidyverse)

SoccerResults <- read_csv('results.csv')

##Server Side
server <- function(input, output, session) {}

##Client Side
ui <- fluidpage()

#Run the app
shinyApp(ui = ui, server = server)