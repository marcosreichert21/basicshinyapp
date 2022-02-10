##Cleaning Leftover Data
# rm(list=ls())

# my.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(my.dir)

###Load packages
library(shiny)
library(tidyverse)
library(plotly)
library(DT)

##Client Side
ui <- fluidPage(
   titlePanel(h1("Soccer Results WebApplication", style={'background-color:#2ecc71;
                        margin-left: -15px;
                        margin-right: -15px;
                        padding: 10px;
                        margin-up: -15px'})),
  #br(),
  fluidRow(column(3, dateRangeInput("dates", "Choose a date range:",
                                               start = "1930-01-01",
                                               end = "2019-01-01"), 
                  selectInput('tourney', 
                              label = 'Tournament:',
                              choices = c('All', unique(Teams$tournament)),
                              multiple = T,
                              selected = 'All'
                              ),
                  radioButtons('yncountry', 
                               'Country:',
                               choices = c('Yes', 'No'), 
                               selected = 'No',
                               inline = T),
                  selectInput('countrydesc',
                              label = NULL,
                              choices = unique(Teams$country),
                              multiple = T), style = {"background-color:#1abc9c; 
                                margin: -10px;
                                padding-down: 1200px"}
                  ),
  column(9,
         tabsetPanel(Statistics,
                     H2H,
                     details
         ))
  )
)  

#Run the app
shinyApp(ui = ui, server = server)