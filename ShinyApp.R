##Cleaning Leftover Data
# rm(list=ls())

my.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my.dir)

###Load packages
library(shiny)
library(tidyverse)
library(plotly)

##Client Side
ui <- fluidPage(
  
  titlePanel("Soccer Results WebApplication"),
  
  fluidRow(column(3, dateRangeInput("dates", "Choose a date range:",
                                               start = "1930-01-01",
                                               end = "2019-01-01"), 
                  selectInput('tourney', 
                              label = 'Tournament',
                              choices = c('All', unique(Teams$tournament)),
                              multiple = T,
                              selected = 'All'
                              ),
                  radioButtons('yncountry', 
                               'Country',
                               choices = c('Yes', 'No'), 
                               selected = 'No',
                               inline = T),
                  selectInput('countrydesc',
                              label = NULL,
                              choices = unique(Teams$country),
                              multiple = T)
                  ),
  column(9,
         tabsetPanel(Statistics,
                     H2H,
           tabPanel(tableOutput('details'), title = 'Details')
         ))
  )
)  

#Run the app
shinyApp(ui = ui, server = server)