library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("united"),
  titlePanel("Kim Jong Un Going to Places"),
  h3("Tracking Kim Jong Un's Official Visits Around North Korea"),
  h5("Note: The app may take a few moments to load."),
  br(),
  
  fluidRow(
    column(4, dateInput('date',
                       label = 'Enter date',
                       value = '2017-02-21',
                       min = '2016-01-02', max = '2017-03-11'),
             
             dateRangeInput('dateRange',
                            label = 'Enter date range',
                            start = '2016-12-25', end = '2017-03-11',
                            min = '2016-01-02', max = '2017-03-11'),
           
           selectInput('mapType', 
                       label = 'Select map type', 
                       choices = c('No map', 'Date range', 'Single day'), 
                       selected = 'No map', multiple = FALSE,
                       selectize = TRUE, width = NULL, size = NULL)
           ),
    
    column(5, offset = 3,
           conditionalPanel("input.mapType == 'Date range'",
                               plotOutput("mapRange")),
           conditionalPanel("input.mapType == 'Single day'",
                            plotOutput("mapSingle")))
  ),
  
  fluidRow(column(12, 
    verbatimTextOutput("Visit"),
    h4("State newspaper coverage:"),
    h5("Reports"),
    verbatimTextOutput("Article"),
    h5("Photos"),
    verbatimTextOutput("Pic"))
    ),
  
  fluidRow(column(12, wellPanel(
    tableOutput("results"))))
))