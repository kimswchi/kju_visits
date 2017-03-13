library(shiny)

shinyUI(fluidPage(
  titlePanel("Kim Jong Un Going to Places"),
  br(),
  
  fluidRow(
    column(4, dateInput('date',
                       label = 'Enter date',
                       value = NULL,
                       min = '2016-01-02', max = '2017-03-11'),
             
             dateRangeInput('dateRange',
                            label = 'Enter date range',
                            start = NULL, end = NULL,
                            min = '2016-01-02', max = '2017-03-11')),
    
    column(3, br(),
           verbatimTextOutput("Visit"),
           h4("State newspaper coverage:"),
           h5("Reports"),
           verbatimTextOutput("Article"),
           h5("Photos"),
           verbatimTextOutput("Pic")),
    
    column(5, plotOutput("nkmap"))
  ),
  
  fluidRow(column(12, wellPanel(
    tableOutput("results"))))
))