#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(corrgram)
library(datasets)
library(xlsx)
library(randomForest)



fluidPage(pageWithSidebar(
  headerPanel('Data Analysis CSV'),
  sidebarPanel(
    # selectInput("dataset", "Choose a dataset:", 
    fileInput("file", label = h3("File input",multiple = FALSE,accept = NULL,width=NULL),
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv','.xlsx')),
    numericInput("obs", "Number of observations to view:", 10),
    checkboxInput("header", "Header", TRUE),
    radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),','),
    radioButtons('quote', 'Quote',c(None='','Double Quote'='"','Single Quote'="'"),'"'),
    selectInput("xcol", "X Variable",choices = names(df)),
    selectInput("ycol", "Y Variable", choices = names(df)),
    sliderInput("type","Point Type",min=1, max=25,value = 19),
    numericInput("size","Point size",3,min=0.5,max = 10),
    numericInput("bins","Number of Bins",10,min=1,max = 50),
    numericInput("frequency","Histogram Frequency",2,min=1,max = 1000),
    selectInput("line",label = "Line Type",choices = list("Line"="l","Points"="p","Step"="s","Broken Points"="b","None"="n"),selected = "p"),
    selectInput(inputId = 'color',label =  'Color', choices = list("Blue","Red","Yellow","Green","Black","Orange","Pink","Brown","LightGreen","LightBlue","LightGrey"),
                selected = "Blue"),
    selectInput(inputId = 'bordercolor',label =  'Bin Border Color', choices = list("Blue","Red","Yellow","Green","Black","Orange","Pink","Brown","LightGreen","LightBlue"),
                selected = "Black"),
    downloadButton('downloadData', 'Download')
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot")),
      tabPanel("Table",tableOutput('table')),
      tabPanel("Observations",tableOutput("observations")),
      tabPanel("Summary",verbatimTextOutput("summary")),
      tabPanel("Histogram",plotOutput("hist")),
      tabPanel("Correlation",verbatimTextOutput("correlation")),
      tabPanel("Correlogram", plotOutput("corr")),
      tabPanel("Simple Linear Regression",verbatimTextOutput("summarylm")),
      # tabPanel("Random Forest", plotOutput("forest")),
      tabPanel("File Information",fluidRow(column(4, verbatimTextOutput("value"))))
    )
  )))
