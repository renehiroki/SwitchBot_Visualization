library(shiny)
library(shinythemes)
library(plotly)
shinyUI(fluidPage(
  theme = shinytheme("darkly"),
  

  
  # Application title
  titlePanel("SwitchBot Visualization"),

  fluidRow(
    column(2,
           fileInput(inputId = "upload", label = NULL, buttonLabel = "upload csv", accept = ".csv")
           ),
    
    column(1,
           downloadButton("downloadReport"),
           ),
    
    column(2,
           withTags({
             script(src="https://buttons.github.io/buttons.js")
           }),
           withTags({
             a(class="github-button", href="https://github.com/renehiroki/SwitchBot_Visualization", 
               "Source Code")
           })
           )
  ),
  
  fluidRow(
    column(5,
           div(plotlyOutput("plot_dot_temp", width = 850, height = 430), align = "left")
    ),
    column(5, #offset = 1,
           div(plotlyOutput("plot_line_temp", width = 850, height = 430), align = "center")
           )
  ),
  
  fluidRow(
    column(5,
           div(plotlyOutput("plot_dot_humid", width = 850, height = 430), align = "left")
    ),
    column(5, 
           div(plotlyOutput("plot_line_humid", width = 850, height = 430), align = "center")
           )
  ),

  fluidRow(
    column(10,
           plotlyOutput("plot_line_long_temp", width = 1100, height = 430))
  ),

  fluidRow(
    column(10,
           plotlyOutput("plot_line_long_humid", width = 1100, height = 430))
  )

))
