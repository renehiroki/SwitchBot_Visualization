library(shiny)
library(tidyverse)
library(shinythemes)
shinyUI(fluidPage(
  theme = shinytheme("darkly"),
  
  
  # Application title
  titlePanel("SwitchBot Visualization"),

  fileInput("upload", NULL, accept = ".csv"),
  
  
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

  br(),

  fluidRow(
    column(10,
           plotlyOutput("plot_line_long_temp", width = 1100, height = 430))
  ),

  fluidRow(
    column(10,
           plotlyOutput("plot_line_long_humid", width = 1100, height = 430))
  )
  # downloadButton("download"),
  
          # selectInput("level", "LEVEL", choices = as.character(1:5)),
          # selectInput("cat01", "CATEGORY", choices = NULL),
          # selectInput("YM_seps", "YEAR or MONTH", choices = unique(c("YEAR", "MONTH")))
          # plotOutput("plot", height = "650px", width = "900px")
  )
)

