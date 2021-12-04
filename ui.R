library(shiny)
library(shinythemes)
library(plotly)
library(shinydashboard)
library(shinydashboardPlus)

today <- Sys.Date()

shinyUI(
  
  dashboardPage(
    
    dashboardHeader(title = "SwitchBot Viz"),

# sidebar -----------------------------------------------------------------
    dashboardSidebar(width = 220,
      
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
      ),
      
      fileInput(inputId = "upload", label = NULL, buttonLabel = "upload csv", accept = ".csv"),
      
     "上図・右下図の日付範囲指定",
      dateRangeInput(
        "daterange",
        label = NULL,
        start = today - 31,
        end = NULL,
        min = today - 365 * 2,
        max = NULL,
        format = "yyyy-mm-dd",
        startview = "month",
        weekstart = 1,
        language = "ja",
        separator = " ~ ",
        width = NULL,
        autoclose = TRUE
        ),
     br(),

     "左下図の日付範囲指定",
     dateInput(
       "select",
       label = NULL,
       value = NULL,
       min = NULL,
       max = NULL,
       format = "yyyy-mm-dd",
       startview = "month",
       weekstart = 1,
       language = "ja",
       width = NULL,
       autoclose = TRUE,
       datesdisabled = NULL,
       daysofweekdisabled = NULL
     ),
     
     # downloadButton("downloadReport"),
     
     withTags({
       script(src="https://buttons.github.io/buttons.js")
     }),
     withTags({
       a(class="github-button", href="https://github.com/renehiroki/SwitchBot_Visualization", 
         "Source Code")
     })
     
    ),


     
# body --------------------------------------------------------------------

    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(
                  column(10,
                         plotlyOutput("temp_humid_abshumid", width = 1000, height = 440)),
                  column(2,
                         HTML("左上部のupload csv より、SwitchBotの温湿度データをアップロードしてください。<br>"),
                         HTML("<br>"),
                         HTML("飽和水蒸気量:1m^3の空間に存在できる水蒸気の質量をgで表したもの(Wikipedia)<br>"),
                         HTML("<br>"),
                         HTML("絶対湿度:飽和水蒸気量×相対湿度（概算）<br>"),
                         HTML("<br>"),
                         HTML("湿り空気線図:<br>"),
                         )
                ),
                
                br(),
                
                fluidRow(
                  column(6,
                         plotlyOutput("oneday_chart", width = 600, height = 400)),
                  column(5,
                         plotOutput("pyschrometric", width = 450, height = 400),
                         )
                  )
                ))
      
      )
))