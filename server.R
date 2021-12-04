library(plotly)
library(tidyverse)
library(shiny)
library(shinythemes)
library(ggrepel)
library(lubridate)
library(plotly)
library(zoo)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
options(encoding = "utf-8")
source("my_func.R")


bashcode <- "
mkdir ~/tmp;
cd ~/tmp;
curl -O -L https://moji.or.jp/wp-content/ipafont/IPAexfont/IPAexfont00401.zip;
unzip IPAexfont00401.zip;
mkdir -p ~/.fonts/ipa;
cp ./IPAexfont00401/*.ttf ~/.fonts/ipa;
fc-cache -f ~/.fonts;
"
system(bashcode)


shinyServer(function(input, output, session) {

  data_raw <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           validate("Invalid file; Please upload a .csv file")
    )
  })
  
  data <- reactive({
    d <- data_raw() 
    colnames(d) <- c("Date", "temperature", "humidity")
    d <- 
      d %>%
      filter(input$daterange[1] <= Date, Date <= input$daterange[2])
    d
  })
  
  data <- reactive({
    d <- read_csv("demodata.csv") 
    colnames(d) <- c("Date", "temperature", "humidity")
    d <- 
      d %>%
      filter(input$daterange[1] <= Date, Date <= input$daterange[2])
    d
  })
  
  
  preped_data <- reactive({
    data() %>% f_preprocessing()
  })
  
  oneday_data <- reactive({
    min_date <- min(preped_data()$Date)
    max_date <- max(preped_data()$Date)
    selected_date <- if_else(as_date(input$select) < as_date(min_date), as_date(min_date), 
                             if_else(as_date(input$select) > as_date(max_date), as_date(max_date), as_date(input$select))
    )
    
    preped_data() %>% 
      filter(selected_date-1 <= as_date(Date), 
             as_date(Date) <=  selected_date+1)
  })
  
# humid plot --------------------------------------------------------------

  output$humid <- renderPlotly({
    preped_data() %>% plot_humidity()
  })
  

# temperature and humidity ------------------------------------------------
  
  output$temp_and_humid <- renderPlotly({
    p_temp <- preped_data() %>% plot_temperature()
    p_humid <- preped_data() %>% plot_humidity()
    
    subplot(p_temp, p_humid, nrows = 2, shareX = TRUE, titleY = TRUE) 
  })
  

# abs humidity ------------------------------------------------------------

  output$abs_humidity <- renderPlotly({
    preped_data() %>% plot_abs_humidity()
  })

# temp humid abshumid -----------------------------------------------------

  output$temp_humid_abshumid <- renderPlotly({
    preped_data() %>% plot_temp_humid_abshumid()
  })

# pyschrometric -----------------------------------------------------------

  output$pyschrometric <- renderPlot({
    preped_data() %>% plot_pyschrometric()
  })
  

# one day chart -----------------------------------------------------------

  output$oneday_chart <- renderPlotly({
    oneday_data() %>% plot_temp_humid_abshumid()
  })  
  
# download ----------------------------------------------------------------
  
  # output$downloadReport <- downloadHandler(
  #   filename = paste0('report_', Sys.Date(), '.html'),
  #   
  #   content = function(file) {
  #     src <- normalizePath('report.Rmd')
  #     
  #     # # temporarily switch to the temp dir, in case you do not have write
  #     # # permission to the current working directory
  #     # owd <- setwd(tempdir())
  #     # on.exit(setwd(owd))
  #     # file.copy(src, 'report.Rmd')
  #     # 
  #     library(rmarkdown)
  #     out <- render(input = 'report.Rmd',
  #                   output_format = html_document(),
  #     )
  #     
  #     file.rename(out, file)
  #   }
  # )
  })
