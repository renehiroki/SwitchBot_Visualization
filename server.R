source("my_func.R")
library(plotly)
library(tidyverse)
library(shiny)
library(shinythemes)



shinyServer(function(input, output, session) {
  
  data <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           validate("Invalid file; Please upload a .csv file")
    )
  })
  
  preped_data <- reactive({
    data() %>% f_preprocessing()
  })

  output$plot_dot_temp <- renderPlotly({
    preped_data() %>% f_plot_dot("temperature")
  })

  output$plot_dot_humid <- renderPlotly({
    preped_data() %>% f_plot_dot("humidity")
  })
  
  output$plot_line_temp <- renderPlotly({
    preped_data() %>% f_plot_line("temperature")
  })

  output$plot_line_humid <- renderPlotly({
    preped_data() %>% f_plot_line("humidity")
  })
  
  output$plot_line_long_temp <- renderPlotly({
    preped_data() %>% f_plot_line_long("temperature", width = 1000)
  })

  output$plot_line_long_humid <- renderPlotly({
    preped_data() %>% f_plot_line_long("humidity", width = 1000)
  })
  
  output$downloadReport <- downloadHandler(
    filename = paste0('report_', Sys.Date(), '.html'),
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # # temporarily switch to the temp dir, in case you do not have write
      # # permission to the current working directory
      # owd <- setwd(tempdir())
      # on.exit(setwd(owd))
      # file.copy(src, 'report.Rmd')
      # 
      library(rmarkdown)
      out <- render(input = 'report.Rmd',
                    output_format = html_document(),
      )
      
      file.rename(out, file)
    }
  )
})
