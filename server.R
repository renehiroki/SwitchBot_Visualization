source("my_func.R")
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
  
  # 
  # level <- reactive({
  #   req(input$level)
  #   cat01_meta %>% 
  #     filter(`@level` == input$level) %>% 
  #     select(`@code`) %>% 
  #     left_join(data, by = c(`@code` = "cat01_code")) %>% 
  #     rename("cat01_code" = `@code`)
  # })
  # 
  # observeEvent(level(), {
  #   choices <- unique(level()$hinmoku)
  #   updateSelectInput(session, "cat01", choices = choices) 
  # })
  # 
  # cat01 <- reactive({
  #   req(input$cat01)
  #   filter(level(), hinmoku == input$cat01)
  # })
  # 
  # 
  # output$plot <- renderPlot({
  #   req(input$YM_seps)
  #   req(input$cat01)
  #   if (input$YM_seps == "YEAR"){
  #     p <-
  #       cat01() %>%
  #       drop_na() %>%
  #       mutate(month = factor(as.character(month), as.character(1:12))) %>%
  #       ggplot(aes(
  #         x = month,
  #         y = value,
  #         color = year,
  #         group = year
  #       )) +
  #       geom_line() +
  #       facet_wrap(vars(setai), ncol = 1) +
  #       labs(title = input$cat01)
  #   }
  #   else {
  #     p <-
  #       cat01() %>%
  #       drop_na() %>%
  #       mutate(month = factor(as.character(month), as.character(1:12))) %>%
  #       ggplot(aes(
  #         x = year,
  #         y = value,
  #         color = month,
  #         group = month
  #       )) +
  #       geom_line() +
  #       facet_wrap(vars(setai), ncol = 1) +
  #       labs(title = input$cat01) +
  #       scale_x_discrete(guide = guide_axis(n.dodge = 2))
  #     
  #   }
  #   print(p)
  # })
  # 
  # output$download <- downloadHandler(
  #   filename = function() {
  #     paste0(input$cat01, ".png")
  #   },
  #   # contentType = "image/png",
  #   content = function(file) {
  #     ggsave(file)
  #   }
  # )
})
