library(tidyverse)
library(lubridate)
library(ggrepel)
library(plotly)
library(zoo)


f_preprocessing <- function(data) {
  d <-
    data %>% 
    mutate(day = paste0(month(Date), "/", day(Date))) %>% 
    left_join(
      data %>% 
        mutate(day = paste0(month(Date), "/", day(Date))) %>% 
        distinct(day) %>% 
        mutate(n = row_number()-1),
      by = "day"
    ) 
  
  d <- 
    d %>% 
    left_join(
      d %>% 
        group_by(day) %>% 
        slice_tail() %>% 
        ungroup() %>% 
        select(Date, label = day),
      by = "Date"
    ) %>% 
    mutate(Date24 = Date - n * 60*60*24 + 1) %>% 
    mutate(day = as_factor(day)) 
  

  
  return(d)
  }

f_plot_dot <- function(preped_data, target_var) {
  
  first_date <- preped_data %>% pull(Date) %>% min() %>% str_split(" ") %>% unlist() %>% .[1]
  last_date <- preped_data %>% pull(Date) %>% max() %>% str_split(" ") %>% unlist() %>% .[1]
  
  tmp <- sym(target_var)
  
  p <- 
    preped_data %>% 
    ggplot(aes(x = Date24, y = !!tmp, color = day, label = label)) +
    geom_point() +
    scale_color_viridis_d() +
    scale_x_datetime(date_labels = "%H",
                     breaks = as_datetime(paste(first_date, "00:00:00")) + c(60 * 60 * 0:24),
                     limits = c(as_datetime(paste(first_date, "00:00:00")),
                                as_datetime(paste(first_date, "00:00:00")) + 60 * 60 * 26)
                     ) +
    theme(text = element_text(size = 16)) +
    labs(title = paste(target_var, "(24 hours)"),
         x = "time", y = "value")
  
  p %>% ggplotly(dynamicTicks = T, width = 700, height = 420)
}

f_plot_line <- function(preped_data, target_var) {
  
  first_date <- preped_data %>% pull(Date) %>% min() %>% str_split(" ") %>% unlist() %>% .[1]
  last_date <- preped_data %>% pull(Date) %>% max() %>% str_split(" ") %>% unlist() %>% .[1]
  
  tmp <- sym(target_var)

  p <- 
    preped_data %>% 
    mutate(value = rollmean(!!tmp, k = 5, na.pad = TRUE)) %>% 
    ggplot(aes(x = Date24, y = value, color = day, label = label)) +
    geom_line() +
    scale_color_viridis_d() +
    scale_x_datetime(date_labels = "%H",
                     breaks = as_datetime(paste(first_date, "00:00:00")) + c(60 * 60 * 0:24),
                     limits = c(as_datetime(paste(first_date, "00:00:00")),
                                as_datetime(paste(first_date, "00:00:00")) + 60 * 60 * 26)
                     ) +
    theme(text = element_text(size = 16)) +
    labs(title = paste(target_var, "(24 hours)"),
         x = "time", y = "value")
  
  p %>% ggplotly(dynamicTicks = T, width = 700, height = 420)
}

f_plot_line_long <- function(preped_data, target_var, width = 700) {
  
  first_date <- preped_data %>% pull(Date) %>% min() %>% str_split(" ") %>% unlist() %>% .[1]
  last_date <- preped_data %>% pull(Date) %>% max() %>% str_split(" ") %>% unlist() %>% .[1]
  
  tmp <- sym(target_var)
  
  p <- 
    preped_data %>% 
    mutate(value = rollmean(!!tmp, k = 15, na.pad = TRUE)) %>% 
    ggplot(aes(x = Date, y = value, color = day, label = label)) +
    geom_line() +
    scale_color_viridis_d() +
    scale_x_datetime(date_labels = "%H") +
    theme(text = element_text(size = 16)) +
    labs(title = target_var, 
         x = "time", y = "value")
  
  p %>% ggplotly(dynamicTicks = T, width = width, height = 420)
}
