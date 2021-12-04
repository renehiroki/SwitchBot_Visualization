library(tidyverse)
library(lubridate)
library(ggrepel)
library(plotly)
library(zoo)


# preprocessing -----------------------------------------------------------

tetens <- function(temp) {
  6.1078 * 10^(7.5*temp / (temp + 237.3))
}

max_humid <- function(temp) {
  217 * tetens(temp) / (temp + 273.15)
}

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
  
  d <- 
    d %>% 
    mutate(max_humidity = max_humid(temperature)) %>% 
    mutate(abs_humidity = max_humidity / 100 * humidity) 
  
  return(d)
  }


# before ------------------------------------------------------------------


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
    ggplot(aes(x = Date, y = value, label = label)) +
    geom_line() +
    # scale_color_viridis_d() +
    scale_x_datetime(date_labels = "%H") +
    labs(title = "", 
         y = "温度")
  
  p %>% ggplotly(dynamicTicks = T, width = width, height = 420) %>% 
    layout(
      xaxis = list(type = 'date',
                   tickformat = "%Y-%m-%d"),
      yaxis = list(ticksuffix = "℃")
    )
  
}



# temperature  ------------------------------------------------


plot_temperature <- function(data) {
  
  p <- 
    data %>% 
    ggplot(aes(Date, temperature, 
               text = map(paste('<b> 日時:</b>', month(Date), "月", day(Date), "日", hour(Date), "時", minute(Date), "分", '<br>',
                                '<b>温度:</b>', temperature, "℃"),
                          HTML))) +
    geom_line() +
    labs(y = "温度")
  
  p_temp <-
    ggplotly(p, dynamicTicks = T, tooltip = "text") %>% 
    layout(
      xaxis = list(type = 'date',
                   tickformat = "%Y-%m-%d"),
      yaxis = list(ticksuffix = "℃")
    )
  
  return(p_temp)
}

# humidity ------------------------------------------------

plot_humidity <- function(data) {
  
  p <- 
    data %>% 
    ggplot(aes(Date, humidity,
               text = map(paste('<b> 日時:</b>', month(Date), "月", day(Date), "日", hour(Date), "時", minute(Date), "分", '<br>',
                                '<b>相対湿度:</b>', humidity, "%"),
                          HTML))) +
    geom_line() +
    labs(y = "湿度")
  
  p_humid <- ggplotly(p, dynamicTicks = T, tooltip = "text") %>% 
    layout(
      xaxis = list(type = 'date',
                   tickformat = "%Y-%m-%d"),
      yaxis = list(ticksuffix = "%")
    )
  
  return(p_humid)
}

# abs humidity ------------------------------------------------------------

plot_abs_humidity <- function(data) {

  p_temp <- data %>% plot_temperature()

  plot_d <-
    data %>%
    select(Date, "飽和水蒸気量" = max_humidity, "絶対湿度" = abs_humidity, humidity) %>% 
    pivot_longer(-c(Date, humidity)) %>% 
    mutate(value = round(value, 1)) %>% 
    mutate(name = factor(name, levels = c("飽和水蒸気量", "絶対湿度")))
  
  
  p <- 
    plot_d %>% 
    ggplot(aes(Date, value, color = name, fill = name, 
               text = map(paste('<b> 日時:</b>', Date, '<br>',
                                '<b>', name, ':</b>', value, "g", '<br>',
                                '<b>相対湿度:</b>', humidity, "%"),
                          HTML))) +
    geom_area(position = "identity", alpha = 0.8) +
    # geom_line(alpha = 0.8) +
    scale_fill_manual(values =  c("powderblue", "deepskyblue")) +
    scale_color_manual(values =  c("powderblue", "deepskyblue")) +
    labs(y = "水蒸気質量") +
    theme(legend.title = element_blank())
  
  p_abs_humid <- 
    plotly::ggplotly(p, dynamicTicks = T, tooltip = "text") %>%
    layout(
      xaxis = list(type = 'date',
                   tickformat = "%m/%d %H時"),
      yaxis = list(ticksuffix = "g")
    )
  
  return(p_abs_humid)
}


# temp humid abs humid ----------------------------------------------------


plot_temp_humid_abshumid <- function(data){
  p_temp <- data %>% plot_temperature()
  p_humid <- data %>% plot_humidity()
  p_abs_humidity <- data %>% plot_abs_humidity()
  
  p <- subplot(p_temp, p_humid, p_abs_humidity, nrows = 3, 
               shareX = TRUE, titleY = TRUE) %>% 
    layout(legend = list(title = "",x = 0.83, y = 0.17))
  return(p)
}


# pyschrometric -----------------------------------------------------------

plot_pyschrometric <- function(data) {
  d <- 
    data %>% 
    mutate(max_humidity = max_humid(temperature)) %>% 
    mutate(abs_humidity = max_humidity / 100 * humidity) 
  
  conf_tib_w <- 
    tibble(temperature = c(21.5, 26.2, 24.0, 19.5, 21.5),
           abs_humidity = c(0, 0, 12, 12, 0))  
  
  conf_tib_s <- 
    tibble(temperature = c(25, 28.2, 26.8, 23.6, 25),
           abs_humidity = c(0, 0, 12, 12, 0))  
  
  p <- d %>% 
    mutate(m = month(Date)) %>% 
    mutate(m = factor(m)) %>% 
    ggplot(aes(x = temperature, y = abs_humidity)) +
    geom_hline(yintercept = 7, color = "blue") +
    geom_polygon(data = conf_tib_s, alpha = 0.7, fill = "palegreen1",
                 aes(temperature, abs_humidity)) +
    geom_polygon(data = conf_tib_w, alpha = 0.7, fill = "palegreen1",
                 aes(temperature, abs_humidity)) +
    geom_point(size = 0.7, alpha = 0.7, aes(color = m)) +
    scale_y_continuous(position = "right", limits = c(0, 30)) +
    scale_x_continuous(limits = c(10, 30)) +
    stat_function(fun = max_humid) +
    labs(x = "気温(℃)", y = "絶対湿度(g)",
         title = "湿り空気線図") +
    theme(text = element_text(size = 22)) +
    scale_colour_discrete("月") +
    guides(colour = guide_legend(override.aes = list(size=4))) +
    theme_gray(base_family = "IPAexGothic")
  
  
  
  p2 <- p +
    stat_function(fun = max_humid) 
  
  return(p2) 
  
}
