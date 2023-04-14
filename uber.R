library(tidyverse)
library(leaflet)
library(dplyr)
library(lubridate)
library(ggplot2)
library(viridis)
library(shiny)
rm(list = ls())

apr <- read.csv('data/uber-raw-data-apr14.csv')
may <- read.csv('data/uber-raw-data-may14.csv')
jun <- read.csv('data/uber-raw-data-jun14.csv')
jul <- read.csv('data/uber-raw-data-jul14.csv')
aug <- read.csv('data/uber-raw-data-aug14.csv')
sep <- read.csv('data/uber-raw-data-sep14.csv')

# Bind data
df <- bind_rows(apr, may, jun, jul, aug, sep)
  
#clean and standardize datetime values
df$Date.Time <- ifelse(grepl("/", df$Date.Time), 
                           format(strptime(df$Date.Time, format = "%m/%d/%Y %H:%M:%S"), "%Y-%m-%d %H:%M:%S"),
                           df$Date.Time)

#convert char to date and time column
df$date_time <- parse_date_time(df$Date.Time, orders = c("ymd HMS", "mdy HMS"), tz = "UTC")
df$Date.Time <- NULL

# Create hour column
df$hour <- hour(df$date_time)
df$month <- month(df$date_time)
df$weekday <- weekdays(df$date_time)
df$day_of_month <- day(df$date_time)
df$week <- week(df$date_time)

# UI
ui <- fluidPage(
  titlePanel("Uber Trips Analysis"),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Hourly Trips Table", tableOutput("hourly_pivot")),
        tabPanel("Monthly Trips", plotOutput("monthly_plot")),
        tabPanel("Hourly Trips Plot", plotOutput("hourly_plot")),
        tabPanel("Daily Trips Table", tableOutput("daily_pivot")),
        tabPanel("Daily Trips Plot", plotOutput("daily_plot")),
        tabPanel("Trips by Weekday & Month", plotOutput("weekday_month_plot")),
        tabPanel("Trips by Base & Month", plotOutput("base_month_plot")),
        tabPanel("Heatmap of Hour & Day", plotOutput("hour_day_heatmap")),
        tabPanel("Heatmap of Month & Day", plotOutput("month_day_heatmap")),
        tabPanel("Heatmap of Month & Week", plotOutput("month_week_heatmap")),
        tabPanel("Heatmap of Base & Weekday", plotOutput("base_weekday_heatmap"))
      )
    )
  )

#Server
server <- function(input, output) {

  ## Pivot table of trips by the hour
  output$hourly_pivot <- renderTable({
    hourly_counts <- df %>%
      group_by(hour) %>%
      summarise(count = n())
  })

  ## Chart of trips by hour and month
  #group and count instances by month and hour
  output$monthly_plot <- renderPlot({
    monthly_hourly_counts <- df %>%
      group_by(month, hour) %>%
      summarise(count = n())

    #convert month and hour to factors for proper ordering in the chart
    monthly_hourly_counts$month <- factor(monthly_hourly_counts$month)
    monthly_hourly_counts$hour <- factor(monthly_hourly_counts$hour)

    #create a bar chart of instances by month and hour
    ggplot(monthly_hourly_counts, aes(x = month, y = count, fill = hour)) +
      geom_col(position = "dodge", width = 0.7) +
      labs(x = "Month", y = "Count", fill = "Hour") +
      ggtitle("Trips by Month & Hour") +
      theme_minimal()
  })
  
  ## Chart of trips every hour
  output$hourly_plot <- renderPlot({
    ggplot(hourly_counts, aes(x = hour, y = count, fill = viridis(24))) +
      geom_col(position = "dodge", width = 0.7, show.legend = FALSE) +
      labs(x = "Hour", y = "Count") +
      ggtitle("Trips by Hour") +
      theme_light() +
      scale_x_continuous(n.breaks = 19) +
      scale_y_continuous(n.breaks = 8)
  })


  ## Table of trips every day (total for every day (max 31 days))
  output$daily_pivot <- renderTable({
    day_of_month_counts <- df %>%
      group_by(day_of_month) %>%
      summarise(count = n())
  })
    
  ## Plot data of trips taken during every day of month
  output$daily_plot <- renderPlot({  
    ggplot(day_of_month_counts, aes(x = day_of_month, y = count, fill = count)) +
      geom_col(position = "dodge", width = 0.7) +
      labs(x = "Day of Month", y = "Count") +
      ggtitle("Trips by Day of Month") +
      theme_minimal() +
      scale_fill_viridis()
  })
    
## Chart of trips by day and month (bar chart with each day of the week,
#x axis as the month). Chart that shows number of trips by month
  output$weekday_month_plot <- renderPlot({
    weekday_month_counts <- df %>%
      group_by(weekday, month) %>%
      summarise(count = n())

    weekday_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    weekday_month_counts$weekday <- factor(weekday_month_counts$weekday, levels = weekday_order)

    ggplot(weekday_month_counts, aes(x = month, y = count, fill = weekday)) +
      geom_bar(position = "stack", width = 0.7, stat = "identity") +
      labs(x = "Month", y = "Count") +
      ggtitle("Trips by Weekday & Month") +
      theme_minimal() 
  })
  
  ## Chart Trips by Bases and Month (Base is the X axis and Month is your label)
  output$base_month_plot <- renderPlot({  
    base_month_counts <- df %>%
      group_by(Base, month) %>%
      summarise(count = n())

    ggplot(base_month_counts, aes(x = Base, y = count, fill = month)) +
      geom_bar(position = "stack", width = 0.7, stat = "identity") +
      labs(x = "Base", y = "Count") +
      ggtitle("Trips by Base & Month") +
      scale_fill_viridis() + theme_minimal()
  })
    
  ## Heat map by hour and day
  output$hour_day_heatmap <- renderPlot({
    hour_day_counts <- df %>%
      group_by(hour, day_of_month) %>%
      summarise(count = n())

    ggplot(hour_day_counts, aes(x = hour, y = day_of_month)) +
      geom_tile(aes(fill = count)) +
      labs(x = "Hour", y = "Day") +
      ggtitle("Heatmap of Hour & Day") +
      scale_fill_viridis(option = "H") + theme_minimal()
  })
    
  ## Heat map by month and day
  output$month_day_heatmap <- renderPlot({
    month_day_counts <- df %>%
      group_by(month, day_of_month) %>%
      summarise(count = n())

    ggplot(month_day_counts, aes(x = month, y = day_of_month)) +
      geom_tile(aes(fill = count)) +
      labs(x = "Month", y = "Day") +
      ggtitle("Heatmap of Month & Day") +
      scale_fill_viridis(option = "H") + theme_minimal()
  })
    
  ## Heat map by month and week
  output$month_week_heatmap <- renderPlot({  
    month_week_counts <- df %>%
      group_by(month, week) %>%
      summarise(count = n())

    ggplot(month_week_counts, aes(x = month, y = week)) +
      geom_tile(aes(fill = count)) +
      labs(x = "Month", y = "Week") +
      ggtitle("Heatmap of Month & Week") +
      scale_fill_viridis(option = "H") + theme_minimal()
  })
  
  ## Heat map Bases and Day of Week
  output$base_weekday_heatmap <- renderPlot({  
    base_weekday_counts <- df %>%
      group_by(Base, weekday) %>%
      summarise(count = n())

    ggplot(base_weekday_counts, aes(x = Base, y = weekday)) +
      geom_tile(aes(fill = count)) +
      labs(x = "Base", y = "Weekday") +
      ggtitle("Heatmap of Base & Week") +
      scale_fill_viridis(option = "H") + theme_minimal()
  })
}  

shinyApp(ui, server)
