library(tidyverse)
library(leaflet)
library(dplyr)
library(lubridate)
library(ggplot2)
library(viridis)
library(shiny)
library(modelr)
rm(list = ls())

#setwd("C:/Users/Eirik/OneDrive/College/Senior/Data 332/uber")

base_month_counts <- read.csv('pivot_files/base_month_counts.csv')
base_weekday_counts <- read.csv('pivot_files/base_weekday_counts.csv')
day_of_month_counts <- read.csv('pivot_files/day_of_month_counts.csv')
hour_day_counts <- read.csv('pivot_files/hour_day_counts.csv')
hourly_counts <- read.csv('pivot_files/hourly_counts.csv')
month_day_counts <- read.csv('pivot_files/month_day_counts.csv')
month_week_counts <- read.csv('pivot_files/month_week_counts.csv')
monthly_hourly_counts <- read.csv('pivot_files/monthly_hourly_counts.csv')
weekday_month_counts <- read.csv('pivot_files/weekday_month_counts.csv')
map <- read.csv('pivot_files/leaflet_map.csv')
daily <- read.csv('pivot_files/daily.csv')

# UI
ui <- fluidPage(
  titlePanel("Uber Trips Analysis"),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Monthly Trips", plotOutput("monthly_plot")),
        tabPanel("Hourly Trips",
                 fluidRow(
                   column(12, plotOutput("hourly_plot")),
                   column(2, tableOutput("hourly_pivot")))
                 ),
        tabPanel("Daily Trips",
                 fluidRow(
                   column(12, plotOutput("daily_plot")),
                   column(2, tableOutput("daily_pivot")))
                ),
        tabPanel("Trips by Weekday & Month", plotOutput("weekday_month_plot")),
        tabPanel("Trips by Base & Month", plotOutput("base_month_plot")),
        tabPanel("Heatmap of Hour & Day", plotOutput("hour_day_heatmap")),
        tabPanel("Heatmap of Month & Day", plotOutput("month_day_heatmap")),
        tabPanel("Heatmap of Month & Week", plotOutput("month_week_heatmap")),
        tabPanel("Heatmap of Base & Weekday", plotOutput("base_weekday_heatmap")),
        tabPanel("Leaflet Map", leafletOutput("map")),
        tabPanel("Model",
                 fluidRow(
                   column(12, plotOutput("mod1")),
                   column(12, plotOutput("mod2")))
                )
      )
    )
  )

#Server
server <- function(input, output) {

  ## Pivot table of trips by the hour
  output$hourly_pivot <- renderTable({
    hourly_counts})

  ## Chart of trips by hour and month
  #group and count instances by month and hour
  output$monthly_plot <- renderPlot({
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
    day_of_month_counts})
    
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
    weekday_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                       "Saturday", "Sunday")
    weekday_month_counts$weekday <- factor(weekday_month_counts$weekday,
                                           levels = weekday_order)

    ggplot(weekday_month_counts, aes(x = month, y = count, fill = weekday)) +
      geom_bar(position = "stack", width = 0.7, stat = "identity") +
      labs(x = "Month", y = "Count") +
      ggtitle("Trips by Weekday & Month") +
      theme_minimal() 
  })
  
  ## Chart Trips by Bases and Month (Base is the X axis and Month is your label)
  output$base_month_plot <- renderPlot({  
    ggplot(base_month_counts, aes(x = Base, y = count, fill = month)) +
      geom_bar(position = "stack", width = 0.7, stat = "identity") +
      labs(x = "Base", y = "Count") +
      ggtitle("Trips by Base & Month") +
      scale_fill_viridis() + theme_minimal()
  })
    
  ## Heat map by hour and day
  output$hour_day_heatmap <- renderPlot({
   ggplot(hour_day_counts, aes(x = hour, y = day_of_month)) +
      geom_tile(aes(fill = count)) +
      labs(x = "Hour", y = "Day") +
      ggtitle("Heatmap of Hour & Day") +
      scale_fill_viridis(option = "H") + theme_minimal()
  })
    
  ## Heat map by month and day
  output$month_day_heatmap <- renderPlot({
    ggplot(month_day_counts, aes(x = month, y = day_of_month)) +
      geom_tile(aes(fill = count)) +
      labs(x = "Month", y = "Day") +
      ggtitle("Heatmap of Month & Day") +
      scale_fill_viridis(option = "H") + theme_minimal()
  })
    
  ## Heat map by month and week
  output$month_week_heatmap <- renderPlot({  
    ggplot(month_week_counts, aes(x = month, y = week)) +
      geom_tile(aes(fill = count)) +
      labs(x = "Month", y = "Week") +
      ggtitle("Heatmap of Month & Week") +
      scale_fill_viridis(option = "H") + theme_minimal()
  })
  
  ## Heat map Bases and Day of Week
  output$base_weekday_heatmap <- renderPlot({  
    ggplot(base_weekday_counts, aes(x = Base, y = weekday)) +
      geom_tile(aes(fill = count)) +
      labs(x = "Base", y = "Weekday") +
      ggtitle("Heatmap of Base & Week") +
      scale_fill_viridis(option = "H") + theme_minimal()
  })
  
  # Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
    addMarkers(data = map, lat = ~Lat, lng = ~Lon)
  })
  
  #Model
  output$mod1 <- renderPlot({
    mod <- lm(n ~ weekday, data = daily)

    daily <- daily %>%
      add_residuals(mod)
    daily %>%
      ggplot(aes(date, resid, group = 1)) +
      geom_ref_line(h = 0) +
      geom_line(color = "gray50") +
      geom_smooth(se = FALSE)
  })
  
  output$mod2 <- renderPlot({
    mod <- lm(n ~ weekday, data = daily)
    
    ggplot(daily, aes(date, resid, color = weekday, group = weekday)) +
      geom_ref_line(h = 0) +
      geom_line(size = 0.8) 
  })
}  

shinyApp(ui, server)
