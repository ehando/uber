# Uber Trips Analysis :car:
*Analyzing Uber trips from April to September.*
---
## Libraries :book:
```
library(tidyverse)
library(leaflet)
library(dplyr)
library(lubridate)
library(ggplot2)
library(viridis)
library(shiny)
```
---
## Data Prep

Read csv files, bind rows to one dataframe, and add columns for hour, month, weekday, day of the month, and week
```
files <- c("data/uber-raw-data-apr14.csv",
           "data/uber-raw-data-may14.csv",
           "data/uber-raw-data-jun14.csv",
           "data/uber-raw-data-jul14.csv",
           "data/uber-raw-data-aug14.csv",
           "data/uber-raw-data-sep14.csv")

df <- bind_rows(lapply(files, read.csv)) %>%
  mutate(Date.Time = ifelse(grepl("/", Date.Time), 
                            format(strptime(Date.Time, format = "%m/%d/%Y %H:%M:%S"), 
                                   "%Y-%m-%d %H:%M:%S"),
                            Date.Time),
         date_time = parse_date_time(Date.Time, orders = c("ymd HMS", "mdy HMS"),
                                     tz = "UTC"),
         hour = hour(date_time),
         month = month(date_time),
         weekday = weekdays(date_time),
         day_of_month = day(date_time),
         week = week(date_time)) %>%
  select(-Date.Time)

```
---
## Analysis
This is only a snippet from my Shiny UI. I made a new tab for each pivot table and plot, but code is similar for each tab
```
ui <- fluidPage(
  titlePanel("Uber Trips Analysis"),
    
    mainPanel(
      tabsetPanel(
        tabPanel("tab1_name", tableOutput("table1_name")),
        tabPanel("tab2_name, plotOutput("plot1_name))
        )))
```
Pivot table to display trips by the hour
```
  output$hourly_pivot <- renderTable({
    hourly_counts <- df %>%
      group_by(hour) %>%
      summarise(count = n())
  })
```
Chart that shows trips by month and hour
```
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
```
Chart that displays trips every hour
```
  output$hourly_plot <- renderPlot({
    hourly_counts <- df %>%
      group_by(hour) %>%
      summarise(count = n())
    
    ggplot(hourly_counts, aes(x = hour, y = count, fill = viridis(24))) +
      geom_col(position = "dodge", width = 0.7, show.legend = FALSE) +
      labs(x = "Hour", y = "Count") +
      ggtitle("Trips by Hour") +
      theme_light() +
      scale_x_continuous(n.breaks = 19) +
      scale_y_continuous(n.breaks = 8)
  })
```
Plot of trips taken every day of the month
```
  output$daily_plot <- renderPlot({
    day_of_month_counts <- df %>%
      group_by(day_of_month) %>%
      summarise(count = n())
    
    ggplot(day_of_month_counts, aes(x = day_of_month, y = count, fill = count)) +
      geom_col(position = "dodge", width = 0.7) +
      labs(x = "Day of Month", y = "Count") +
      ggtitle("Trips by Day of Month") +
      theme_minimal() +
      scale_fill_viridis()
  })
```
Pivot table of trips taken every day of the month
```
  output$daily_pivot <- renderTable({
    day_of_month_counts <- df %>%
      group_by(day_of_month) %>%
      summarise(count = n())
  })
```
Chart of trips by day and month
```
  output$weekday_month_plot <- renderPlot({
    weekday_month_counts <- df %>%
      group_by(weekday, month) %>%
      summarise(count = n())

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
```
Chart of trips by base and month
```
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
```
Heatmap by hour and day
```
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
```
Heatmap by month and day
```
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
```
Heatmap by month and week
```
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
```
Heatmap by base and day of the week
```
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
```
Leaflet map using a subset of 1,500 random rows in dataframe. 4.5 million points in original df, so had to subset to be able to publish on shinyapps.io with free account
```
 df1 <- dplyr::sample_n(df, 1500)
```
```
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
    addMarkers(data = df1, lat = ~Lat, lng = ~Lon, popup = ~Base)
  })
```
