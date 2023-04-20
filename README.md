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
library(modelr)
```
---
## Data Prep

- Read csv files, bind rows to one dataframe, and add columns for hour, month, weekday, day of the month, and week
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
         week = week(date_time),
         year = year(date_time)) %>%
  select(-Date.Time)
```
### Create pivot tables and write csv files
- Created a pivot table for each of the charts and wrote it into separate csv files, like this:
```
hourly_counts <- df %>%
  group_by(hour) %>%
  summarise(count = n())
write.csv(hourly_counts,
          file = "your_filepath/hourly_counts.csv",
          row.names = FALSE)
```
- Created the Shinyapp in a new R file reduce the size so I could publish it. Read in the csv files I wrote above. Example:
```
hourly_counts <- read.csv('pivot_files/hourly_counts.csv')
```
I did this for all the pivot tables.
### Two special cases: leaflet map and model
- For the leaflet map I grouped the data by Lat and Lon and found the top 20 busiest locations to plot on the map
```
leaflet_map <- df %>%
  group_by(Lat, Lon) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(20)
```
- For the model I had to convert the weekdays into factors and order them correctly. Then I made a table and wrote into a csv file like the other tables
```
df$weekday <- as.factor(df$weekday)
weekday_labels <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
df$weekday_num <- as.numeric(factor(df$weekday, levels = weekday_labels))

daily <- df %>%
  mutate(date = make_date(year, month, day_of_month)) %>%
  group_by(date, weekday) %>%
  summarise(n = n())
```
---
## Analysis
### Shinyapp UI
- I made a new tab for each chart, some tabs have a chart and a pivot table. I also made a separate tab for the leaflet map and for the model.
```
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
                ))))
```
### Charts, Pivot Tables, Map, and Models
- Made charts in the Shiny server. Most of the charts were made in similar fashion, so I will only show code for one of each different type.

- Pivot table and chart of Trips by the hour
```
output$hourly_pivot <- renderTable({
    hourly_counts})
    
 output$hourly_plot <- renderPlot({
   ggplot(hourly_counts, aes(x = hour, y = count, fill = viridis(24))) +
      geom_col(position = "dodge", width = 0.7, show.legend = FALSE) +
      labs(x = "Hour", y = "Count") +
      ggtitle("Trips by Hour") +
      theme_light() +
      scale_x_continuous(n.breaks = 19) +
      scale_y_continuous(n.breaks = 8)
  })
```
- Trips by month and hour. Had to convert month and hour to factor in order to plot correctly
```
  output$monthly_plot <- renderPlot({
    monthly_hourly_counts$month <- factor(monthly_hourly_counts$month)
    monthly_hourly_counts$hour <- factor(monthly_hourly_counts$hour)

    ggplot(monthly_hourly_counts, aes(x = month, y = count, fill = hour)) +
      geom_col(position = "dodge", width = 0.7) +
      labs(x = "Month", y = "Count", fill = "Hour") +
      ggtitle("Trips by Month & Hour") +
      theme_minimal()
  })
```
- Pivot and plot of trips taken every day of the month
```
output$daily_pivot <- renderTable({
    day_of_month_counts})
    
  output$daily_plot <- renderPlot({
    ggplot(day_of_month_counts, aes(x = day_of_month, y = count, fill = count)) +
      geom_col(position = "dodge", width = 0.7) +
      labs(x = "Day of Month", y = "Count") +
      ggtitle("Trips by Day of Month") +
      theme_minimal() +
      scale_fill_viridis()
  })
```
- Chart of trips by day and month. Had to convert weekdays to factors and order them correctly for plotting
```
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
```
- Chart of trips by base and month
```
  output$base_month_plot <- renderPlot({  
    ggplot(base_month_counts, aes(x = Base, y = count, fill = month)) +
      geom_bar(position = "stack", width = 0.7, stat = "identity") +
      labs(x = "Base", y = "Count") +
      ggtitle("Trips by Base & Month") +
      scale_fill_viridis() + theme_minimal()
  })
```
- Heatmap by hour and day
```
  output$hour_day_heatmap <- renderPlot({
   ggplot(hour_day_counts, aes(x = hour, y = day_of_month)) +
      geom_tile(aes(fill = count)) +
      labs(x = "Hour", y = "Day") +
      ggtitle("Heatmap of Hour & Day") +
      scale_fill_viridis(option = "H") + theme_minimal()
  })
```
- Heatmap by month and day
```
  output$month_day_heatmap <- renderPlot({
    ggplot(month_day_counts, aes(x = month, y = day_of_month)) +
      geom_tile(aes(fill = count)) +
      labs(x = "Month", y = "Day") +
      ggtitle("Heatmap of Month & Day") +
      scale_fill_viridis(option = "H") + theme_minimal()
  })
```
- Heatmap by month and week
```
  output$month_week_heatmap <- renderPlot({  
    ggplot(month_week_counts, aes(x = month, y = week)) +
      geom_tile(aes(fill = count)) +
      labs(x = "Month", y = "Week") +
      ggtitle("Heatmap of Month & Week") +
      scale_fill_viridis(option = "H") + theme_minimal()
  })
```
- Heatmap by base and day of the week
```
  output$base_weekday_heatmap <- renderPlot({  
    ggplot(base_weekday_counts, aes(x = Base, y = weekday)) +
      geom_tile(aes(fill = count)) +
      labs(x = "Base", y = "Weekday") +
      ggtitle("Heatmap of Base & Week") +
      scale_fill_viridis(option = "H") + theme_minimal()
  })
```
- Leaflet map showing the top 20 busiest locations in the data
```
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
    addMarkers(data = map, lat = ~Lat, lng = ~Lon)
  })
```
- For the model I made a two models showing the trend over time. One shows the overall trends and one shows the trend by weekday
```
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
```
