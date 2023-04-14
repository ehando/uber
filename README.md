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

Read csv files and bind rows to one dataframe
```
df <- bind_rows(apr, may, jun, jul, aug, sep)
```
Clean and standardize datetime values before converting the column from character to datetime format
```
df$Date.Time <- ifelse(grepl("/", df$Date.Time), 
                           format(strptime(df$Date.Time, format = "%m/%d/%Y %H:%M:%S"),
                                           "%Y-%m-%d %H:%M:%S"),
                           df$Date.Time)
```
```
df$date_time <- parse_date_time(df$Date.Time, orders = c("ymd HMS", "mdy HMS"),
                                tz = "UTC")
df$Date.Time <- NULL
```
Create columns for hour, weekday, day of the month, week, and month for later analysis
```
df$hour <- hour(df$date_time)
df$month <- month(df$date_time)
df$weekday <- weekdays(df$date_time)
df$day_of_month <- day(df$date_time)
df$week <- week(df$date_time)
```
---
## Analysis
