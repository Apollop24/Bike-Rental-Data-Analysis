
# Bike Rental Data Analysis

This repository contains scripts for analyzing bike rental data. The analysis covers various aspects of the dataset including exploratory data analysis, visualization, and model comparison. The dataset is sourced from Kaggle, detailing bike rentals in Washington D.C. for the years 2011 and 2012.
<img width="1422" height="683" alt="image" src="https://github.com/user-attachments/assets/04039f2f-ede1-4dd4-9425-35347d5762f0" />

## Table of Contents
1. [Introduction](#introduction)
2. [Data Source](#data-source)
3. [Installation](#installation)
4. [Data Preparation](#data-preparation)
5. [Attribute Information](#attribute-information)
6. [Exploratory Data Analysis](#exploratory-data-analysis)
7. [Visualizations](#visualizations)
8. [Model Comparison](#model-comparison)
9. [Conclusion](#conclusion)
10. [License](#license)

## Introduction

Bike-sharing systems are modern automated systems for renting bicycles. These systems facilitate easy bike rentals and returns, offering a convenient transportation option. The data in this project is aggregated from daily logs and includes various environmental and seasonal factors affecting bike rentals.

## Data Source

The dataset is obtained from [Kaggle](https://www.kaggle.com/datasets/archit9406/bike-sharing) and contains bike-sharing counts aggregated on a daily basis. The dataset includes two years of data (2011 and 2012) from the Capital Bikeshare system in Washington D.C., USA.

**Citation:**
Fanaee-T, Hadi, and Gama, Joao, "Event labeling combining ensemble detectors and background knowledge", Progress in Artificial Intelligence (2013): pp. 1-15, Springer Berlin Heidelberg, doi:10.1007/s13748-013-0040-3.

## Installation

Ensure you have the following R packages installed:
```r
install.packages(c("tidyverse", "lubridate", "TTR", "forecast", "readxl", "dplyr", "ggplot2", "plotly", "apaTables", "devtools"))
devtools::install_github("crsh/papaja")
```

## Data Preparation

Load the required libraries and read the data from the Excel file:
```r
library(tidyverse)
library(lubridate)
library(TTR)
library(forecast)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(apaTables)
library(papaja)

# Read data
bike_rental <- read_excel("~/day.xlsx")

# Convert date column to a date format
bike_rental$dteday <- as.Date(bike_rental$dteday, format = "%d/%m/%Y")
```

## Attribute Information

- **instant**: Record index
- **dteday**: Date
- **season**: Season (1: spring, 2: summer, 3: fall, 4: winter)
- **yr**: Year (0: 2011, 1: 2012)
- **mnth**: Month (1 to 12)
- **holiday**: Whether the day is a holiday or not (extracted from http://dchr.dc.gov/page/holiday-schedule)
- **weekday**: Day of the week
- **workingday**: If the day is neither weekend nor holiday is 1, otherwise is 0
- **weathersit**: Weather situation:
  - 1: Clear, Few clouds, Partly cloudy, Partly cloudy
  - 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
  - 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
  - 4: Heavy Rain + Ice Pellets + Thunderstorm + Mist, Snow + Fog
- **temp**: Normalized temperature in Celsius. The values are divided to 41 (max)
- **atemp**: Normalized feeling temperature in Celsius. The values are divided to 50 (max)
- **hum**: Normalized humidity. The values are divided to 100 (max)
- **windspeed**: Normalized wind speed. The values are divided to 67 (max)
- **casual**: Count of casual users
- **registered**: Count of registered users
- **cnt**: Count of total rental bikes (casual + registered)

## Exploratory Data Analysis

Perform initial exploration of the dataset:
```r
# Display structure and summary of the data
str(bike_rental)
summary(bike_rental)
```



## Visualizations

### Distribution of Total Number of Bike Rentals

Histogram showing the distribution of total bike rentals:
```r
ggplot(bike_rental, aes(x=cnt)) +
  geom_histogram(binwidth = 100, fill="dodgerblue4", color="white") +
  xlab("Count of Total Rental Bikes") +
  ylab("Frequency") +
  ggtitle("Distribution of Total Number of Bike Rentals ") +
  theme(plot.title = element_text(family = "Times New Roman", size = 12))
```

### Distribution by Season

Histogram showing bike rentals by season:
```r
ggplot(bike_rental, aes(x = cnt, fill = factor(season))) +
  geom_histogram(binwidth = 100, color = "#e9ecef", alpha = 0.8, position = "identity") +
  facet_wrap(~ season, ncol = 2) +
  scale_fill_manual(values = c("#619CFF", "#00EAD3", "#FF7F50", "#BDBDBD")) +
  labs(title = "Distribution of Total Number of Bike Rentals by Season",
       x = "Total Number of Bike Rentals",
       y = "Count") +
  theme(text = element_text(family = "Times New Roman", size = 12))
```

### Count of Bike Rentals by Weekday and Working Day

Boxplot showing bike rentals by weekday and working day:
```r
theme_set(theme_bw(base_family = "Times New Roman"))

p <- ggplot(bike_rental, aes(x = factor(weekday), y = cnt, fill = factor(workingday))) +
  geom_boxplot() +
  scale_fill_discrete(name = "Working Day",
                      labels = c("Non-Working Day", "Working Day")) +
  xlab("Weekday") +
  ylab("Count of Bike Rentals") +
  ggtitle("Count of Bike Rentals by Weekday and Working Day")

# Convert plot to plotly object
p <- ggplotly(p)

# Customize plot appearance
p <- p %>%
  layout(
    title = list(
      text = "Count of Bike Rentals by Weekday and Working Day",
      font = list(family = "Times New Roman")
    ),
    xaxis = list(
      title = list(
        text = "Weekday",
        font = list(family = "Times New Roman")
      ),
      tickfont = list(family = "Times New Roman")
    ),
    yaxis = list(
      title = list(
        text = "Count of Bike Rentals",
        font = list(family = "Times New Roman")
      ),
      tickfont = list(family = "Times New Roman")
    ),
    legend = list(
      title = list(
        text = "Working Day",
        font = list(family = "Times New Roman")
      ),
      font = list(family = "Times New Roman")
    )
  )

# Show plot
p
```

### Count of Bike Rentals by Temperature, Humidity, and Wind Speed

Scatter plots with regression lines:
```r
p1 <- bike_rental %>%
  ggplot(aes(x = temp, y = cnt)) +
  geom_point(aes(color = hum)) +
  scale_color_gradient(low = "blue", high = "red", name = "Humidity") +
  geom_smooth(method = "lm", se = TRUE, level = 0.95) +
  xlab("Temperature") +
  ylab("Count of Bike Rentals") +
  ggtitle("Count of Bike Rentals by Temperature and Humidity") +
  theme(text = element_text(family = "Times New Roman"))

p2 <- bike_rental %>%
  ggplot(aes(x = windspeed, y = cnt)) +
  geom_point(aes(color = hum)) +
  scale_color_gradient(low = "blue", high = "red", name = "Humidity") +
  geom_smooth(method = "lm", se = TRUE, level = 0.95) +
  xlab("Wind Speed") +
  ylab("Count of Bike Rentals") +
  ggtitle("Count of Bike Rentals by Wind Speed and Humidity") +
  theme(text = element_text(family = "Times New Roman"))

ggplotly(p1) %>%
  layout(title = list(text = "Count of Bike Rentals by Temperature and Humidity", font = list(family = "Times New Roman")),
         xaxis = list(title = list(text = "Temperature", font = list(family = "Times New Roman"))),
         yaxis = list(title = list(text = "Count of Bike Rentals", font = list(family = "Times New Roman"))),
         legend = list(title = list(text = "Humidity", font = list(family = "Times New Roman"))),
         font = list(family = "Times New Roman"))

ggplotly(p2) %>%
  layout(title = list(text = "Count of Bike Rentals by Wind Speed and Humidity", font = list(family = "Times New Roman")),
         xaxis = list(title = list(text = "Wind Speed", font = list(family = "Times New Roman"))),
         yaxis = list(title = list(text = "Count of Bike Rentals", font = list(family = "Times New Roman"))),
         legend = list(title = list(text = "Humidity", font = list(family = "Times New Roman"))),
         font = list(family = "Times New Roman"))
```

### Count of Bike Rentals over Time

Line plot showing bike rentals over time:
```r
bike_rental %>%
  mutate(dteday = as.Date(dteday)) %>%
  ggplot(aes(x = dteday, y = cnt)) +
  geom_line(color = "#008080") +
  xlab("Date") +
  ylab("Count of Bike Rentals") +
  ggtitle("Count of Bike Rentals over Time") +
  theme(text = element_text(family = "Times New Roman")) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 month") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  guides(fill = guide_legend(title = "Working Day",
                             labels = c("Non-Working Day", "Working Day"),
                             title.position = "top")) +
  scale_color_gradient(low = "#9BC4E2", high = "#F3A712") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm

"))

ggplotly()
```

## Model Comparison

### Linear Regression Model

```r
library(stargazer)

stargazer(lm_model, 
          type = "text", 
          title = "Linear Regression Model for Bike Rentals",
          align = TRUE,
          covariate.labels = c("Temperature", "Humidity", "Wind Speed"),
          dep.var.caption = "Dependent Variable: Count of Bike Rentals",
          dep.var.labels.include = FALSE,
          column.labels = c("Intercept", "Temperature", "Humidity", "Wind Speed"),
          out = "lm_table.txt")
```

### Time Series Models

#### Seasonal Decomposition
```r
bike_rental_ts <- bike_rental %>%
  select(dteday, cnt) %>%
  mutate(dteday = ymd(dteday)) %>%
  as.ts(index = dteday, frequency = 1)

plot(bike_rental_ts, main = "Count of Bike Rentals over Time")
```

#### ARIMA Model
```r
bike_rental_ts <- bike_rental %>%
  select(dteday, cnt) %>%
  mutate(dteday = ymd(dteday)) %>%
  select(cnt) %>%
  as.ts()

# Fit ARIMA model
arima_model <- auto.arima(bike_rental_ts, seasonal = TRUE)
summary(arima_model)
```

#### Exponential smoothing state space model with Box-Cox transformation and ARMA errors (ETS)
```r
ets_model <- ets(bike_rental_ts, model = "ZZZ")
summary(ets_model)
```

## Conclusion

This project provides a comprehensive analysis of bike rental data, including visualizations and model comparisons. The results offer insights into the patterns and factors influencing bike rentals in Washington D.C.

## License

Use of this dataset in publications must be cited as follows:

Fanaee-T, Hadi, and Gama, Joao, "Event labeling combining ensemble detectors and background knowledge", Progress in Artificial Intelligence (2013): pp. 1-15, Springer Berlin Heidelberg, doi:10.1007/s13748-013-0040-3.
```

Feel free to adjust the file paths, installation commands, or any other details as necessary for your specific environment and setup.






