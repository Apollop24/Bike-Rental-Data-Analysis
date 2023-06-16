# Load libraries
library(tidyverse)
library(lubridate)
library(TTR)
library(forecast)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(apaTables)
install.packages("devtools")  # Install devtools package
devtools::install_github("crsh/papaja")  # Install papaja package
library(papaja)

# Read data
bike_rental <- read_excel("~/day.xlsx")

# Convert date column to a date format
bike_rental$dteday <- as.Date(bike_rental$dteday, format = "%d/%m/%Y")

# Perform exploratory data analysis
str(bike_rental)
summary(bike_rental)


# Distribution of Total Number of Bike Rentals

# Histogram of count variable
ggplot(bike_rental, aes(x=cnt)) +
  geom_histogram(binwidth = 100, fill="dodgerblue4", color="white") +
  xlab("Count of Total Rental Bikes") +
  ylab("Frequency") +
  ggtitle("Distribution of Total Number of Bike Rentals ") +
  theme(plot.title = element_text(family = "Times New Roman", size = 12))
# Distribution of Total Number of Bike Rentals by Season
ggplot(bike_rental, aes(x = cnt, fill = factor(season))) +
  geom_histogram(binwidth = 100, color = "#e9ecef", alpha = 0.8, position = "identity") +
  facet_wrap(~ season, ncol = 2) +
  scale_fill_manual(values = c("#619CFF", "#00EAD3", "#FF7F50", "#BDBDBD")) +
  labs(title = "Distribution of Total Number of Bike Rentals by Season",
       x = "Total Number of Bike Rentals",
       y = "Count") +
  theme(text = element_text(family = "Times New Roman", size = 12))


#Count of Bike Rentals by Weekday and Working Day
# Set font to Times New Roman
theme_set(theme_bw(base_family = "Times New Roman"))


# Create plot
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

# Visualize count of bike rentals by temperature, humidity, and wind speed

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




## Visualize count of bike rentals over time


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
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

ggplotly()


# Compare linear regression models against multiple time series models

## Simple linear regression model
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




## Multiple time series models

### Seasonal decomposition of time series

bike_rental_ts <- bike_rental %>%
  select(dteday, cnt) %>%
  mutate(dteday = ymd(dteday)) %>%
  as.ts(index = dteday, frequency = 1)

plot(bike_rental_ts, main = "Count of Bike Rentals over Time")


### Autoregressive integrated moving average (ARIMA) model


# Convert to time series object
bike_rental_ts <- bike_rental %>%
  select(dteday, cnt) %>%
  mutate(dteday = ymd(dteday)) %>%
  select(cnt) %>%
  as.ts()

# Fit ARIMA model
arima_model <- auto.arima(bike_rental_ts, seasonal = TRUE)
summary(arima_model)





### Exponential smoothing state space model with Box-Cox transformation and ARMA errors (ETS)
ets_model <- ets(bike_rental_ts, model = "ZZZ")
summary(ets_model)


