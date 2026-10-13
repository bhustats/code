
# Problem 2.2: CO2 Time Series Analysis
# Monthly CO2 data from 1959 to 1997

# 1. Read the data
data <- read.csv(file.choose())

# Arrange the months in the correct order
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Convert the data into a monthly time series
co2 <- ts(as.vector(t(data[, months])),
          start = c(min(data$Year), 1),
          frequency = 12)

# Display the time series
print(co2)

# --------------------------------------------------
# 1. Plot the time series and identify the trend
# --------------------------------------------------

plot(co2,
     main = "Monthly Atmospheric CO2 Concentration",
     xlab = "Year",
     ylab = "CO2 (ppm)",
     col = "blue",
     lwd = 1.5)

# Observation: CO2 concentration shows an overall
# increasing trend with seasonal fluctuations.


# --------------------------------------------------
# 2. Calculate the 12-month moving average
# --------------------------------------------------

# Centered 12-month moving average
ma12 <- stats::filter(
  co2,
  filter = c(0.5, rep(1, 11), 0.5) / 12,
  sides = 2
)

plot(co2, main = "CO2 Series and Moving Average",
     xlab = "Year", ylab = "CO2 (ppm)",
     col = "grey", lwd = 1)

lines(ma12, col = "red", lwd = 2)

legend("topleft",
       legend = c("Original Series", "12-month Moving Average"),
       col = c("grey", "red"),
       lty = 1, lwd = 2)

# Print the smoothed time series
print(ma12)


# --------------------------------------------------
# 3. Obtain detrended data and plot the residuals
# --------------------------------------------------

detrended <- co2 - ma12

plot(detrended,
     main = "Detrended CO2 Data",
     xlab = "Year",
     ylab = "Detrended Values",
     col = "blue")

abline(h = 0, col = "red", lty = 2)

# Display detrended values
print(detrended)


# --------------------------------------------------
# 4. Estimate the seasonal effects
# --------------------------------------------------

# Calculate average detrended value for each month
seasonal_effect <- tapply(
  as.numeric(detrended),
  cycle(detrended),
  mean,
  na.rm = TRUE
)

# Make the seasonal effects sum to zero
seasonal_effect <- seasonal_effect - mean(seasonal_effect)

names(seasonal_effect) <- months

cat("Estimated seasonal effects:\n")
print(seasonal_effect)

# Plot seasonal effects
barplot(seasonal_effect,
        main = "Monthly Seasonal Effects",
        xlab = "Month",
        ylab = "Seasonal Effect (ppm)",
        col = "skyblue")


# --------------------------------------------------
# 5. Obtain deseasonalized data and estimate trend
# --------------------------------------------------

# Repeat the seasonal effects for all observations
seasonal_series <- ts(
  seasonal_effect[cycle(co2)],
  start = start(co2),
  frequency = 12
)

# Remove seasonal effects
deseasonalized <- co2 - seasonal_series

plot(deseasonalized,
     main = "Deseasonalized CO2 Series",
     xlab = "Year",
     ylab = "CO2 (ppm)",
     col = "blue")

# Estimate the trend using least squares
t <- as.numeric(time(co2))

trend_model <- lm(as.numeric(deseasonalized) ~ t)

trend_est <- ts(
  fitted(trend_model),
  start = start(co2),
  frequency = 12
)

lines(trend_est, col = "red", lwd = 2)

legend("topleft",
       legend = c("Deseasonalized Data", "Estimated Trend"),
       col = c("blue", "red"),
       lty = 1, lwd = 2)

cat("Trend model:\n")
print(summary(trend_model))


# --------------------------------------------------
# 6. Obtain residuals and check stationarity
# --------------------------------------------------

residuals_co2 <- ts(
  residuals(trend_model),
  start = start(co2),
  frequency = 12
)

plot(residuals_co2,
     main = "Residuals of CO2 Time Series",
     xlab = "Year",
     ylab = "Residuals",
     col = "blue")

abline(h = 0, col = "red", lty = 2)

# ACF plot to check autocorrelation
acf(residuals_co2,
    main = "ACF of Residuals")

# Optional stationarity tests:
# Run install.packages("tseries") once if required.
if (requireNamespace("tseries", quietly = TRUE)) {
  cat("Augmented Dickey-Fuller Test:\n")
  print(tseries::adf.test(na.omit(residuals_co2)))
  
  cat("KPSS Stationarity Test:\n")
  print(tseries::kpss.test(na.omit(residuals_co2)))
} else {
  cat("Install tseries for ADF and KPSS tests:\n")
  cat('install.packages("tseries")\n')
}


# --------------------------------------------------
# 7. Draw the histogram of residuals
# --------------------------------------------------

hist(as.numeric(residuals_co2),
     main = "Histogram of Residuals",
     xlab = "Residuals",
     col = "lightblue",
     border = "black",
     breaks = 20)

# Add a normal curve for comparison
x <- seq(min(residuals_co2, na.rm = TRUE),
         max(residuals_co2, na.rm = TRUE),
         length.out = 200)

y <- dnorm(x,
           mean = mean(residuals_co2, na.rm = TRUE),
           sd = sd(residuals_co2, na.rm = TRUE))

lines(x, y * length(na.omit(residuals_co2)) *
        diff(hist(residuals_co2, plot = FALSE)$breaks)[1],
      col = "red", lwd = 2)


# --------------------------------------------------
# 8. Check normality and white noise characteristics
# --------------------------------------------------

# Normal Q-Q plot
qqnorm(residuals_co2,
       main = "Normal Q-Q Plot of Residuals")

qqline(residuals_co2, col = "red", lwd = 2)

# Shapiro-Wilk normality test
cat("Shapiro-Wilk Normality Test:\n")
print(shapiro.test(as.numeric(residuals_co2)))

# Ljung-Box test for autocorrelation
cat("Ljung-Box Test:\n")
print(Box.test(residuals_co2,
               lag = 24,
               type = "Ljung-Box"))

# Check residual mean and standard deviation
cat("Mean of residuals:\n")
print(mean(residuals_co2))

cat("Standard deviation of residuals:\n")
print(sd(residuals_co2))


# --------------------------------------------------
# 9. Forecast the next 24 monthly observations
# --------------------------------------------------

# Forecast future trend using the fitted linear model
future_time <- seq(
  from = tail(t, 1) + 1 / 12,
  by = 1 / 12,
  length.out = 24
)

future_trend <- predict(
  trend_model,
  newdata = data.frame(t = future_time)
)

# Future months: January 1998 to December 1999
future_month <- rep(1:12, length.out = 24)

# Assuming future residuals have expected value zero
forecast_co2 <- future_trend + seasonal_effect[future_month]

# Prepare the forecast table
forecast_year <- rep(1998:1999, each = 12)

forecast_month <- rep(months, 2)

forecast_table <- data.frame(
  Year = forecast_year,
  Month = forecast_month,
  Forecast_CO2 = round(as.numeric(forecast_co2), 3)
)

cat("Forecast for the next 24 months:\n")
print(forecast_table)

# Plot observed data and forecasts
plot(co2,
     xlim = c(1959, 2000),
     ylim = range(c(co2, forecast_co2), na.rm = TRUE),
     main = "CO2 Time Series and 24-Month Forecast",
     xlab = "Year",
     ylab = "CO2 (ppm)",
     col = "blue",
     lwd = 1.5)

lines(ts(forecast_co2,
         start = c(1998, 1),
         frequency = 12),
      col = "red", lwd = 2)

legend("topleft",
       legend = c("Observed CO2", "Forecast"),
       col = c("blue", "red"),
       lty = 1, lwd = 2)
