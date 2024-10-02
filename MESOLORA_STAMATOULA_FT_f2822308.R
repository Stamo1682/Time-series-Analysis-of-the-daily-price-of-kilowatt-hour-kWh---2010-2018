library(openxlsx)
library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)
library(tseries)
library(forecast)
library(lmtest)
library(car)
library(FinTS)
library(nortest)
library(urca)
library(reshape2)
library(qqplotr)


initial_data <- read.xlsx("C:\\Users\\fani_\\OneDrive\\Υπολογιστής\\Business_Analytics\\Spring Quarter\\Advanced Topics in Statistics\\Assignment\\electricity price data.xlsx", colNames = FALSE)

colnames(initial_data) <- c("Date_2010", "Price_2010", "Date_2011", "Price_2011", 
                            "Date_2012", "Price_2012", "Date_2013", "Price_2013", 
                            "Date_2014", "Price_2014", "Date_2015", "Price_2015", 
                            "Date_2016", "Price_2016", "Date_2017", "Price_2017", 
                            "Date_2018", "Price_2018")

initial_data$Date_2010 <- as.Date(initial_data$Date_2010, format = "%Y.%m.%d")
initial_data$Date_2011 <- as.Date(as.numeric(initial_data$Date_2011), origin = "1899-12-30")
initial_data$Date_2012 <- as.Date(as.numeric(initial_data$Date_2012), origin = "1899-12-30")
initial_data$Date_2013 <- as.Date(as.numeric(initial_data$Date_2013), origin = "1899-12-30")
initial_data$Date_2014 <- as.Date(as.numeric(initial_data$Date_2014), origin = "1899-12-30")
initial_data$Date_2015 <- as.Date(as.numeric(initial_data$Date_2015), origin = "1899-12-30")
initial_data$Date_2016 <- as.Date(as.numeric(initial_data$Date_2016), origin = "1899-12-30")
initial_data$Date_2017 <- as.Date(as.numeric(initial_data$Date_2017), origin = "1899-12-30")
initial_data$Date_2018 <- as.Date(initial_data$Date_2018, format = "%d.%m.%Y")

str(initial_data)
summary(initial_data)


data <- NULL
for (i in seq(2, ncol(initial_data), by = 2)) {
  data <- rbind(data, data.frame(Date = initial_data[, i - 1], Price = initial_data[, i]))
}

str(data)
summary(data)

data <- na.omit(data)

data_month <- data %>%
  mutate(month = floor_date(Date, "month")) %>%
  group_by(month) %>%
  summarize(price = mean(Price))

str(data_month)
summary(data_month)


ts <- ts(data_month$price, start = c(2010, 1), frequency = 12)


# Autoplot
autoplot(ts) +
  ggtitle("Monthly Electricity Prices (2010-2018)") +
  xlab("Date") +
  ylab("Price") +
  theme_minimal()  

# Seasonal Subseries plot
ggsubseriesplot(ts) +
  ylab("$ Price") +
  ggtitle("Seasonal subseries plot: Monthly Electricity Prices (2010-2018)") +
  theme_minimal()

# STL Decomposition
decomposition_stl <- stl(ts, "periodic")
seasonal_stl   <- decomposition_stl$time.series[,1] 
trend_stl     <- decomposition_stl$time.series[,2] 
random_stl  <- decomposition_stl$time.series[,3] 

plot(as.ts(seasonal_stl))
plot(trend_stl)
plot(random_stl)

decomposition_df <- data.frame(
  Date = time(ts), 
  Seasonal = seasonal_stl, 
  Trend = trend_stl, 
  Remainder = random_stl
)

decomposition_df_melt <- melt(decomposition_df, id.vars = "Date")

ggplot(decomposition_df_melt, aes(x = Date, y = value, color = variable)) + 
  geom_line() + 
  facet_grid(variable ~ ., scales = "free_y") +
  labs(title = "Decomposition STL", y = "Value", x = "Date") + 
  theme_minimal() +
  theme(legend.position = "none")


adf.test(ts)

log_ts<- log(ts)

autoplot(log_ts) +
  ggtitle("Daily Electricity Prices (2010-2016) - Logarithmic Transformation") +
  xlab("Date") +
  ylab("Price") +
  theme_minimal()  


diff_log_ts <- diff(log_ts)

autoplot(diff_log_ts) +
  ggtitle("Daily Electricity Prices (2010-2016) - First Differences of the Logarithmic Transformation") +
  xlab("Date") +
  ylab("Price") +
  theme_minimal()  

adf.test(diff_log_ts)

acf_values <- acf(as.numeric(diff_log_ts), plot = FALSE)

acf_df <- data.frame(
  Lag = acf_values$lag,
  ACF = acf_values$acf
)

ggplot(acf_df, aes(x = Lag, y = ACF)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "darkgrey", width = 0.2) +
  labs(title = "ACF - Autocorrelation Plot", y = "ACF", x = "Lag") +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(0.2, -0.2), linetype = "dashed", color = "red") +  # 95% confidence intervals (approx.)
  theme_minimal()


pacf_values <- pacf(as.numeric(diff_log_ts), plot = FALSE)

pacf_df <- data.frame(
  Lag = pacf_values$lag,
  PACF = pacf_values$acf
)

ggplot(pacf_df, aes(x = Lag, y = PACF)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "darkgrey", width = 0.2) +
  labs(title = "PACF - Partial Autocorrelation Plot", y = "PACF", x = "Lag") +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(0.2, -0.2), linetype = "dashed", color = "red") +  # 95% confidence intervals (approx.)
  theme_minimal()


# Manual ARIMA models
for (p in 1:6) {
  for (q in 1:6) {
      model <- arima(diff_log_ts, order = c(p, 0, q))
      print(c(p, q, model$aic))
  }
}

arima_model <- arima(diff_log_ts, order = c(3, 0, 1))
seasonal_1 <- Arima(diff_log_ts, order = c(1, 0, 1), seasonal = list(order = c(1, 0, 1), period = 8))
seasonal_2 <- Arima(diff_log_ts, order = c(1, 0, 1), seasonal = list(order = c(1, 0, 0)))


# Auto ARIMA model
auto_arima_model <- auto.arima(diff_log_ts)


summary(auto_arima_model)
summary(arima_model)
summary(seasonal_1)
summary(seasonal_2)

AIC(auto_arima_model)
AIC(arima_model)
AIC(seasonal_1)
AIC(seasonal_2)


residuals_auto_arima <- residuals(auto_arima_model)
residuals_arima_model <- residuals(arima_model)
residuals_seasonal_1 <- residuals(seasonal_1)
residuals_seasonal_2 <- residuals(seasonal_2)


autoplot(residuals_auto_arima) +
  ggtitle("Residuals of the auto ARIMA Model") +
  xlab("Date") +
  ylab("Residuals")

autoplot(residuals_arima_model) +
  ggtitle("Residuals of the ARIMA Model") +
  xlab("Date") +
  ylab("Residuals")

autoplot(residuals_seasonal_1) +
  ggtitle("Residuals of the SARIMA1 Model") +
  xlab("Date") +
  ylab("Residuals")

autoplot(residuals_seasonal_2) +
  ggtitle("Residuals of the SARIMA2 Model") +
  xlab("Date") +
  ylab("Residuals")



# residuals_seasonal_1 for the report
residuals_df <- data.frame(Date = time(residuals_seasonal_1), Residuals = residuals_seasonal_1)

ggplot(residuals_df, aes(x = Date, y = Residuals)) +
  geom_line() +
  ggtitle("Residuals of the SARIMA Model") +
  xlab("Date") +
  ylab("Residuals") +
  theme_minimal()



# Normality TestS of residuals

# Lilliefors Test 
lillie.test(residuals_seasonal_1)

# Shapiro-Wilk Test 
shapiro.test(residuals_seasonal_1)

# Histogram and Q-Q plot for normality
residuals_numeric <- as.numeric(residuals_seasonal_1)

ggplot(data.frame(residuals = residuals_numeric), aes(x = residuals)) + 
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.05, fill = "dark grey", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  ggtitle("Distribution of Residuals") +
  xlab("Residuals") +
  ylab("Density") +
  theme_minimal()

ggplot(data.frame(sample = residuals_numeric), aes(sample = sample)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  ggtitle("Q-Q Plot of Residuals") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme_minimal()
qqnorm(residuals_seasonal_1)
qqline(residuals_seasonal_1, col="red")


#Tests for Autocorrelation of Residuals

# Box-Pierce and Ljung-Box
Box.test(residuals_seasonal_1,  lag=24, type = "Box-Pierce") 
Box.test(residuals_seasonal_1,  lag=24, type = "Ljung-Box") 

# Durbin-Watson Test
dwtest(residuals_seasonal_1 ~ 1)

# Breusch-Godfrey Test
bgtest(residuals_seasonal_1 ~ 1, order = 1)


# ggplots for report
# Autocorrelation Plot
ggAcf(residuals_seasonal_1) +
  ggtitle("ACF - Autocorrelation Plot of Residuals") +
  xlab("Lag") +
  ylab("ACF") +
  theme_minimal()

# Partial Autocorrelation Plot
ggPacf(residuals_seasonal_1) +
  ggtitle("PACF - Partial Autocorrelation Plot of Residuals") +
  xlab("Lag") +
  ylab("PACF") +
  theme_minimal()


# Tests of Heteroskedasticity of Residuals

squared_residuals <- residuals_seasonal_1^2

# Autocorrelation Test of Squared Residuals
Box.test(squared_residuals, type = "Ljung-Box")

# Autocorrelation and Partial Autocorrelation Plots of Squared Residuals
ggAcf(squared_residuals)
ggPacf(squared_residuals)

# Goldfeld-Quandt Test
gqtest(residuals_seasonal_1 ~ fitted(seasonal_1))

# Breusch-Pagan Test
bptest(residuals_seasonal_1 ~ fitted(seasonal_1))

# White Test (for heteroskedasticity)
bptest(residuals_seasonal_1 ~ fitted(seasonal_1) + I(fitted(seasonal_1)^2))

# ggplots for report
acf_df <- data.frame(lag = acf_values$lag[-1], acf = acf_values$acf[-1])
pacf_df <- data.frame(lag = pacf_values$lag[-1], pacf = pacf_values$acf[-1])

ggplot(acf_df, aes(x = lag, y = acf)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "darkgrey", width = 0.2) +
  labs(title = "ACF - Autocorrelation Plot of Squared Residuals", y = "ACF", x = "Lag") +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(0.2, -0.2), linetype = "dashed", color = "red") +  # 95% confidence intervals
  theme_minimal()

ggplot(pacf_df, aes(x = lag, y = pacf)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "darkgrey", width = 0.2) +
  labs(title = "PACF - Partial Autocorrelation Plot of Squared Residuals", y = "PACF", x = "Lag") +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(0.2, -0.2), linetype = "dashed", color = "red") +  # 95% confidence intervals
  theme_minimal()


# Tests for stationarity
# KPSS 
kpss.test(residuals_seasonal_1)

# Phillips-Perron Test
pp.test(residuals_seasonal_1)



# Forecasting
last_value <- tail(ts, 1) # 65.06532

forecast_values <- forecast(seasonal_1, h = 12)
forecast_values_df <- as.data.frame(forecast_values)
exp_prices <- exp(forecast_values_df[ , "Point Forecast"])

forecasted_diffs <- cumsum(exp_prices)   
original_prices <- forecasted_diffs + 65.06532

original_prices_ts <- ts(original_prices, start = c(2018, 7), frequency = 12)


# Uncertainty Intervals:
# Uncertainty Intervals (in log scale)
UL_log <- forecast_values$upper[, 2]  
LL_log <- forecast_values$lower[, 2]  

# Transforming Upper and Lower Limits to Original Scale
UL_exp <- exp(UL_log)
LL_exp <- exp(LL_log)

# Cumulative sum transformation and adding the baseline value
UL <- cumsum(UL_exp) + 65.06532
LL <- cumsum(LL_exp) + 65.06532

forecast_values_df$Upper <- UL
forecast_values_df$Lower <- LL

start_date <- as.Date("2010-01-01")
dates <- seq(start_date, by = "month", length.out = length(ts))
forecast_dates <- seq(dates[length(dates)], by = "month", length.out = 12)

forecast_df <- data.frame(
  Date = forecast_dates,
  Predicted = original_prices_ts,
  Upper = UL,
  Lower = LL
)

actual_df <- data.frame(
  Date = dates,
  Actual = ts
)

ggplot() +
  geom_line(data = actual_df, aes(x = Date, y = Actual), color = "black") + 
  geom_line(data = forecast_df, aes(x = Date, y = Predicted), color = "red", size = 1) +
  geom_line(data = forecast_df, aes(x = Date, y = Upper), color = "blue", linetype = "dashed") +
  geom_line(data = forecast_df, aes(x = Date, y = Lower), color = "blue", linetype = "dashed") +
  labs(title = "Forecast for the First Half of 2019", x = "Date", y = "Values") +
  theme_minimal()



# Normality
residuals <- residuals(forecast_values)

shapiro.test(residuals)

residuals_numeric <- as.numeric(residuals)

ggplot(data.frame(residuals = residuals_numeric), aes(x = residuals)) + 
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.05, fill = "dark grey", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  ggtitle("Distribution of Forecast Residuals") +
  xlab("Residuals") +
  ylab("Density") +
  theme_minimal()



dwtest(residuals ~ 1)

acf_values <- acf(ts(residuals, frequency = 1), plot = FALSE)
acf_df <- data.frame(lag = acf_values$lag, acf = acf_values$acf)

ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", position = "identity", width = 0.2) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = qnorm((1 + 0.95)/2) / sqrt(length(residuals)), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = -qnorm((1 + 0.95)/2) / sqrt(length(residuals)), linetype = "dashed", color = "blue") +
  labs(title = "ACF of Forecast Residuals", x = "Lag", y = "ACF") +
  theme_minimal()


pacf_values <- pacf(ts(residuals, frequency = 1), plot = FALSE)
pacf_df <- data.frame(lag = pacf_values$lag, pacf = pacf_values$acf)

ggplot(pacf_df, aes(x = lag, y = pacf)) +
  geom_bar(stat = "identity", position = "identity", width = 0.2) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = qnorm((1 + 0.95)/2) / sqrt(length(residuals)), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = -qnorm((1 + 0.95)/2) / sqrt(length(residuals)), linetype = "dashed", color = "blue") +
  labs(title = "PACF of Forecast Residuals", x = "Lag", y = "PACF") +
  theme_minimal()


bptest(residuals ~ fitted(seasonal_1))



for (i in 1:length(original_prices)) {
  cat(sprintf("Month %d: %.2f\n", i, original_prices[i]))
}

trend <- ifelse(diff(original_prices) > 0, "incr", "decr")

average_price_2018 <- mean(window(ts, start = c(2018, 1)))

average_forecasted_price_2019 <- mean(original_prices)
