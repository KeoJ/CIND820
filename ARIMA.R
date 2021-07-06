#### ARIMA ####

# install.packages("forecast")
# install.packages("tseries")
library(forecast)
library(tidyverse)


### Data Prep ###
plot(x = btc_total$Date, y = btc_total$Close, xlab = "Date", ylab = "Price")

# filtering data to 2017-01-01 to 2019-12-31
train_set <- btc_total %>%
  select(ID, Date, Close)%>%
  filter(ID > 998 & ID <2094)

twitter_set <- btc_total %>%
  select(ID, Date, tweet_vol)%>%
  filter(ID > 998 & ID <2094)

# converting data to time series type
tsData <- ts(train_set$Close, start = c(2017, 1), frequency = 365)
plot(tsData, ylab = "Price", xlab = "Date")

# decomposing time series data - able to see underlying patterns  
components_ts <- decompose(tsData, type = "mult")
plot(components_ts, xlab = "Date")



## Stationary Conversion ##
# ADF test to check if stationary - Result: p = 0.4136, data is not stationary
adf.test(train_set$Close)

# difference data to make it stationary on mean (remove trend)
plot(diff(tsData, lag = 1, differences = 1), ylab='Differenced BTC Price')

# log transform data to make it stationary on variance
plot(log10(tsData), ylab='Log BTC Price')

# difference log data to make stationary on both mean & variance
plot(diff(log10(tsData), lag = 1, differences = 1), ylab='Differenced Log BTC Price')

# ADF test - Result: p < 0.01, null hypothesis rejected, data is stationary
# Result is also p < 0.01 with just differencing
BTC_stationary <- diff(log10(train_set$Close), lag = 1, differences = 1)
adf.test(BTC_stationary)


### Model Creation ###

## Identify potential AR & MA Model ##
# ACF & PACF tests
acf(ts(train_set$Close), main = 'ACF BTC Price - No Change')
pacf(ts(train_set$Close), main = 'PACF BTC Price - No Change')
acf(ts(BTC_stationary), main ='ACF BTC Price')
pacf(ts(BTC_stationary), main ='PACF BTC Price')

# Auto-arima test - Result: ARIMA(0,1,0); confirms our above changes
arima_fit <- auto.arima(log10(train_set$Close))
summary(arima_fit)

# confirming fit of the model
# result: Ljung-Box test - p = 0.1402; model is acceptable (p > 0.05)
# the model's residuals are independent and not auto-correlated
checkresiduals(arima_fit)

# creating the model
# Question: should I convert the data to log?
# arima <- arima(log10(train_set$Close), order = c(0,1,0))
arima_model <- arima(log10(train_set$Close), order = c(0,1,0))
summary(arima_model)


### Forecasting ###
forecast <- forecast(arima_model, h = 50)
plot(forecast) #fix the x-axis labels
forecast

# Forecast Comparison
test_set <- btc_total %>%
  select(ID, Date, Close)%>%
  filter(ID > 2093 & ID < 2144)
test_set$Close <- log10(test_set$Close)
comparison <- data.frame(test_set, forecast)

# MSE & RMSE
mean((comparison$Close - comparison$Point.Forecast)^2)
sqrt(mean((comparison$Close - comparison$Point.Forecast)^2))

### ARIMAX ###
# creating the model with twitter volume as a regressor
arimax_model <- auto.arima(log10(train_set$Close), xreg = log10(twitter_set$tweet_vol))
summary(arimax_model)

# forecasting
forecast2 <- forecast(arimax_model, xreg = log10(twitter_set$tweet_vol), h = 50 )
plot(forecast2) #fix the x-axis labels
forecast2

# forecast Comparison
forecast_df <- data.frame(forecast2)
forecast_df <- forecast_df[1:50,] #fix for the forecast period limit issue
comparison2 <- data.frame(test_set, forecast_df)

# MSE & RMSE
mean((comparison2$Close - comparison2$Point.Forecast)^2)
sqrt(mean((comparison2$Close - comparison2$Point.Forecast)^2))
