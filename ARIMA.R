#### ARIMA ####

# install.packages("forecast")
# install.packages("tseries")
library(forecast)
library(tidyverse)


btc_total <- read_csv("C:/Users/Kevin/Desktop/Languages/R/CIND820/btc_total.csv")


### Variables
  prediction <- 30
  start_date <- 999 #2017-01-01
  end_date <- 2093 #2019-12-31


#-------------------------------------------------------------------------------
### Data Prep
  ## Filtering data
      train_set <- btc_total %>%
        select(ID, Date, Close)%>%
        filter(ID >= start_date & ID <= end_date)
        
      twitter_set <- btc_total %>%
        select(ID, Date, tweet_vol)%>%
        filter(ID >= start_date & ID <= end_date)
        
      test_set <- btc_total %>%
        select(ID, Date, Close)%>%
        filter(ID > end_date & ID <= (end_date + prediction))
  
  ## Converting data to time series type
      tsData <- ts(train_set$Close, start = c(2017), frequency = 365) # freq = 365, 212, 182, 30
      plot(tsData, ylab = "Price", xlab = "Date")
  
  ## Decomposing time series data - able to see underlying patterns  
      components_ts <- decompose(tsData, type = "mult")
      plot(components_ts, xlab = "Date")


  ## Stationary Conversion
    # ADF test to check if stationary - Result: p = 0.4136, data is non-stationary
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

    # decomposing stationary time series data
      tsData2 <- ts(BTC_stationary, start = c(2017), frequency = 365) # freq = 365, 212, 182, 30
      components_ts2 <- decompose(tsData2, type = "mult")
      plot(components_ts2, xlab = "Date")


#-------------------------------------------------------------------------------
### Model Creation
  ## Identify potential AR & MA Model ##
    # ACF & PACF tests
      acf(ts(train_set$Close), main = 'ACF BTC Price - No Change')
      pacf(ts(train_set$Close), main = 'PACF BTC Price - No Change')
      acf(ts(BTC_stationary), main ='ACF BTC Price')
      pacf(ts(BTC_stationary), main ='PACF BTC Price')

    # Auto-arima test - Result: ARIMA(0,1,0); confirms our above changes
      # lambda = 0 will log then inverse values
      arima_model <- auto.arima(train_set$Close, lambda = 0)
      summary(arima_model)

    # confirming fit of the model
      # result: Ljung-Box test - p = 0.1402; model is acceptable (p > 0.05)
      # the model's residuals are independent and not auto-correlated
      checkresiduals(arima_model)


#-------------------------------------------------------------------------------
### Forecasting
    forecast <- forecast(arima_model, h = prediction)
    autoplot(forecast, main = "BTC Price Prediction - ARIMA(0,1,0)", 
         ylab = "BTC Price ($)", 
         xlab = "Days",
         ylim = c(0, 20000))
    forecast

  # forecast comparison
    comparison <- data.frame(test_set, forecast)

  # MSE, RMSE, MAPE, & MAPA
    MSE <- mean((comparison$Close - comparison$Point.Forecast)^2)
    RMSE <- sqrt(MSE)
    MAPE <- (1/length(comparison$Close)) * sum(abs(comparison$Close - comparison$Point.Forecast)/comparison$Close) * 100
    MAPA <- 100 - MAPE  

    
#-------------------------------------------------------------------------------
### ARIMAX ###
  # creating the model with twitter volume as a regressor
    arimax_model <- auto.arima(train_set$Close, xreg = twitter_set$tweet_vol, lambda = 0)
    summary(arimax_model)

  # confirming fit of model
    # result: Ljung-Box test - p = 0.0545; model is acceptable (p > 0.05)
    # the model's residuals are independent and not auto-correlated
    checkresiduals(arimax_model)

  # forecasting
    forecast2 <- forecast(arimax_model, xreg = twitter_set$tweet_vol, h = prediction)
    autoplot(forecast2, main = "BTC Price Prediction - ARIMAX(0,1,0)", 
         ylab = "BTC Price ($)", 
         xlab = "Days",
         xlim = c(0, (length(train_set$ID) + prediction)), #quick-fix for the forecast limit issue
         ylim = c(0, 20000))
    forecast2

  # forecast Comparison
    forecast_df <- data.frame(forecast2)
    forecast_df <- forecast_df[1:prediction,] #quick-fix for the forecast limit issue
    comparison2 <- data.frame(test_set, forecast_df)

  # MSE, RMSE, MAPE, & MAPA
    MSE <- mean((comparison2$Close - comparison2$Point.Forecast)^2)
    RMSE <- sqrt(MSE)
    MAPE <- (1/length(comparison2$Close)) * sum(abs(comparison2$Close - comparison2$Point.Forecast)/comparison2$Close) * 100
    MAPA <- 100 - MAPE  
  
