install.packages("tensorflow")
library(tensorflow)
install_tensorflow()

install.packages("keras")
library(keras)
install_keras()

library(tidyverse)
library(ggplot2)

# Package & Library Check
x <- tf$constant("Hello Tensorflow")
x

# Loading data frame
btc_total <- read_csv("btc_total.csv")



### Data Prep ###
prediction <- 30
start_date <- 999 #2017-01-01
end_date <- 2093 #2019-12-31

# filtering data
train_set <- btc_total %>%
  select(ID, Date, Close)%>%
  filter(ID >= start_date & ID <= end_date)

twitter_set <- btc_total %>%
  select(ID, Date, tweet_vol)%>%
  filter(ID >= start_date & ID <= end_date)

test_set <- btc_total %>%
  select(ID, Date, Close)%>%
  filter(ID > end_date & ID <= (end_date + prediction))

# rescaling the data using mean and standard deviation of the training set
# Mean = 6324.52628412039 SD = 3533.28977718451
scale_factors <- c(mean(train_set$Close), sd(train_set$Close))

scaled_train <- train_set %>%
  select(Close) %>%
  mutate(Close = (Close - scale_factors[1]) / scale_factors[2])


## Conversion to tensor format ##
# 3D array [samples, timesteps, features] #
  # samples - # of observations which will be processed in batches.
  # timesteps - how many units back in time we want our network to see.
  # features - # of predictors (1 for univariate, n for multivariate).
lag <- prediction
scaled_train <- as.matrix(scaled_train)

# lag the data and arrange into columns for X values (predictor)
x_train_data <- t(sapply(
  1:(length(scaled_train) - lag - prediction + 1),
  function(x) scaled_train[x:(x + lag - 1), 1]
))

# transform data into 3D form for x-values (predictor)
x_train_arr <- array(
  data = as.numeric(unlist(x_train_data)),
  dim = c(
    nrow(x_train_data),
    lag,
    1
  )
)

# lag and transform for y-values (target)
y_train_data <- t(sapply(
  (1 + lag):(length(scaled_train) - prediction + 1),
  function(x) scaled_train[x:(x + prediction - 1)]
))

y_train_arr <- array(
  data = as.numeric(unlist(y_train_data)),
  dim = c(
    nrow(y_train_data),
    prediction,
    1
  )
)

## Prepare input data for prediction ##
x_test <- btc_total$Close[(nrow(scaled_train) - prediction + 1):nrow(scaled_train)]

# scale the data with same scaling factors as for training
x_test_scaled <- (x_test - scale_factors[1]) / scale_factors[2]

# one sample for the array b/c we are performing one [insert prediction #] day prediction
x_pred_arr <- array(
  data = x_test_scaled,
  dim = c(
    1,
    lag,
    1
  )
)



### LSTM Model Creation ###
lstm_model <- keras_model_sequential()

lstm_model %>%
  layer_lstm(units = 50, # size of the layer - apparently 2 is good enough?
             batch_input_shape = c(1, 30, 1), # samples, timesteps, features
             return_sequences = TRUE,
             stateful = TRUE) %>%
  # fraction of the units to drop for the linear transformation of the inputs
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 50,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  time_distributed(keras::layer_dense(units = 1))

# Compiling the RNN
lstm_model %>%
  compile(loss = 'mae', optimizer = 'adam', metrics = 'accuracy')

summary(lstm_model)

# Fitting the RNN to the training set
lstm_model %>% fit(
  x = x_train_arr,
  y = y_train_arr,
  batch_size = 1,
  epochs = 20,
  verbose = 0,
  shuffle = FALSE
)



### Forecasting ###
lstm_forecast <- lstm_model %>%
  predict(x_pred_arr, batch_size = 1) %>%
  .[, , 1]

# rescale the data to restore the original values
lstm_forecast <- lstm_forecast * scale_factors[2] + scale_factors[1]
lstm_forecast

# forecast comparison
comparison <- data.frame(test_set, lstm_forecast)

# MSE, RMSE
# Add: MAPE, Accuracy, & Percentage of Accuracy
mean((comparison$Close - comparison$lstm_forecast)^2)
sqrt(mean((comparison$Close - comparison$lstm_forecast)^2))

## Plotting ##
comparison$ID <- c(1:length(comparison$Close))
ggplot(comparison, aes(x=ID)) + 
  geom_line(aes(y = Close), color = "darkred") + 
  geom_line(aes(y = lstm_forecast), color="blue")+
  labs(x = "Day", y = "BTC Price ($)")



### Regressor ###
reg <- twitter_set$tweet_vol

# rescaling the data using mean and standard deviation of the training set
scale_factors_reg <- list(
  mean = mean(reg),
  sd = sd(reg)
)

scaled_reg <- (reg - scale_factors_reg$mean)/scale_factors_reg$sd

# values for the forecast
scaled_reg_prediction <- (reg[(length(reg) - prediction): length(reg)] - 
                            scale_factors_reg$mean) /scale_factors_reg$sd

# combine training data with regressor
x_train <- cbind(scaled_train, scaled_reg)
x_train_data <- list()

# transform the data into lagged columns
for (i in 1:ncol(x_train)) {
  x_train_data[[i]] <- t(sapply(
    1:(length(x_train[, i]) - lag - prediction + 1),
    function(x) x_train[x:(x + lag - 1), i]
  ))
}

# transform list into a 3D array
x_train_arr <- array(
  data = as.numeric(unlist(x_train_data)),
  dim = c(
    nrow(x_train_data[[1]]),
    lag,
    2
  )
)

# combine data with regressor
x_test_data <- c(x_test_scaled, scaled_reg_prediction)

# transform to tensor
x_pred_arr <- array(
  data = x_test_data,
  dim = c(
    1,
    lag,
    2
  )
)

## LSTM Model Creation w/ Regressor ##
lstm_model <- keras_model_sequential()

lstm_model %>%
  layer_lstm(units = 50, # size of the layer - apparently 2 is good enough?
             batch_input_shape = c(1, 30, 2), # samples, timesteps, features
             return_sequences = TRUE,
             stateful = TRUE) %>%
  # fraction of the units to drop for the linear transformation of the inputs
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 50,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  time_distributed(keras::layer_dense(units = 1))

# Compiling the RNN
lstm_model %>%
  compile(loss = 'mae', optimizer = 'adam', metrics = 'accuracy')

summary(lstm_model)

# Fitting the RNN to the training set
lstm_model %>% fit(
  x = x_train_arr,
  y = y_train_arr,
  batch_size = 1,
  epochs = 20,
  verbose = 0,
  shuffle = FALSE
)

## Forecasting with Regressor ##
lstm_forecast <- lstm_model %>%
  predict(x_pred_arr, batch_size = 1) %>%
  .[, , 1]

# rescale the data to restore the original values
lstm_forecast <- lstm_forecast * scale_factors[2] + scale_factors[1]
lstm_forecast

# forecast comparison
comparison <- data.frame(test_set, lstm_forecast)

# MSE, RMSE
# Add: MAPE, Accuracy, & Percentage of Accuracy
mean((comparison$Close - comparison$lstm_forecast)^2)
sqrt(mean((comparison$Close - comparison$lstm_forecast)^2))

### Plotting with Regressor ###
comparison$ID <- c(1:length(comparison$Close))
ggplot(comparison, aes(x=ID)) + 
  geom_line(aes(y = Close), color = "darkred") + 
  geom_line(aes(y = lstm_forecast), color="blue")+
  labs(x = "Day", y = "BTC Price ($)")
