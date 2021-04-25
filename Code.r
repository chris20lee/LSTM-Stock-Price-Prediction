# Name/#: Shadi Chamseddine 100937807
# Name/#: Nicolas Dirienzo  100978404
# Name/#: Christopher Lee   100937241
# Course Number: DATA5000 W
# Evaluation: Final Project

#https://www.codingfinance.com/post/2018-03-27-download-price/

#install packages if not already installed
if(!require(bizdays)) install.packages("bizdays")
if(!require(caTools)) install.packages("caTools")
if(!require(cowplot)) install.packages("cowplot")
if(!require(forecast)) install.packages("forecast")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(glue)) install.packages("glue")
if(!require(highcharter)) install.packages("highcharter")
if(!require(keras)) install.packages("keras")
if(!require(neuralnet)) install.packages("neuralnet")
if(!require(quantmod)) install.packages("quantmod")
if(!require(recipes)) install.packages("recipes")
if(!require(rsample)) install.packages("rsample")
if(!require(tensorflow)) install.packages("tensorflow")
if(!require(tibbletime)) install.packages("tibbletime")
if(!require(tidyquant)) install.packages("tidyquant")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(timetk)) install.packages("timetk")
if(!require(TimeWarp)) install.packages("TimeWarp")
if(!require(tseries)) install.packages("tseries")
if(!require(TTR)) install.packages("TTR")
if(!require(yardstick)) install.packages("yardstick")
if(!require(zoo)) install.packages("zoo")


#Load R packages required for program:
library(bizdays)
library(caTools)
library(cowplot)
library(forecast)
library(ggplot2)
library(glue)
library(highcharter)
library(keras)
library(neuralnet)
library(quantmod)
library(recipes)
library(rsample)
library(tensorflow)
library(tibbletime)
library(tidyquant)
library(tidyverse)
library(timetk)
library(TimeWarp)
library(tseries)
library(TTR)
library(yardstick)
library(zoo)

#install the tensorflow connection
#install_tensorflow()

options("getSymbols.warning4.0" = FALSE)
options("getSymbols.yahoo.warning" = FALSE)

# Downloading Apple Price using quantmod
getSymbols("AAPL", from = '2010-01-01', to = '2019-12-31', warnings = FALSE, auto.assign = TRUE)

head(AAPL)
class(AAPL)
View(AAPL)


#Create interactive stock chart to view price changes in stock
highchart(type = 'stock') %>% 
  hc_add_series(AAPL) %>% 
  hc_xAxis(type = 'datetime')

#Moving Average Convergence-Divergence (MACD) Stock Technical Indicators
macd = MACD(Cl(AAPL), nFast = 12, nSlow = 26, nSig = 9) #nFast = short-run smoothing, nSlow = Long-run Smoothing, nSig = Signal 
barChart(AAPL)
addMACD(fast = 12, slow = 26, signal = 9)



#Grab Bollinger Bands Data
B_Bands = BBands(Cl(AAPL), n=20, sd = 2)

#Graph Bollinger Bands, Volume, and Moving Average Convergence/Divegence 
AAPL%>%chartSeries(TA='addBBands();addVo();addMACD()',subset='2018') 

# Calculate the Exponential Moving Average
Exp_MA = TTR::EMA(Cl(AAPL), n=10)

#Pull in stock market volatility data
getSymbols("VIX", from = '2010-01-01', to = '2019-12-31', warnings = FALSE, auto.assign = TRUE)
head(VIX)
class(VIX)
View(VIX)
chart_Series(VIX)

#Fetch list of all tickers from Yahoo Finance
tickersList <- stockSymbols()


tickers = c("AAPL", "NFLX", "AMZN", "K", "O")
getSymbols(tickers, from = '2018-01-01', to = '2018-12-31')
prices = map(tickers, function(x) Ad(get(x)))
prices = reduce(prices, merge)
colnames(prices) = tickers
head(prices)
class (prices)
View (prices)

# Pull and merge all stock data of interest into a single table
getSymbols("SPY", from = '2015-01-01', to = '2019-12-31', warnings = FALSE, auto.assign = TRUE)
getSymbols("VIX", from = '2015-01-01', to = '2019-12-31', warnings = FALSE, auto.assign = TRUE)
getSymbols("AAPL", from = '2015-01-01', to = '2019-12-31', warnings = FALSE, auto.assign = TRUE)
getSymbols("NFLX", from = '2015-01-01', to = '2019-12-31', warnings = FALSE, auto.assign = TRUE)
getSymbols("AMZN", from = '2015-01-01', to = '2019-12-31', warnings = FALSE, auto.assign = TRUE)
getSymbols("K", from = '2015-01-01', to = '2019-12-31', warnings = FALSE, auto.assign = TRUE)
getSymbols("O", from = '2015-01-01', to = '2019-12-31', warnings = FALSE, auto.assign = TRUE)

final_data = cbind(SPY, VIX, AAPL, NFLX, AMZN, K, O)
View(final_data)
is.data.frame(final_data)

############Benchmark ARIMA Model############
#Generate Training and Testing Data:
row_count = nrow(AAPL)
subset = 0.80*row_count
train_data = AAPL[1:subset , ] #Sets training data as the first 80% subset of the total data
test_data = AAPL[-(1:subset),] #Sets test data as the remaining 20% subset of the total data

#We will investigate the autocorrelation and partial autocorrelation in our training dataset
acf(Cl(train_data))
pacf(Cl(train_data))

#We need our data to be stationary, so we first test for stationarity using an augmented Dickey-Fuller test
adf.test(Cl(train_data)) 

#Due to our high autocorrelation, we will optimize the fit of our training model using auto-ARIMA
model_fit = auto.arima(Cl(train_data), d=1, D = 1, seasonal = FALSE) #Suggests the best fit Auto ARMIA to fit our model
plot(Cl(train_data), type = 'l') #Plot the data
title('APPL Stock Price')

#Forecast Value using ARIMA - We use our trained model to forecast the closeness our model is to the test data
# 0 Auto-regressors, Difference of 1 for time-series stationary, 0 order of Moving average
forecast_price = forecast(model_fit, h = 0.2*row_count) #h is the number of future time periods we are trying to forecast (days)
forecast_price
plot(forecast_price)


#Test the deviation of our forecast from our test data to determine the forecast error
trainVStestDF = data.frame(test_data$AAPL.Close, forecast_price)
forecast_error = ((trainVStestDF$AAPL.Close - trainVStestDF$Point.Forecast)/trainVStestDF$AAPL.Close)
forecast_error
mean(forecast_error)

plot(trainVStestDF$AAPL.Close,type="l",col="Black", main = "Apple Stock Price VS ARIMA Forecasted Price", xlab = "Day", ylab = "Apple Stock Price ($)") #Now plot the actual test data to our model's fitted predicition
lines(trainVStestDF$Point.Forecast,col="Red")


#Ljung-Box to test for randomness of the risiduals (P-values that reject the null means risiduals are random which indicates a strong model)
Box.test(model_fit$residuals, lag = 5, type = "Ljung-Box")
Box.test(model_fit$residuals, lag = 10, type = "Ljung-Box")
Box.test(model_fit$residuals, lag = 15, type = "Ljung-Box")


##########Long Short-Term Memory Neural Network Model##########

# Downloading Apple Price using quantmod
getSymbols("AAPL", from = '1990-01-01', to = '2019-12-31', warnings = FALSE, auto.assign = TRUE)
AAPL = map("AAPL", function(x) Ad(get(x)))
AAPL = reduce(AAPL, merge)
colnames(AAPL) = "value"

################# LSTM Start ###########################################
Series = AAPL$value  # your time series 

# difference the dataset for stationarity
diffed = diff(Series, differences = 1)

# create a lagged dataset, i.e to be supervised learning
lags <- function(x, k){
  lagged =  c(rep(NA, k), x[1:(length(x)-k)])
  DF = as.data.frame(cbind(lagged, x))
  colnames(DF) <- c( paste0('x-', k), 'x')
  DF[is.na(DF)] <- 0
  return(DF)
}
supervised = lags(diffed, 1)


## split into train and test sets
row_count = nrow(supervised)
subset = round(row_count *1, digits = 0)
train = supervised[1:subset, ]
AAPL_train = AAPL[1:subset, ]
AAPL_test = AAPL[(subset+1):row_count,  ]

## scale data
normalize <- function(train, feature_range = c(0, 1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = ((x - min(x) ) / (max(x) - min(x)  ))
  
  scaled_train = std_train *(fr_max -fr_min) + fr_min

  return( list(scaled_train = as.vector(scaled_train), scaler= c(min =min(x), max = max(x))) )
}


## inverse-transform
inverter = function(scaled, scaler, feature_range = c(0, 1)){
  min = scaler[1]
  max = scaler[2]
  n = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(n)
  
  for( i in 1:n){
    X = (scaled[i]- mins)/(maxs - mins)
    rawValues = X *(max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}


Scaled = normalize(train, c(-1, 1))

y_train = Scaled$scaled_train[, 2]
x_train = Scaled$scaled_train[, 1]

## fit the model
dim(x_train) <- c(length(x_train), 1, 1)
dim(x_train)
X_shape2 = dim(x_train)[2]
X_shape3 = dim(x_train)[3]
batch_size = 1
units = 1

model <- keras_model_sequential() 
model%>%
  layer_lstm(units, activation = 'relu', batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE)%>%
  layer_dense(units = 1)

model %>% 
  compile(loss = 'mae', optimizer = 'adam')

summary(model)

#Fit the Model 
Epochs = 10
for(i in 1:Epochs){
  model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  model %>% reset_states()
}

#grab a list of all date indicies
dates <- rownames(train)
#Store the last index as a variable that will allow us to offset by for creating future predictions
last_date = xts::last(dates, 1)
create.calendar(name="Actual")

#Model predictions past test data
pred_periods = 60
future_pred = numeric(pred_periods)

scaler = Scaled$scaler

Series = as.data.frame(Series)

set.seed(2009)
noise = sample(x_train, 1)
noise_index = x_train %>% match(x = noise)

#Store prediction
for(i in 1:pred_periods){
  
  random = sample(1:2, 1)
  if(random == 1){
    WN = arima.sim(list(order = c(0, 0, 0)), n = 1, mean = 1, sd = sample(3:6, 1)) #Generate White Noise
  } else {
    WN = arima.sim(list(order = c(0, 0, 0)), n = 1, mean = -1, sd = sample(3:6, 1)) #Generate White Noise
  }
  
  
  if (i%%30 == 0){
    noise = sample(x_train, 1)
    noise_index = noise_index = x_train %>% match(x = noise)
    
  }else{
    noise = x_train[noise_index]
    noise_index = noise_index + 1
  }
  
  #X = mean(tail(x_train, 200)) + WN
  #X = sample(x_train, 1)
  X = noise
  
  
  #X = x_test[i] #Stretch this out to all the periods I want to account for in the predicition
  dim(X) = c(1,1,1) #adjust the dimensions (first 1 becomes length of x)
  yhat = model %>% predict(X, batch_size=batch_size)
  
  #Scale and Append new values to the x_test table
  x_train = append(x_train, yhat)
  
  #invert scaling
  yhat = inverter(yhat, scaler,  c(-1, 1))
  
  # invert differencing
  yhat  = yhat + Series[subset + i - 1,]
  yhat = Series$value[length(AAPL) + i - 1] + WN
  Series = rbind(Series, yhat)
  
  print(yhat)
  #Store the conpleted prediction 
  future_pred[i] <- yhat + noise
}

#Create Future Prediction DataFrame
future_pred_df = data.frame(future_pred)
colnames(future_pred_df) = "value"

#Append new day to dataframe to store the result to be used at the next iteration
for(i in 1:nrow(future_pred_df)){
  new_day = add.bizdays(last_date, i, "Actual") #Get next business day 
  #rownames(future_pred[nrow(future_pred)+1,]) = new_day #add business day to the dataframe
  rownames(future_pred_df)[i] = format(as.Date(new_day)) #add business day to the dataframe
}

final_AAPL_true = as.data.frame(AAPL)
final_AAPL_true$Type = "Actual"
future_pred_df$Type = "Prediction"
final_table = rbind(final_AAPL_true, future_pred_df)

#Get data to test against
getSymbols("AAPL", from = '2019-12-31', to = '2020-03-27', warnings = FALSE, auto.assign = TRUE)
AAPL = map("AAPL", function(x) Ad(get(x)))
AAPL2 = reduce(AAPL, merge)
colnames(AAPL2) = "value2"
AAPL2 = as.data.frame(AAPL2)

x = cbind(future_pred_df,AAPL2)


plot(x$value2, type="l", col = "black", xlab = "Date", ylab = "$ USD")
lines(x$value,col = "red")
forecast_error3 = ((x$value2 - x$value)/x$value2)
forecast_error3
mean(forecast_error3)



plot(future_pred_df$value, type="l", col= "Red", xlab = "Date", ylab = "$ USD")
lines(AAPL2$AAPL.Close, col= "Black")

###########################PLOT THE RESULTS########################################
plot(PredDF$value,type="l",col="Black", xlab = "Date", ylab = "$ USD", title = "Actual vs Prediction Valuation of AAPL Stock") #Now plot the actual test data to our model's fitted predicition
lines(PredDF$PlotPred.predictions,col="Red")
lines(future_pred$future_pred,col="Orange")

plot(future_pred$future_pred,type="l",col="Black", xlab = "Date", ylab = "$ USD", title = "Actual vs Prediction Valuation of AAPL Stock") #Now plot the actual test data to our model's fitted predicition

forecast_error2 = ((PredDF$value - PredDF$PlotPred.predictions)/PredDF$value)
forecast_error2
mean(forecast_error2)

###############################END LSTM CODE#################################
