####################################################################################
#Project: Minor Project
#File: stock_returns.r
#Purpose: R program to predict stock returns for a given stock symbol using various time-series and ML models
#Author: Manoj Cheruvu

####################################################################################





####################################################################################
#Step 1: install/load the referrenced packages
####################################################################################
#all the library methods are in time_series_lib.r, load it into memory if not already loaded
if(!exists("ts_plot", mode="function")) source('time_series_lib.r')
#install.packages(c("caret", "recipes", "klaR", "ipred"))
#install.packages("Rcpp")
#install.packages("rmarkdown")
#install.packages("knitr")

#list of packages to be installed and referenced
packages =  c("timeSeries","forecast", "xts", "zoo", "timeDate", "tseries",
              "quantmod", "lubridate","ggfortify", "assertthat",
             "ggplot2", "dplyr", "tidyr", "data.table", 
              "corrplot", "gridExtra","TSA", "tsibble", "docstring", "readr", "here",
             "DT", "plotly")


ts_install_and_ref_packages(packages)

####################################################################################
#Step 2: get cached locally into data folder or get the new dataset with given stock symbol for the given date range
#data source: Yahoo Finance - https://finance.yahoo.com/
#the resulted object is time-series dataset/object
####################################################################################


symbol <- "AAPL"
symbol_title <- "Apple Stock Prices"

predict_months <- 3

toDate <- Sys.Date()
years <- 3
#total N years worth monthly data
fromDate <- toDate %m-% months(12*years)

print(fromDate)
print(toDate)

raw_stock_ts <- ts_get_sample_stock_data(symbol, fromDate, toDate,  periodicity = "monthly")

print('data is saved for future use and loaded into memory')

#analyse the time-series object
head(raw_stock_ts)
summary(raw_stock_ts)
str(raw_stock_ts)

####################################################################################
#Step 3: clean and transform the time-series dataset
####################################################################################

stock_ts <- ts_clean_and_transform(raw_stock_ts)
#stock_ts = raw_stock_ts

####################################################################################
#Step 4: Exploratory Data Analysis (EDA)
####################################################################################

# plot time series object
ts_plot(stock_ts, symbol_title)

# ACF and PACF
ts_auto_correlate(stock_ts, paste0('Log ', symbol_title))

# Conduct ADF test on log returns series
print(adf.test(stock_ts))

ts_decomp(stock_ts, symbol_title)

ts_seasonal(stock_ts, symbol_title)

####################################################################################
#Step 5: Split the dataset into training and testing by 80%-20%
####################################################################################

split_data_ts <- ts_split_train_and_test(stock_ts, ratio=0.8)

train_ts <- split_data_ts$train
test_ts <- split_data_ts$test

print(str(train_ts))
print(str(test_ts))

print(month.abb[cycle(train_ts)])

stock_df <- data.frame(stock_ts)
head(stock_df)
print(paste0('total dataset rows: ', nrow(stock_df)))

train_df <- data.frame(train_ts)
head(train_df)
print(paste0('train dataset rows: ', nrow(train_df)))

test_df <- data.frame(test_ts)
head(test_df)
print(paste0('test dataset rows: ', nrow(test_df)))


####################################################################################
#Step 6: Model the predictions using various algorithms
####################################################################################

arima <- ts_forecast_arima(train_ts, test_ts, predict_months, paste(symbol_title, "ARIMA Forecast", sep=" "))

exp_smoothing <- ts_forecast_exp_smoothing(train_ts, test_ts, predict_months, paste(symbol_title, "Exp Smooth", sep=" "))

#boxcox <- ts_forecast_box_cox(train_ts, test_ts, predict_months, paste(symbol_title, "Box-Cox Forecast", sep=" "))

naive <- ts_forecast_naive(train_ts, test_ts, predict_months, paste(symbol_title, "Naive Forecast", sep=" "))

snaive <- ts_forecast_seasonal_naive(train_ts, test_ts, predict_months, paste(symbol_title, "Seasonal Naive Forecast", sep=" "))

nnet <- ts_forecast_neural_net(train_ts, test_ts, predict_months, paste(symbol_title, "Neural Network Forecast", sep=" "))

####################################################################################
#Step 7: Compare algorithm accuracies
#refer: https://otexts.com/fpp2/accuracy.html
####################################################################################

result <- data.frame(Forecast = character(), ME=double(), 
                     RMSE=double(),MAE=double(), 
                     MPE=double(), MAPE=double(), 
                     MASE=double(), ACF1=double())

result$Forecast <- as.character(result$Forecast)

result[nrow(result) + 1,] <- append(list(Forecast = "ARIMA"), arima$accuracy)
result[nrow(result) + 1,] <- append(list(Forecast = "Exponential Smoothing"), exp_smoothing$accuracy)
#result[nrow(result) + 1,] <- append(list(Forecast = "Box-Cox"), accuracy_boxcox)
result[nrow(result) + 1,] <- append(list(Forecast = " Naive Forecast"),naive$accuracy)
result[nrow(result) + 1,] <- append(list(Forecast = "Seasonal Naive Forecast"), snaive$accuracy)
result[nrow(result) + 1,] <- append(list(Forecast = "Neural Network"), nnet$accuracy)

head(result[order(result$RMSE),], 5)


#print forecast values
print('Arima Forecast: ')
print(arima$forecast)

print('Exponential Smoothing Forecast: ')
print(exp_smoothing$forecast)

print('Naive Forecast: ')
print(naive$forecast)

#print('Seasonal Naive Forecast: ')
print(snaive$forecast)

#print('Neural Net Forecast: ')
print(nnet$forecast)



