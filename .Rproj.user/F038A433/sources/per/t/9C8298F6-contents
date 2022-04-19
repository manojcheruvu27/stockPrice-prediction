#Helper functions to manage time-series objects and plots
# use ?function_name for more details.


###########################################################################################
# Function: ts_install_and_ref_package                                                   #
# installs and refers the input package                                                   #
###########################################################################################

ts_install_and_ref_package <- function(pkg, ...){
  #' installs and refers the input package     
  #'  
  #'
  #' @param pkg package to be installed and referred   
  #' @examples
  
  #' ts_install_and_ref_package("xts")
  #'
  #'
  if (!(pkg %in% installed.packages()[,1])) {
    install.packages(pkg)
  }
  return (library(pkg, ...))
}
###########################################################################################


###########################################################################################
# Function: ts_install_and_ref_packages                                                 #
# installs and refers the packages                                                      #
###########################################################################################

ts_install_and_ref_packages <- function(pkgs, ...){
  #' installs and refers the R packages
  #' calls ts_install_and_ref_package 
  #'
  #' @param pkgs array of R package names  #' 
  #' @examples
  
  #' ts_install_and_ref_packages(c("xts", "timeSeries","tseries", "forecast"))
  #'
  #'
  purrr::walk(pkgs, ts_install_and_ref_package, character.only = TRUE, warn.conflicts = FALSE)
}


###########################################################################################
# Function: ts_save_as_csv                                                                #
# save time series object to CSV file                                                     #
###########################################################################################
ts_save_as_csv <- function(x, fname) {
  #' Saves time series object to CSV file
  #'
  #'
  #' @param x time series object
  #' @param fname  file_name, ex: google_stock_data.csv
  #' @examples
  
  #' ts_save_as_csv(ts, 'google_stock_data.csv')
  #'
  
  write.zoo(x, fname, sep = ",")
}

ts_save_as_csv_old <- function(x, fname) {
  #' Saves time series object to CSV file
  #'
  #'
  #' @param x time series object
  #' @param fname  file_name, ex: google_stock_data.csv
  #' @examples
  
  #' ts_save_as_csv(ts, 'google_stock_data.csv')
  #'
  
  if (NCOL(x) == 1L) {
    # Univariate time series
    readr::write_csv(
      as.data.frame(tsibble::as_tsibble(x)),
      fname)
  } else {
    # Multivariate time series
    readr::write_csv(
      as.data.frame(tsibble::spread(tsibble::as_tsibble(x), key, value)),
      fname)
  }
}
###########################################################################################


###########################################################################################
# Function: ts_get_sample_stock_data                                                        #
#  function to get stock data that returns time series object and saves data for future use #
###########################################################################################
ts_get_sample_stock_data <- function(symbol, fromDate, toDate, periodicity = "monthly"){
  #' Get Sample Stock Data using getSymbols method of quantmod package
  #'
  #' Get the sample daily stock data for the given perioids
  #' custom themes to ensure plots are
  #' consistent. Utlizes \code{autoplot} function for plots.
  #'
  #' @param symbol string - stock symbol ex: GOOG for google
  #' @param fromDate  date-string - from date: ex: 2018-03-10
  #' @param toDate  date-string - to date: ex: 2019-03-10
  #' @param periodicity valid values are "daily", "weekly", "monthly"
  #' @examples
  
  #' ts_get_sample_stock_data('GOOG', '2018-03-10', '2019-03-10', periodicity="daily")
  #'
  
  
  valid <- TRUE
  help(is.date)
  if (missing(symbol) == TRUE || trimws(symbol) == ""){
    valid <- FALSE
  }
  
  if (missing(fromDate) == TRUE || is.date(fromDate) == FALSE){
    valid <- FALSE
  }
  
  if (missing(toDate) == TRUE || is.date(toDate) == FALSE){
    valid <- FALSE
  }
  
  if (valid == FALSE){
    warning("invalid input")
  }
  else{
    
    from <- format(as.Date(fromDate), "%Y_%m_%d")
    to <- format(as.Date(toDate), "%Y_%m_%d")
    
    data_folder <- paste0(getwd(), "/data")
    data_file <- paste0(data_folder, "/stock_data_", tolower(symbol), "-", from, "-", to, ".csv")
    debug(data_file)
    
    # check data folder exists, if not, create it
    if (!dir.exists(data_folder)){
      
      dir.create(data_folder)
    }
    
    if (file.exists(data_file)){
      debug('data is loaded from the file')
      #data <- as.xts(read.zoo(file=data_file, sep=",", header=TRUE))
      data <- read.csv(data_file)
    }
    else{
      
      #?getSymbols
      # data <- getSymbols(symbol, from=fromDate, to=toDate, 
      #                    auto.assign = FALSE, 
      #                    src="yahoo",
      #                    periodicity = periodicity,
      #                    return.class = "ts")
      
      
      data <- getSymbols.yahoo(symbol, from=fromDate, to=toDate, 
                               auto.assign = FALSE, 
                               periodicity = periodicity,
                               return.class = "ts")
      
      
      ts_save_as_csv(data, data_file)
      #write.zoo(data, file= data_file, sep=",")
      #save data for future use
      print('data is loaded from getSymbols and saved to disk for future use')
    }
    
    # Select the relevant close price series
    stock_prices <- data[,4]
    stock_prices <- stock_prices[!is.na(stock_prices)]
    
    debug(length(stock_prices))
    
    debug(head(stock_prices))
    
    stock <- stock_prices
    stock <- stock[!is.na(stock)]
    
    print(class(stock))
    
    print(frequency(stock))
    
    debug(length(stock))
    
    #monthly
    frequency <- 365.5
    
    if (periodicity == "daily")
    {
      frequency <- 365.5
    }
    else if (periodicity == "weekly")
    {
      frequency <- 52
    }
    else {
      frequency <- 12
    }
    
    print(frequency)
    
    stock_ts <- ts(stock,
                   start=c(year(fromDate), month(fromDate)),
                   end=c(year(toDate), month(toDate)),
                   frequency = frequency)
    
    return(stock_ts)
    
  }
}
###########################################################################################


###########################################################################################
# Function: ts_clean_and_transform                                                                 #
#  function to clean, transform (log) and apply diff(), if need be to make ts stationary  #
###########################################################################################
ts_clean_and_transform <- function(ts_object){
  #' function to clean, transform (log) and apply diff(), if need be to make ts stationary
  #'
  #' Get the sample daily stock data for the given perioids
  #' custom themes to ensure plots are
  #' consistent. Utlizes \code{autoplot} function for plots.
  #'
  #' @param symbol string - stock symbol ex: GOOG for google
  #' @param fromDate  date-string - from date: ex: 2018-03-10
  #' @param toDate  date-string - to date: ex: 2019-03-10
  #' @param periodicity valid values are "daily", "weekly", "monthly"
  #' @examples
  #' data(AirPassengers)
  #' air_pass_ts <- as.ts(AirPassengers)
  #' air_pass_ts_transform <- ts_clean_and_transform(air_pass_ts)
  #'
  
  
  if (is.ts(ts_object) == TRUE){
    
    start <- start(ts_object)
    print(start)
    
    end <- end(ts_object)
    print(end)
    
    frequency <- frequency(ts_object)
    print(frequency)
    
    # clean the time series object
    #stock_prices <- tsclean(ts_object)
    stock_prices <- data.frame(ts_object)
    
    #find out whether 
    skew <- skewness(stock_prices)
    debug(paste0("skewness: ", skew))
    
    kurt <- kurtosis(stock_prices)
    debug(paste0("kurtosis: ", kurt))
    
    #if (kurt > 2 || kurt < 2)
    #{
     # debug('data is skewed. we will apply transformation')
      #data is skewed, apply log transformation
      #stock_prices <- log(stock_prices)
    #}
    
    # ADF: The null-hypothesis for an ADF test is that the data are non-stationary. 
    # So large p-values are indicative of non-stationarity, 
    # and small p-values suggest stationarity. 
    #need_diff <- adf.test(stock_prices)$p.value > 0.05
    
    #number of differences necessary to make data stationary
    #ndiff <- ndiffs(stock_prices)
    
    #if (ndiff > 0)
    #{
      #debug('data is not stationary. we will need to apply diff()')
      
      #Compute the log returns for the stock
      #stock_prices <- diff(stock_prices,lag=1)
    #}
    
    stock <- stock_prices
    #stock <- stock[!is.na(stock)]
    
    print(class(stock))
    
    print(frequency(stock))
    
    debug(length(stock))
    
    stock_ts <- ts(stock,
                   start=start,
                   end=end,
                   frequency = frequency)
    
    
    return(stock_ts)
    
  } else {
    warning('Make sure object entered is time-series object!')
  }
  
}
##########################################################################################

###########################################################################################
# Function: ts_split_train_and_test                                                         #
# Cleans the time series data and splits into training and testing datasets               #
###########################################################################################
ts_split_train_and_test <- function(ts_object, ratio=0.8){
  #' Cleans the time series data and splits into training and testing datasets  
  #'
  #'
  #' @param ts_object time series object
  #' @param ratio training and testing dataset ratio
  #' @examples
  #' data(AirPassengers)
  #' air_pass_ts <- as.ts(AirPassengers)
  #' air_pass_clean <- ts_split_train_and_test(air_pass_ts)
  #' air_pass_clean$train
  #' air_pass_clean$test
  
  if (is.ts(ts_object) == TRUE){
    
    # clean the time series object
    stock_clean <- tsclean(ts_object)
    
    train <- head(stock_clean, round(length(stock_clean) * ratio))
    h <- length(stock_clean) - length(train)
    test <- tail(stock_clean, h)
    
    data <- list( train ,test )
    names(data) <- c("train" , "test")
    
    return(data)
    
  } else {
    warning('Make sure object entered is time-series object!')
  }
}
###########################################################################################


###########################################################################################
# Function: ts_plot                                                                      #
# Plot Time Series Object                                                                 #
###########################################################################################
ts_plot <- function(ts_object, ts_object_name, color="dodgerblue3"){
  #' Plot Time Series Object
  #'
  #' Creates time series plot utilizing \code{ggplot2} utlizing
  #' custom themes to ensure plots are
  #' consistent. Utlizes \code{autoplot} function for plots.
  #'
  #' @param ts_object time series object used to create plot
  #' @param ts_object_name preferred title of plot
  #' @param color color of the plotting, defaults to "dodgerblue3"
  #' @examples
  #' data(AirPassengers)
  #' air_pass_ts <- as.ts(AirPassengers)
  #' ts_plot(air_pass_ts)
  #'
  #'
  
  default.color <- color
  
  #' plot_time_series(air_pass_ts, 'Air Passengers')
  if (is.ts(ts_object) == TRUE){
    if(missing(ts_object_name)) {
      warning('Title for plot not entered!')
    } else {
      startYear <- start(ts_object) # Grabs start date
      endYear <- end(ts_object) # Grabs end date
      tsPlot <- autoplot(ts_object,
                         ts.colour =  default.color,
                         size = 1,
                         main = sprintf("Plot of %s Time Series (%s - %s)",
                                        ts_object_name, startYear[1], endYear[1])) +
        theme(axis.text.x = element_text(angle = 35, hjust = 1),
              panel.background = element_rect(fill = "gray98"),
              axis.line.x = element_line(colour="gray"),
              axis.line.y = element_line(colour="gray")) +
        labs(x = "Year", y = "Closing Values") 
      
      #abline(reg=lm(ts_object~time(ts_object)))
      
      return(tsPlot)
    }
  }
  else {
    warning('Make sure object entered is time-series object!')
  }
}
###########################################################################################


###########################################################################################
# Function: ts_auto_correlate                                                              #
# ACF and PCAF of a time series object and plot them                                       #
###########################################################################################
ts_auto_correlate <- function(ts_object, ts_object_name, color="dodgerblue3", fill_color="#ffff4d"){
  #' Plot ACF and PACF for Time Series Object
  #'
  #' Creates \emph{Autocorrelation} and \emph{Partial Autocorrelation} plot
  #' utilizing \code{ggplot2} with custom themes to ensure plots are
  #' consistent. Utlizes \code{autoplot} function for plots.
  #'
  #' @param ts_object time series object used to create plot
  #' @param ts_object_name preferred title of plot
  #' @param color color of the plotting, defaults to "dodgerblue3"
  #' @param fill_color color to fill the background of the plotting, defaults to "#ffff4d"
  #' @examples
  #' data(AirPassengers)
  #' air_pass_ts <- as.ts(AirPassengers)
  #' ts_auto_correlate(air_pass_ts, "air passengers auto correlate")
  #'
  #' 
  #' 
  
  default.color <- color
  default.fill <- fill_color
  
  if (is.ts(ts_object) == TRUE){
    if(missing(ts_object_name)) {
      warning('Title for plot not entered!')
    } else {
      a <- autoplot(acf(ts_object, plot = FALSE),
                    colour = default.color,
                    conf.int.fill = default.fill,
                    conf.int.value = 0.95, conf.int.type = 'ma') +
        theme(panel.background = element_rect(fill = "gray98"),
              axis.line.y   = element_line(colour="gray"),
              axis.line.x = element_line(colour="gray")) +
        ggtitle(sprintf("ACF plot of %s", ts_object_name))
      
      b <- autoplot(pacf(ts_object, plot = FALSE),
                    colour = default.color,
                    conf.int.fill = default.fill,
                    conf.int.value = 0.95, conf.int.type = 'ma') +
        theme(panel.background = element_rect(fill = "gray98"),
              axis.line.y   = element_line(colour="gray"),
              axis.line.x = element_line(colour="gray")) + labs(y="PACF") +
        ggtitle(sprintf("PACF plot of %s", ts_object_name))
      
      grid.arrange(a, b)
    }
  } else {
    warning('Make sure object entered is time-series object!')
  }
}
###########################################################################################

###########################################################################################
# Function: ts_decomp                                                                     #
# Decompose time series and plots the components                                          #
###########################################################################################
ts_decomp <- function(ts_object, ts_object_name, color="dodgerblue3"){
  #' Plots Seasonal Decomposition for Time Series Object
  #'
  #' Decomposes time series object to \emph{Seasonal},
  #' \emph{Remainder}, and \emph{Trend}.
  #' Utilizing \code{ggplot2} with custom themes to ensure plots are
  #' consistent. Utlizes \code{autoplot} function for plots.
  #'
  #' @param ts_object time series object used to create plot
  #' @param ts_object_name preferred title of plot
  #' @param color color of the plotting, defaults to "dodgerblue3"
  #' @examples
  #' data(AirPassengers)
  #' air_pass_ts <- as.ts(AirPassengers)
  #' ts_decomp(air_pass_ts)
  #'
  #'
  
  default.color <- color
  
  if (is.ts(ts_object) == TRUE){
    autoplot(stl(ts_object, s.window = "periodic"),
             main = sprintf("Decomposition Plot of %s", ts_object_name),
             ts.colour =  default.color) +
      theme(panel.background = element_rect(fill = "gray98"),
            axis.line.y   = element_line(colour="gray"),
            axis.line.x = element_line(colour="gray"))
  } else {
    warning('Make sure object entered is time-series object!')
  }
}
###########################################################################################



###########################################################################################
# Function: ts_seasonal                                                                  #
# Seasonal Plot                                                                           #
###########################################################################################
# Seasonal Plot
ts_seasonal <- function(ts_object, ts_object_name, color="dodgerblue3"){
  
  #' Plots Seasonal Component for Time Series Object
  #'
  #' Plots \emph{Seasonal} aspect of time series object.
  #' Utilizing \code{ggplot2} with custom themes to ensure plots are
  #' consistent. Utlizes \code{autoplot} function for plots.
  #'
  #' @param ts_object time series object used to create plot
  #' @param ts_object_name preferred title of plot
  #' @param color color of the plottting, defaults to "dodgerblue3"
  #' @examples
  #' data(AirPassengers)
  #'
  #' air_pass_ts <- as.ts(AirPassengers)
  #'
  #' ts_seasonal(air_pass_ts, 'Air Passengers Data Set')
  #' 
  
  default.color <- color
  
  if (is.ts(ts_object) == TRUE){
    ggseasonplot(ts_object, xlab="Year",
                 main=sprintf("Seasonal Plot of %s", ts_object_name),
                 year.labels=TRUE, year.labels.left=TRUE,
                 col=1:20, pch=19) +
      theme(panel.background = element_rect(fill = "gray98"),
            axis.line.y = element_line(colour="gray"),
            axis.line.x = element_line(colour="gray"))
  } else {
    warning('Make sure object entered is time-series object!')
  }
}
###########################################################################################


###########################################################################################
# HERE FOUND AT http://librestats.com/2012/06/11/autoplot-graphical-methods-with-ggplot2/ #
# BY DREW SCHMIDT WITH SLIGHT MODIFICATIONS TO FIT OUR PLOTS                              #
###########################################################################################

ts_forecast_plot <- function(forecast, forc_name, ts_object_name, 
                             color='dodgerblue3', actual_color='olivedrab4', forecast_color='firebrick2',
                             ..., holdout=NaN){
  
  #' Plots Forecasted values for Time Series Models
  #'
  #' Borrowed from Drew Schmidt, but modified to fit our aesthetic appeal
  #' we create a dataframe with all the appropriate sections (i.e. upper and
  #' lower 95% CI bands, forecasted, actual values, the training time series
  #' object, and upper and lower 80% CI), the we create a \code{ggplot2} object that
  #' reflects the forecasted plot
  #'
  #' @param forecast forecasted values created using \code{forecast} function
  #' @param forc_name name of forecasted method included in title
  #' @param color color of the plot, defaults to 'dodgerblue3'
  #' @param actual_color color of the actual time series data
  #' @param forecast_color color of the forecast time series data
  #' @param ts_object_name time series name included in title
  #' @param holdout time series object that contains actual values that can be
  #' compared to the forecasted values
  
  
  summary(forecast)
  
  # check residuals
  checkresiduals(forecast$residuals)
  
  plot(forecast)
  
  #cm <- caret::confusionMatrix(data = forecast, forecast$type)
  #F1_score(cm$table, "lda")
  
}

ts_forecast_plot_custom <- function(forecast, forc_name, ts_object_name, 
                             color='dodgerblue3', actual_color='olivedrab4', forecast_color='firebrick2',
                             ..., holdout=NaN){
  #' Plots Forecasted values for Time Series Models
  #'
  #' Borrowed from Drew Schmidt, but modified to fit our aesthetic appeal
  #' we create a dataframe with all the appropriate sections (i.e. upper and
  #' lower 95% CI bands, forecasted, actual values, the training time series
  #' object, and upper and lower 80% CI), the we create a \code{ggplot2} object that
  #' reflects the forecasted plot
  #'
  #' @param forecast forecasted values created using \code{forecast} function
  #' @param forc_name name of forecasted method included in title
  #' @param color color of the plot, defaults to 'dodgerblue3'
  #' @param actual_color color of the actual time series data
  #' @param forecast_color color of the forecast time series data
  #' @param ts_object_name time series name included in title
  #' @param holdout time series object that contains actual values that can be
  #' compared to the forecasted values
  
  
  summary(forecast)
  
  # check residuals
  checkresiduals(forecast$residuals)
  
  plot(forecast)
  
  # forecast plot colors
  default.color <- color
  actual.color <- actual_color
  forecast.color <- forecast_color 
  eighty.conf.color <- "orange" 
  ninety_five.conf.color <- "yellow"
  
  # data wrangling
  time <- attr(forecast$x, "tsp")
  time <- seq(time[1], attr(forecast$mean, "tsp")[2], by=1/time[3])
  lenx <- length(forecast$x)
  lenmn <- length(forecast$mean)
  
  df <- data.frame(time=time,
                   x=c(forecast$x, forecast$mean),
                   x2=c(forecast$x, rep(NA, lenmn-length(holdout)), holdout),
                   forecast=c(rep(NA, lenx), forecast$mean),
                   low1=c(rep(NA, lenx), forecast$lower[, 1]),
                   upp1=c(rep(NA, lenx), forecast$upper[, 1]),
                   low2=c(rep(NA, lenx), forecast$lower[, 2]),
                   upp2=c(rep(NA, lenx), forecast$upper[, 2]),
                   holdout=c(rep(NA, lenx+lenmn-length(holdout)), holdout)
  )
  
  ggplot(df, aes(time, x)) +
    geom_ribbon(aes(ymin=low2, ymax=upp2), fill=eighty.conf.color, na.rm=TRUE) +
    geom_ribbon(aes(ymin=low1, ymax=upp1), fill=ninety_five.conf.color, na.rm=TRUE) +
    geom_line(data=df, aes(time, x2), color="red")+
    geom_line(colour = default.color, size = 1) +
    
    geom_line(data=df[!is.na(df$forecast), ], aes(time, forecast), color=forecast.color, na.rm=TRUE) +
    geom_line(data=df[!is.na(df$holdout), ], aes(time, holdout), color=actual.color, na.rm=TRUE) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
    scale_y_continuous("")  +
    theme(axis.text.x = element_text(angle = 35, hjust = 1),
          panel.background = element_rect(fill = "gray98"),
          axis.line.y   = element_line(colour="gray"),
          axis.line.x = element_line(colour="gray")) +
    labs(x = "Months", y = "log(Closing Values)") +
    ggtitle(sprintf('%s Forecast Plot of %s', forc_name, ts_object_name))
}

###########################################################################################



###########################################################################################
# Function: ts_dashboard_plot                                                            #
# Method for Shiny Dashboard                                                             #
###########################################################################################
ts_dashboard_plot <- function(object, ts_object_name, color='dodgerblue3', gof.lag = 10,
                              conf.int = TRUE,
                              conf.int.colour = '#0000FF', conf.int.linetype = 'dashed',
                              conf.int.fill = NULL, conf.int.alpha = 0.3,
                              ad.colour = '#888888', ad.linetype = 'dashed', ad.size = .2,
                              nrow = NULL, ncol = 1, ...) {
  
  
  default.color <- color
  
  rs <- stats::residuals(object)
  if (is.null(rs)) {
    rs <- object$residuals
  }
  if (is.null(rs)) {
    rs <- object$resid
  }
  
  p.std <- ggplot2::autoplot(rs, na.action = stats::na.pass,
                             ts.colour = default.color, size = 1.05) +
    ggplot2::geom_hline(yintercept = 0,
                        linetype = ad.linetype, size = ad.size,
                        colour = ad.colour) +
    labs(subtitle = '') +
    ggplot2::ggtitle(sprintf("Residual Diagnostics for %s \nNon-Standardized Residuals",
                             ts_object_name))
  
  acfobj <- stats::acf(rs, plot = FALSE, na.action = stats::na.pass)
  p.acf <- autoplot(acfobj, conf.int = conf.int,
                    conf.int.colour = conf.int.colour,
                    conf.int.linetype = conf.int.linetype,
                    conf.int.fill = conf.int.fill,
                    conf.int.alpha = conf.int.alpha,
                    colour = default.color, size = 1.25)
  p.acf <- p.acf + ggplot2::ggtitle('ACF of Residuals')
  
  nlag <- gof.lag
  pval <- numeric(nlag)
  for (i in 1L:nlag) pval[i] <- stats::Box.test(rs, i, type = "Ljung-Box")$p.value
  lb.df <- data.frame(Lag = 1L:nlag, `p value` = pval,
                      lower = -0.05, upper = 0.05)
  # Unnable to create column with space by above expression
  colnames(lb.df) <- c('Lag', 'p value', 'lower', 'upper')
  p.lb <- ggplot2::ggplot(data = lb.df, mapping = ggplot2::aes_string(x = 'Lag')) +
    ggplot2::geom_point(mapping = ggplot2::aes_string(y = '`p value`'), na.rm = TRUE,
                        colour = default.color) +
    ggplot2::scale_y_continuous(limits=c(-0.1, 1)) +
    ggplot2::ggtitle('p values for Ljung-Box statistic')
  
  p.lb <- ggfortify:::plot_confint(p = p.lb, data = lb.df, conf.int = conf.int,
                                   conf.int.colour = conf.int.colour,
                                   conf.int.linetype = conf.int.linetype,
                                   conf.int.fill = conf.int.fill, conf.int.alpha = conf.int.alpha)
  
  if (is.null(ncol)) { ncol <- 0 }
  if (is.null(nrow)) { nrow <- 0 }
  new('ggmultiplot', plots = list(p.std, p.acf, p.lb), nrow = nrow, ncol = ncol)
}

###########################################################################################



###########################################################################################
# Function: ts_forecast_arima                                                            #
# forecast arima                                                                         #
###########################################################################################

ts_forecast_arima <- function(train, test, predict_duration, title, color="dodgerblue3"){
  
  #' Forecasts arima model, returns accuracy
  #'
  #'
  #' @param train time series object with training dataset
  #' @param test time series object with testing dataset
  #' @param predict_duration the prediction duration (ex: months)
  #' @param title tile of the plotting
  #' @param color color of the plottting, defaults to "dodgerblue3"
  #' @examples
  #' data(AirPassengers)
  #'
  #' air_pass_ts <- as.ts(AirPassengers)
  #'
  #' ts_forecast_arima(air_pass_train, air_pass_test, 90, 'Air Passengers Data Set')
  #' 
  
  if (is.ts(train) == FALSE || is.ts(test)  == FALSE){
    warning('Make sure object entered is time-series object!')
  }
  else {
    model_train <- auto.arima(train)
    train_forecast <- forecast(model_train,h=predict_duration)
   
    #debug(fit_auto_arima)
    ts_forecast_plot(train_forecast, "ARIMA", title, color=color,
                holdout = fit_test)
    
    ts_dashboard_plot(train_forecast$model, ts_object_name = "ARIMA Model") +
      theme(panel.background = element_rect(fill = "gray98"),
            panel.grid.minor = element_blank(),
            axis.line.y = element_line(colour="gray"),
            axis.line.x = element_line(colour="gray"))
    
    
    #model_test <- Arima(test, model= model_train)
    model_test <- auto.arima(test)
    test_forecast <- forecast(model_test,h=predict_duration)
    accuracy <- accuracy(model_test)
    debug(accuracy)
    
    data <- list(test_forecast, accuracy)
    names(data) <- c("forecast" , "accuracy")
    
    return(data)
  }
}
###########################################################################################


###########################################################################################
# Function: ts_forecast_exp_smoothing                                                     #
# forecast expnential smoothing model                                                     #
###########################################################################################

ts_forecast_exp_smoothing <- function(train, test, predict_duration, title, color="dodgerblue3"){
  
  #' Forecasts exponential smoothing model, returns accuracy
  #'
  #'
  #' @param train time series object with training dataset
  #' @param test time series object with testing dataset
  #' @param predict_duration prediction duration (ex: months)
  #' @param title tile of the plotting
  #' @param color color of the plottting, defaults to "dodgerblue3"
  #' @examples
  #' data(AirPassengers)
  #'
  #' air_pass_ts <- as.ts(AirPassengers)
  #'
  #' ts_forecast_exp_smoothing(air_pass_train, air_pass_test, 90, 'Air Passengers Data Set')
  #' 
  
  if (is.ts(train) == FALSE || is.ts(test)  == FALSE){
    warning('Make sure object entered is time-series object!')
  }
  else {
    
    model_train <- ets(train)
    train_forecast <- forecast(model_train, h = predict_duration)
    
    ts_forecast_plot(train_forecast, "Exponential Smoothing", title, color = color, holdout = test)
    
    ts_dashboard_plot(train_forecast$model, ts_object_name = "Exponential Smoothing Model") +
      theme(panel.background = element_rect(fill = "gray98"),
            panel.grid.minor = element_blank(),
            axis.line.y = element_line(colour="gray"),
            axis.line.x = element_line(colour="gray"))
    
    
    model_test <- ets(test, model = model_train)
    test_forecast <- forecast(model_test,h=predict_duration)
    accuracy <- accuracy(model_test)
   
    debug(accuracy)
    
    data <- list(test_forecast, accuracy)
    names(data) <- c("forecast" , "accuracy")
    
    return(data)
  }
}
###########################################################################################


###########################################################################################
# Function: ts_forecast_box_cox                                                          #
# forecast Box-Cox model                                                                 #
###########################################################################################

ts_forecast_box_cox <- function(train, test, predict_duration, title, color="dodgerblue3"){
  
  #' Forecasts Box-Cox model
  #'
  #'
  #' @param train time series object with training dataset
  #' @param test time series object with testing dataset
  #' @param predict_duration prediction duration (ex: months)
  #' @param title tile of the plotting
  #' @param color color of the plottting, defaults to "dodgerblue3"
  #' @examples
  #' data(AirPassengers)
  #'
  #' air_pass_ts <- as.ts(AirPassengers)
  #'
  #' ts_forecast_box_cox(air_pass_train, air_pass_test, 90, 'Air Passengers Data Set')
  #' 
  
  if (is.ts(train) == FALSE || is.ts(test)  == FALSE){
    warning('Make sure object entered is time-series object!')
  }
  else {
    lambda <- BoxCox.lambda(train)
    model_train <- ar(BoxCox(train,lambda))
    fit_bc <- forecast(model_train,h=predict_duration,lambda=lambda)
    
    ts_forecast_plot(fit_bc, "Box-Cox Transformation", title, color=color, holdout = test)
    
    ts_dashboard_plot(fit_bc$model, ts_object_name = "Box-Cox Transformation Model") +
      theme(panel.background = element_rect(fill = "gray98"),
            panel.grid.minor = element_blank(),
            axis.line.y = element_line(colour="gray"),
            axis.line.x = element_line(colour="gray"))
  
    #model_test <- ar(BoxCox(test, BoxCox.lambda(test)))
    accuracy <- accuracy(model_train)
    print(accuracy)
    
    #accuracy <- round(accuracy[1,2], 3)
    
    debug(accuracy)
    return(accuracy)
    
  }
}
###########################################################################################


###########################################################################################
# Function: ts_forecast_naive                                                           #
# forecast Naive Model                                                                   #
###########################################################################################

ts_forecast_naive <- function(train, test, predict_duration, title, color="dodgerblue3"){
  
  #' Forecasts Seasonal Naive model, returns accuracy
  #'
  #'
  #' @param train time series object with training dataset
  #' @param test time series object with testing dataset
  #' @param predict_duration prediction duration (ex: months)
  #' @param title tile of the plotting
  #'  @param color color of the plottting, defaults to "dodgerblue3"
  #'
  #' @examples
  #' data(AirPassengers)
  #'
  #' air_pass_ts <- as.ts(AirPassengers)
  #'
  #' ts_forecast_naive(air_pass_train, air_pass_test, 90, 'Air Passengers Data Set')
  #' 
  
  if (is.ts(train) == FALSE || is.ts(test) == FALSE){
    warning('Make sure object entered is time-series object!')
  }
  else {
    model_train <- naive(train, h = predict_duration)
    
    ts_forecast_plot(model_train, "Naive", title, color, holdout = test)
    
    ts_dashboard_plot(model_train, ts_object_name = "Naive Forecast Model") +
      theme(panel.background = element_rect(fill = "gray98"),
            panel.grid.minor = element_blank(),
            axis.line.y = element_line(colour="gray"),
            axis.line.x = element_line(colour="gray"))
    
    model_test <- naive(test, h = predict_duration)
    test_forecast <- forecast(model_test,h=predict_duration)
    accuracy <- accuracy(model_test)
    
    debug(accuracy)
    
    data <- list(test_forecast, accuracy)
    names(data) <- c("forecast" , "accuracy")
    
    return(data)
  }
}
###########################################################################################


###########################################################################################
# Function: ts_forecast_seasonal_naive                                                  #
# forecast Seasonal Naive Model                                                          #
###########################################################################################

ts_forecast_seasonal_naive <- function(train, test, predict_duration, title, color="dodgerblue3"){
  
  #' Forecasts Seasonal Naive model, returns accuracy
  #'
  #'
  #' @param train time series object with training dataset
  #' @param test time series object with testing dataset
  #' @param predict_duration prediction duration (ex: months)
  #' @param title tile of the plotting
  #' @param color color of the plottting, defaults to "dodgerblue3"
  #' @examples
  #' data(AirPassengers)
  #'
  #' air_pass_ts <- as.ts(AirPassengers)
  #'
  #' ts_forecast_seasonal_naive(air_pass_train, air_pass_test,90, 'Air Passengers Data Set')
  #' 
  
  if (is.ts(train) == FALSE || is.ts(test) == FALSE){
    warning('Make sure object entered is time-series object!')
  }
  else {
    model_train <- snaive(train, h = predict_duration)
    
    ts_forecast_plot(model_train, "Seasonal Naive Forecast", title, color=color, holdout = test)
    
    ts_dashboard_plot(model_train, ts_object_name = "Seasonal Naive Forecast Model") +
      theme(panel.background = element_rect(fill = "gray98"),
            panel.grid.minor = element_blank(),
            axis.line.y = element_line(colour="gray"),
            axis.line.x = element_line(colour="gray"))
    
    
    model_test <- snaive(test, h = predict_duration)
    test_forecast <- forecast(model_test,h=predict_duration)
    accuracy <- accuracy(model_test)
    
    debug(accuracy)
    
    data <- list(test_forecast, accuracy)
    names(data) <- c("forecast" , "accuracy")
    
    return(data)
    
  }
}
###########################################################################################


###########################################################################################
# Function: ts_forecast_neural_net                                                       #
# forecast neural network model                                                           #
###########################################################################################

ts_forecast_neural_net <- function(train, test, predict_duration, title, color="dodgerblue3"){
  
  #' Forecasts neural network model, returns accuracy
  #'
  #'
  #' @param train time series object with training dataset
  #' @param test time series object with testing dataset
  #' @param predict_duration prediction duration (ex: months)
  #' @param title tile of the plotting
  #' @param color color of the plottting, defaults to "dodgerblue3"
  #' @examples
  #' data(AirPassengers)
  #'
  #' air_pass_ts <- as.ts(AirPassengers)
  #'
  #' ts_forecast_neural_net(air_pass_train, air_pass_test, 90, 'Air Passengers Data Set')
  #' 
  
  if (is.ts(train) == FALSE || is.ts(test)  == FALSE){
    warning('Make sure object entered is time-series object!')
  }
  else {
    lambda <- BoxCox.lambda(train)
    model_train <- nnetar(train, lambda = lambda) # Using BC lambda
    fit_net <- forecast(model_train, h = predict_duration, PI = TRUE)
    
    ts_forecast_plot(fit_net, "Neural Network", color = color, title, holdout = test)
    
    ts_dashboard_plot(fit_net, ts_object_name = "Neural Networks Model") +
      theme(panel.background = element_rect(fill = "gray98"),
            panel.grid.minor = element_blank(),
            axis.line.y = element_line(colour="gray"),
            axis.line.x = element_line(colour="gray"))
    
    
    
    model_test <- nnetar(test, model=model_train)
    test_forecast <- forecast(model_test,h=predict_duration)
    accuracy <- accuracy(model_test)
    
    debug(accuracy)
    
    data <- list(test_forecast, accuracy)
    names(data) <- c("forecast" , "accuracy")
    
    return(data)
    
  }
}
###########################################################################################


debug <- function(x) {
  print(x)
  flush.console()
}
